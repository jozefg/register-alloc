{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Interpreter (eval) where
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Language
import ControlFlowGraph

data Heap = Heap { tempValues :: Map Temp Int
                 , stackValues :: Map Int Int
                 } deriving Show

-- Execute an instruction and return either a label to jump to or
-- nothing if you should fall through to the next instruction.
simulate :: MonadState Heap m => Code -> m (Maybe Label)
simulate c = case c of
  Label _ -> return Nothing
  Asgn t l o r -> do
    denoteOp o <$> evalOper l <*> evalOper r >>= storeTemp t
    return Nothing
  Copy t o -> evalOper o >>= storeTemp t >> return Nothing
  Jmp l -> return (Just l)
  CJmp l o r l1 l2 -> do
    b <- denoteRel o <$> evalOper l <*> evalOper r
    return . Just $ if b then l1 else l2
  Load t o -> evalAddr o >>= load >>= storeTemp t >> return Nothing
  Store to v -> do
    join $ storeStack <$> evalAddr to <*> evalOper v
    return Nothing
  where evalOper (Temp t) = (M.! t) . tempValues <$> get
        evalOper (Lit i) = return i
        evalOper (InStack _) = error "Cannot treat InStack as int right now"
        evalAddr (Temp t) = (M.! t) . tempValues <$> get
        evalAddr (Lit _) = error "Don't have arbitrary memory yet"
        evalAddr (InStack i) = return i

        storeTemp t i =
          modify $ \h -> h{tempValues = M.insert t i (tempValues h) }
        storeStack t i =
          modify $ \h -> h{stackValues = M.insert t i (stackValues h) }
        load s = (M.! s) . stackValues <$> get
        denoteOp = \case Add -> (+); Sub -> (-); Mul -> (*); Div -> div
        denoteRel = \case Eq -> (==); Neq -> (/=); Leq -> (<=); Geq -> (>=)

simulateCfg :: (MonadState Heap m) => Cfg -> m ()
simulateCfg (Cfg blocks g) = execute firstBlock
  where firstBlock = snd . head . S.toList $ S.filter ((Start ==) . fst) g

        execute Start = fail "execute: received start"
        execute (CfgLabel l) = simBlock l >>= execute . chooseNext l
        execute End = return ()

        chooseNext _ (Just l) = CfgLabel l
        chooseNext l Nothing =
          case S.toList $ S.filter ((CfgLabel l ==) . fst) g of
           [(_, l')] -> l'
           _ -> error "Ambigious next choice"

        simBlock = fmap last . mapM simulate . (blocks M.!)

-- | Run a program and compute the value stored in Temp 0
eval :: Cfg -> Int
eval = (M.! 0)
       . tempValues
       . flip execState (Heap M.empty M.empty)
       . simulateCfg
