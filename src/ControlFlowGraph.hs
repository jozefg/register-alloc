module ControlFlowGraph
  ( CfgNode(..)
  , Cfg(..)
  , InstrLike (..)
  , buildCfg
  , allTemps ) where
import Control.Applicative
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Language

toSet :: (Foldable f, Ord a) => f a -> Set a
toSet = foldMap S.singleton

class InstrLike a where
  gen :: a -> Set Temp
  kill :: a -> Set Temp

instance InstrLike Code where
  kill (Asgn t _ _ _) = S.singleton t
  kill (Copy t _) = S.singleton t
  kill (Load t _) = S.singleton t
  kill _ = S.empty

  gen (Asgn _ l _ r) = foldMap toSet [operand2temp l, operand2temp r]
  gen (Copy _ o) = toSet (operand2temp o)
  gen (Jmp _) = S.empty
  gen (CJmp l _ r _ _) = foldMap toSet [operand2temp l, operand2temp r]
  gen (Load _ o) = toSet (operand2temp o)
  gen (Store to o) = foldMap toSet [operand2temp to, operand2temp o]
  gen (Label _) = S.empty

instance InstrLike a => InstrLike [a] where
  gen xs = foldr (\n p -> (p S.\\ kill n) `S.union` gen n) S.empty xs
  kill xs = foldMap kill xs

-- | Split code into basic blocks. A basic block is a chunk of
-- expressions which do not loop, jmp anywhere, and are only ever
-- executed together in a straightline manner.
basicBlocks :: [Code] -> [[Code]]
basicBlocks = go
  where go [] = []
        go (c : cs) =
          case span (not . liftA2 (||) startBB endBB) cs of
           (bb, []) -> [c : bb]
           (bb, end : rest) | startBB end -> (c : bb) : go (end : rest)
                            | otherwise -> (c : bb ++ [end]) : go rest
        startBB (Label _) = True
        startBB _ = False
        endBB (Jmp _) = True
        endBB (CJmp _ _ _ _ _) = True
        endBB _ = False

-- | Add a label to the start of every basic block, this simplifies
-- the representation of the control flow graph which wants a name for
-- each basic block.
labelBlocks :: [[Code]] -> [[Code]]
labelBlocks = go 0
  where go _ [] = []
        go i (bb@(Label _ : _) : rest) = bb : go i rest
        go i (bb : rest) = (Label (Fresh i) : bb) : go (i + 1) rest

-- | Tags for nodes in the control flow graph. The distinguished start
-- and stop tags are just for consistency.
data CfgNode = Start | End | CfgLabel Label
             deriving (Eq, Show, Ord)

-- | The representation of control flow graphs, the graph bit is just
-- a big set of edges and code labels are mapped to the appropriate
-- basic blocks in a separate map.
data Cfg = Cfg { cfgBlocks :: Map Label [Code]
               , cfg       :: Set (CfgNode, CfgNode)
               } deriving (Eq, Show)

-- | Convert basic blocks to our naive repr of a control flow graph
mkCfg :: [[Code]] -> Cfg
mkCfg [] = Cfg M.empty S.empty
mkCfg bbs@(firstBB : _) =
  Cfg (M.fromList (zip (map lbl bbs) bbs))
      (S.fromList ((Start, CfgLabel $ lbl firstBB) : cfgEdges bbs))
  where lbl :: [Code] -> Label
        lbl (Label i : _) = i
        lbl _ = error "lbl: Not called on an annotated basic block"

        flowsTo l1 (Jmp l2) _ = [(CfgLabel l1, CfgLabel l2)]
        flowsTo l1 (CJmp _ _ _ l2 l3) _ =
          [(CfgLabel l1, CfgLabel l2), (CfgLabel l1, CfgLabel l3)]
        flowsTo l1 _ next = [(CfgLabel l1, next)]

        cfgEdges [] = []
        cfgEdges [bb] = flowsTo (lbl bb) (last bb) End
        cfgEdges (bb : next :rest) =
          flowsTo (lbl bb) (last bb) (CfgLabel $ lbl next)
          ++ cfgEdges (next : rest)

-- | Construct a control flow graph for a program. It is assumed that
-- the program doesn't make use of 'Fresh' labels.
--
-- TODO: Remove this assumption
buildCfg :: [Code] -> Cfg
buildCfg = mkCfg . labelBlocks . basicBlocks

-- | A small but useful utility to snarf up all the temporaries in "Cfg".
allTemps :: Cfg -> Set Temp
allTemps = foldMap (\t -> gen t `S.union` kill t) . Compose . cfgBlocks
