{-# LANGUAGE RecordWildCards, ConstraintKinds, FlexibleContexts #-}
module Allocator (alloc, fakeAssembly) where
import Control.Monad.State
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Language
import ControlFlowGraph
import Analysis

data AllocState = AllocState { freshCount :: Int
                             , stackDepth :: Int
                             } deriving Show
type AllocM m = MonadState AllocState m

freshTemp :: AllocM m => m Temp
freshTemp = do
  modify (\a -> a{freshCount = freshCount a + 1})
  freshCount <$> get

freshAddr :: AllocM m => m Operand
freshAddr = do
  modify (\a -> a{stackDepth = stackDepth a + 1})
  InStack . stackDepth <$> get

runAllocM :: Temp -> State AllocState a -> a
runAllocM t m = evalState m (AllocState t 0)

-- | Rewrite a CFG so things are loaded into a local temp and stored
-- upon each use.
rewriteSpill :: AllocM m => Cfg -> Temp -> m Cfg
rewriteSpill (Cfg blocks graph) t = do
  addr <- freshAddr
  blocks' <- traverse (fmap concat . mapM (spill addr)) blocks
  return $ Cfg blocks' graph
  where spill a i
          | t `S.member` S.union (gen i) (kill i) = do
              t' <- freshTemp
              let is =
                    [ if S.member t (gen i) then [Load t' a] else []
                    , [replace t' t i]
                    , if S.member t (kill i) then [Store a (Temp t')] else []
                    ]
                in return (concat is)
          | otherwise = return [i]

degree :: Temp -> InterferenceGraph -> Int
degree t (IGraph g) = S.size $ S.filter ((== t) . fst) g

data CoalesceState = CoalesceState { toSpill :: [Temp]
                                   , toSimplify :: [Temp]
                                   , handled :: [Temp]
                                   , interferGraph :: Set (Temp, Temp)
                                   , degrees :: Map Temp Int
                                   } deriving Show

buildLists :: Cfg -> InterferenceGraph -> Int -> CoalesceState
buildLists cfg ig@(IGraph g) kColors =
  CoalesceState (M.keys highDegree) (M.keys lowDegree) [] g dGraph
  where (highDegree, lowDegree) = M.partition (>= kColors) dGraph
        temps = S.toList (allTemps cfg)
        dGraph = M.fromList [(t, degree t ig) | t <- temps]

assignColors :: InterferenceGraph -> Int -> [Temp] -> (Map Temp Int, Set Temp)
assignColors (IGraph graph) kColors = go (M.empty, S.empty)
  where allColors = S.fromList [1 .. kColors]
        go s [] = s
        go (colors, spilled) (t : temps) =
          let neighbors = S.map snd $ S.filter ((== t) . fst) graph
              usedColors = foldMap (toSet . flip M.lookup colors) neighbors
              goodColors = allColors S.\\ usedColors
          in if S.null goodColors
             then go (colors, S.insert t spilled) temps
             else go (M.insert t (S.findMin goodColors) colors, spilled) temps
        toSet = foldMap S.singleton

doSimplify :: Int -> CoalesceState -> CoalesceState
doSimplify _ c@CoalesceState{toSimplify = []} = c
doSimplify kColors (CoalesceState toSpill (t : toSimplify) handled g degrees) =
  CoalesceState toSpill' toSimplify' (t : handled) igraph' degrees'
  where neighbors = S.map snd $ S.filter ((== t) . fst) g
        degrees' = foldl (flip $ M.adjust pred) degrees neighbors
        saved = S.filter ((== kColors) . (degrees M.!)) neighbors
        toSpill' = filter (not . (`S.member` saved)) toSpill
        toSimplify' = S.toList saved ++ toSimplify
        igraph' = S.filter (\(a, b) -> a == t || b == t) g

registerAlloc :: AllocM m => Int -> Cfg -> m (Cfg, Map Temp Int)
registerAlloc kColors cfg =
  let tempStack = handled $ coalesce (buildLists cfg igraph kColors)
      (coloring, spills) = assignColors igraph kColors tempStack
  in if S.null spills
     then return (cfg, coloring)
     else foldM rewriteSpill cfg spills >>= registerAlloc kColors
  where igraph = interfers cfg
        coalesce c@(CoalesceState {..}) =
          case (toSimplify, toSpill) of
           (_ : _, _) -> coalesce (doSimplify kColors c)
           ([], t : rest) -> coalesce (c {toSimplify = [t], toSpill = rest})
           ([], []) -> c

-- | Allocate @n@ registers for the program specified by supplied "Cfg".
alloc :: Int -> Cfg -> (Cfg, Map Temp Int)
alloc kColors cfg =
  runAllocM (maximum $ allTemps cfg) (registerAlloc kColors cfg)

-- | Runs 'alloc' and automatically rewrites the resulting "Cfg" so
-- that the temporaries are replaced with the actual registers they're
-- scheduled for.
--
-- Register allocations correctness property is that
-- > eval = eval . fakeAssembly n
fakeAssembly :: Int -> Cfg -> Cfg
fakeAssembly kColors cfg = c'{cfgBlocks = map applySchedule <$> cfgBlocks c'}
  where (c', schedule) = alloc kColors cfg
        replaceAll = appEndo . M.foldMapWithKey (\t r -> Endo $ replace r t)
        applySchedule = replaceAll (negate <$> M.insert 0 0 schedule)
