{-# LANGUAGE FlexibleContexts #-}
module Analysis
       ( InterferenceGraph(..)
       , interfers) where
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Language
import ControlFlowGraph

data Live = Live { liveIn :: Set Temp
                 , liveOut :: Set Temp
                 } deriving (Eq, Show)

liveness :: Cfg -> Map CfgNode Live
liveness (Cfg blocks graph) =
  coalesce (S.fromList labels) (M.fromList initial)
  where labels = Start : End : map CfgLabel (M.keys blocks)
        initial = map (\k -> (k, Live S.empty S.empty)) labels

        getCode (CfgLabel l) = blocks M.! l
        getCode _ = []

        work l liveMap =
          let code = getCode l
              children = S.map snd . S.filter ((== l). fst) $ graph
              parents = S.map fst . S.filter ((== l). snd) $ graph
              newOut = foldMap (liveIn . (liveMap M.!)) children
              newIn = gen code `S.union` (newOut S.\\ kill code)
              new = Live newIn newOut
              todo = if new == liveMap M.! l then S.empty else parents
          in (todo, M.insert l new liveMap)
        coalesce workList liveMap =
          case S.minView workList of
           Nothing -> liveMap
           Just (l, workList') ->
             let (redoList, newMap) = work l liveMap
             in coalesce (S.union redoList workList') newMap

data InterferenceGraph = IGraph (Set (Temp, Temp)) deriving Show

-- | Build an interference graph using the results of liveness
-- analysis
interfers :: Cfg -> InterferenceGraph
interfers cfg@(Cfg blocks _) =
  IGraph (prepareFinalGraph $ M.foldMapWithKey go liveMap)
  where liveMap = liveness cfg
        go Start _ = S.empty
        go End _ = S.empty
        go (CfgLabel l) (Live _ lout) =
          let code = blocks M.! l
          in execWriter $ foldM_ goInstr lout (reverse code)

        goInstr lout i =
          let lin = (lout S.\\ kill i) `S.union` gen i
              edges = S.fromList [(u, v) |
                                   u <- S.toList (kill i)
                                 , v <- S.toList lout]
          in tell edges >> return lin

        prepareFinalGraph edges =
          S.filter (uncurry (/=)) $
            edges `S.union` (S.map (\(a, b) -> (b, a)) edges)
