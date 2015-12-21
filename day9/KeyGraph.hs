module KeyGraph (
    KeyGraph,
    nodes,
    neighbors,
    edge,
    edgesFrom,
    fromListUndirected,
    deleteUndirected,
) where

import qualified Data.Map as M

type KeyGraph nodetype weighttype = M.Map nodetype [(nodetype, weighttype)]

nodes :: KeyGraph n e -> [n]
nodes = M.keys

neighbors :: Ord n => KeyGraph n e -> n -> Maybe [n]
neighbors kg node = map fst <$> M.lookup node kg

edge :: Ord n => KeyGraph n e -> n -> n -> Maybe e
edge kg from to = M.lookup from kg >>= lookup to

edgesFrom :: Ord n => KeyGraph n e -> n -> Maybe [(n, e)]
edgesFrom = flip M.lookup

fromListUndirected :: Ord n => [(n, n, e)] -> KeyGraph n e
fromListUndirected = M.fromListWith (flip (++)) . concatMap makeEdges
    where makeEdges (n1, n2, e) = [(n1, [(n2, e)]), (n2, [(n1, e)])]

deleteUndirected :: Ord n => KeyGraph n e -> n -> KeyGraph n e
deleteUndirected kg node = M.map remove_refs $ M.delete node kg
    where remove_refs = filter ((/= node) . fst)
