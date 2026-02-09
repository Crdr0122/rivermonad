module Utils.BiMap (BiMap, empty, insert, lookupA, lookupBs, delete, move) where

import Data.Map.Strict qualified as M
import Data.Sequence qualified as S

data BiMap a b = BiMap
  { aToBs :: M.Map a (S.Seq b)
  , bToA :: M.Map b a
  }

empty :: BiMap a b
empty = BiMap M.empty M.empty

insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert a b bimap@(BiMap ab ba)
  | M.member b ba = bimap
  | otherwise =
      let ab' = M.insertWith (S.><) a (S.singleton b) ab
          ba' = M.insert b a ba
       in BiMap ab' ba'

lookupA :: (Ord b) => b -> BiMap a b -> Maybe a
lookupA b = M.lookup b . bToA

lookupBs :: (Ord a) => a -> BiMap a b -> S.Seq b
lookupBs a = M.findWithDefault S.empty a . aToBs

delete :: (Ord a, Ord b) => b -> BiMap a b -> BiMap a b
delete b bimap@(BiMap ab ba)
  | not (M.member b ba) = bimap
  | otherwise =
      let ba' = M.delete b ba
          a = ba M.! b
          seqB = ab M.! a
          newSeqB = S.filter (/= b) seqB
          ab' = M.insert a newSeqB ab
       in BiMap ab' ba'

move :: (Ord a, Ord b) => b -> a -> BiMap a b -> BiMap a b
move b newA bm =
  let bm1 = delete b bm
   in insert newA b bm1
