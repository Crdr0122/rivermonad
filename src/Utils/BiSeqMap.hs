module Utils.BiSeqMap (BiSeqMap, empty, insert, lookupA, lookupBs, delete, move, insertSeq, insertList, insertByIndex) where

import Data.Foldable (foldr')
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S

data BiSeqMap a b = BiSeqMap
  { aToBs :: M.Map a (S.Seq b)
  , bToA :: M.Map b a
  }

empty :: BiSeqMap a b
empty = BiSeqMap M.empty M.empty

insert :: (Ord a, Ord b) => a -> b -> BiSeqMap a b -> BiSeqMap a b
insert a b bimap@(BiSeqMap ab ba)
  | M.member b ba = bimap
  | otherwise =
      let ab' = M.insertWith (S.><) a (S.singleton b) ab
          ba' = M.insert b a ba
       in BiSeqMap ab' ba'

insertByIndex :: (Ord a, Ord b) => a -> b -> Int -> BiSeqMap a b -> BiSeqMap a b
insertByIndex a b index bimap@(BiSeqMap ab ba)
  | M.member b ba = bimap
  | otherwise =
      let ab' =
            M.alter
              ( \case
                  Nothing -> Just $ S.singleton b
                  Just s -> Just $ S.insertAt index b s
              )
              a
              ab
          ba' = M.insert b a ba
       in BiSeqMap ab' ba'

insertSeq :: (Ord a, Ord b) => a -> S.Seq b -> BiSeqMap a b -> BiSeqMap a b
insertSeq a bs (BiSeqMap ab ba) = BiSeqMap ab' ba
 where
  ab' = M.insert a bs ab

insertList :: (Ord a, Ord b) => a -> [b] -> BiSeqMap a b -> BiSeqMap a b
insertList a bs bm = res
 where
  res = foldr' (\newW oldMap -> insert a newW oldMap) bm (reverse bs)

lookupA :: (Ord b) => b -> BiSeqMap a b -> Maybe a
lookupA b = M.lookup b . bToA

lookupBs :: (Ord a) => a -> BiSeqMap a b -> S.Seq b
lookupBs a = M.findWithDefault S.empty a . aToBs

delete :: (Ord a, Ord b) => b -> BiSeqMap a b -> BiSeqMap a b
delete b bimap@(BiSeqMap ab ba)
  | not (M.member b ba) = bimap
  | otherwise =
      let ba' = M.delete b ba
          a = ba M.! b
          seqB = ab M.! a
          newSeqB = S.filter (/= b) seqB
          ab' = M.insert a newSeqB ab
       in BiSeqMap ab' ba'

move :: (Ord a, Ord b) => b -> a -> BiSeqMap a b -> BiSeqMap a b
move b newA bm =
  let bm1 = delete b bm
   in insert newA b bm1
