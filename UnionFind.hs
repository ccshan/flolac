{-# OPTIONS -W #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck (quickCheckAll)
import qualified Data.Map as M

type    State = M.Map Key Node
newtype Key   = Key Int                   deriving (Eq, Ord, Show)
type    Info  = String
type    Rank  = Int
data    Node  = Root Rank Info | Link Key deriving (Eq, Show)

testState :: State
testState = M.fromList
  [ (Key 100, Root 0 "A")
  , (Key 101, Link (Key 104))
  , (Key 102, Root 1 "C")
  , (Key 103, Link (Key 102))
  , (Key 104, Root 1 "E")
  , (Key 105, Root 0 "F") ]

testState' :: State
testState' = M.fromList
  [ (Key 100, Root 0 "A")
  , (Key 101, Link (Key 104))
  , (Key 102, Link (Key 104))
  , (Key 103, Link (Key 102))
  , (Key 104, Root 2 "E")
  , (Key 105, Link (Key 108))
  , (Key 106, Link (Key 108))
  , (Key 107, Link (Key 106))
  , (Key 108, Root 2 "I") ]

fresh :: Info -> State -> (Key, State)
-- ^Add a new set containing only the new element
prop_fresh0 = fresh "A" M.empty   == (Key 100, M.fromList [(Key 100, Root 0 "A")])
prop_fresh6 = fresh "G" testState == (Key 106, M.insert (Key 106) (Root 0 "G") testState)

find :: Key -> State -> (Key, Rank, Info, State)
-- ^Find the representative of the set containing the given element
prop_findA = find (Key 100) testState  == (Key 100, 0, "A", testState)
prop_findE = and  [ find k testState   == (Key 104, 1, "E", testState) | k <- [Key 101, Key 104] ]
prop_findC = and  [ find k testState   == (Key 102, 1, "C", testState) | k <- [Key 102, Key 103] ]
prop_findF = find (Key 105) testState  == (Key 105, 0, "F", testState)
prop_find' = find (Key 103) testState' == (Key 104, 2, "E", M.insert (Key 103) (Link (Key 104))
                                                                     testState')

union :: Key -> Key -> State -> State
-- ^Merge the two sets containing the two given elements
prop_unionC  = and [ union k1' k2' testState == M.insert k1 (Link (Key 102)) testState
                   | k1        <- [Key 100, Key 105]
                   , k2        <- [Key 102, Key 103]
                   , (k1',k2') <- [(k1, k2), (k2, k1)] ]
prop_unionAF = and [ elem (union k1 k2 testState)
                          [ M.insert (Key 100) (Link (Key 105))
                               (M.insert (Key 105) (Root 1 "F") testState)
                          , M.insert (Key 105) (Link (Key 100))
                               (M.insert (Key 100) (Root 1 "A") testState) ]
                   | (k1,k2) <- [(Key 100, Key 105), (Key 105, Key 100)] ]
prop_unionCE = and [ elem (union k1' k2' testState)
                          [ M.insert (Key 102) (Link (Key 104))
                               (M.insert (Key 104) (Root 2 "E") testState)
                          , M.insert (Key 104) (Link (Key 102))
                               (M.insert (Key 102) (Root 2 "C") testState) ]
                   | k1        <- [Key 101, Key 104]
                   , k2        <- [Key 102, Key 103]
                   , (k1',k2') <- [(k1, k2), (k2, k1)] ]
prop_unionEI = and [ elem (union k1' k2' testState')
                          [ M.insert (Key 108) (Link (Key 104))
                               (M.insert (Key 104) (Root 3 "E") testState')
                          , M.insert (Key 104) (Link (Key 108))
                               (M.insert (Key 108) (Root 3 "I") testState') ]
                   | k1        <- [Key 101, Key 102, Key 104]
                   , k2        <- [Key 105, Key 106, Key 108]
                   , (k1',k2') <- [(k1, k2), (k2, k1)] ]

#ifdef SOLUTION
fresh i s = let k = case M.lookupMax s of
                      Nothing         -> Key 100
                      Just (Key k, _) -> Key (k+1)
            in (k, M.insert k (Root 0 i) s)

find k s = case s M.! k of
             Root r i -> (k, r, i, s)
             Link p   -> let (root, r, i, s') = find p s
                         in  (root, r, i, M.insert k (Link root) s')

union k1 k2 s = let (k1', r1, _, s')  = find k1 s
                    (k2', r2, i, s'') = find k2 s'
                in   if k1' == k2' then s''
                else if r1 < r2 then M.insert k1' (Link k2') s''
                else if r1 > r2 then M.insert k2' (Link k1') s''
                                else M.insert k1' (Link k2') (M.insert k2' (Root (r2+1) i) s'')
#endif

return []
main = $quickCheckAll >>= print
