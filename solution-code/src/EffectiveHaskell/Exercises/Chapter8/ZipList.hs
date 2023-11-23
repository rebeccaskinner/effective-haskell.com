module EffectiveHaskell.Exercises.Chapter8.ZipList  where
import Prelude hiding (drop)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL

-- | A non-empty list with a focus on a single element.
--
-- A zip list is a non-empty list with a "focus" element. Supports
-- O(1) inserts, updates, and deletes at the current cursor.
data ZipList a = ZipList [a] a [a]
  deriving (Eq, Show)

-- | Creates a zip-list with a single element. The cursor is set to
-- the element.
singleton :: a -> ZipList a
singleton a = ZipList [] a []

-- | Construct a ZipList from a non-empty list with the focus on the
-- first element.
--
-- >>> fromNonEmpty (0 :| [1,2,3])
-- ZipList [] 0 [1,2,3]
--
fromNonEmpty :: NonEmpty a -> ZipList a
fromNonEmpty (a:|as) = ZipList [] a as

-- | Convert a ZipList to a non-empty list. The list will be in order
-- (the current location of the cursor does not change the list).
--
-- >>> toNonEmpty $ ZipList [] 0 [1,2,3]
-- 0 :| [1,2,3]
--
-- >>> toNonEmpty $ ZipList [3,2,1] 4 [5,6,7]
-- 1 :| [2,3,4,5,6,7]
--
toNonEmpty :: ZipList a -> NonEmpty a
toNonEmpty (ZipList h c t) = NL.prependList t' $ c :| t
  where t' = reverse h

-- | Attempt to create a ZipList from a list. Returns 'Nothing' if the
-- list is empty.
--
-- >>> fromList [1..5]
-- Just (ZipList [] 1 [2,3,4,5])
--
-- >>> fromList []
-- Nothing
--
fromList :: [a] -> Maybe (ZipList a)
fromList = fmap fromNonEmpty . NL.nonEmpty

-- | Convert a ZipList to a list. The current location of the cursor
-- does not change the generated list.
--
-- >>> toList $ ZipList [] 0 [1,2,3]
-- [0,1,2,3]
--
-- >>> toList $ ZipList [3,2,1] 4 [5,6,7]
-- [1,2,3,4,5,6,7]
--
toList :: ZipList a -> [a]
toList = NL.toList . toNonEmpty

-- | Insert a value "behind" the current cursor value, and move the
-- cursor to the newly inserted value.
--
-- >>> insertBack 9 (ZipList [1] 2 [3])
-- ZipList [2,1] 9 [3]
--
insertBack :: a -> ZipList a -> ZipList a
insertBack a (ZipList h c t) = ZipList (c:h) a t

-- | Insert a value "in front" the current cursor value, and move the
-- cursor to the newly inserted value.
--
-- >>> insertFront 9 (ZipList [1] 2 [3])
-- ZipList [1] 9 [2,3]
--
insertFront :: a -> ZipList a -> ZipList a
insertFront a (ZipList h c t) = ZipList h a (c:t)

-- | alias for 'insertFront'
insert :: a -> ZipList a -> ZipList a
insert = insertFront

-- | alias for 'insertBack'
append :: a -> ZipList a -> ZipList a
append = insertBack

-- | Replace the value at the cursor.
--
-- >>> update 9 $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [3,2,1] 9 [5,6,7]
update :: a -> ZipList a -> ZipList a
update a (ZipList h _ t) = ZipList h a t

-- | Remove the element at the cursor. The cursor will move toward the
-- tail of the list, except when dropping the last element in the zip
-- list.
--
-- >>> drop $ ZipList [3,2,1] 4 [5,6,7]
-- Just (ZipList [3,2,1] 5 [6,7])
--
-- >>> drop $ ZipList [3,2,1] 5 [6,7]
-- Just (ZipList [3,2,1] 6 [7])
--
-- >>> drop $ ZipList [3,2,1] 7 []
-- Just (ZipList [2,1] 3 [])
--
-- drop $ ZipList [] 1 [2,3]
-- Just (ZipList [] 2 [3])
--
-- >>> drop $ ZipList [] 1 []
-- Nothing
--
drop :: ZipList a -> Maybe (ZipList a)
drop (ZipList h _c t)
  | (x:xs) <- t = Just $ ZipList h x xs
  | (x:xs) <- h = Just $ ZipList xs x t
  | otherwise = Nothing

-- | Like 'drop' but does not check to ensure the resulting
-- list is not empty.
unsafeDrop :: ZipList a -> ZipList a
unsafeDrop (ZipList h _c t)
  | (x:xs) <- t = ZipList h x xs
  | (x:xs) <- h = ZipList xs x t
  | otherwise = error "empty ZipList"

-- | Returns 'True' if the cursor is at the start of the zip list.
isStart :: ZipList a -> Bool
isStart (ZipList h _c _t) = null h

-- | Returns 'True' if the cursor is at the end of the zip list.
isEnd :: ZipList a -> Bool
isEnd (ZipList _h _c t) = null t

-- | Set the cursor to the beginning of the list.
cursorStart :: ZipList a -> ZipList a
cursorStart z
  | isStart z = z
  | otherwise = cursorStart $ prev z

-- | Set the cursor to the end of the list.
cursorEnd :: ZipList a -> ZipList a
cursorEnd z
  | isEnd z = z
  | otherwise = cursorEnd $ next z

-- | Return the value under the cursor.
value :: ZipList a -> a
value (ZipList _ c _) = c

-- | Move the value "forward" one element (toward the tail). Has no
-- effect if the cursor is already at the end of the list.
--
-- >>> next $ ZipList [] 1 [2,3,4]
-- ZipList [1] 2 [3,4]
--
-- >>> next $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [4,3,2,1] 5 [6,7]
--
-- >>> next $ ZipList [3,2,1] 4 []
-- ZipList [3,2,1] 4 []
--
next :: ZipList a -> ZipList a
next z@(ZipList _h _c []) = z
next (ZipList h c (x:xs)) = ZipList (c:h) x xs

-- | Move the value "backwards" one element (toward the head). Has no
-- effect if the cursor is already at the start of the list.
--
-- >>> prev $ ZipList [3,2,1] 4 []
-- ZipList [2,1] 3 [4]
--
-- >>> prev $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [2,1] 3 [4,5,6,7]
--
-- >>> prev $ ZipList [] 4 [5,6,7]
-- ZipList [] 4 [5,6,7]
--
prev :: ZipList a -> ZipList a
prev z@(ZipList [] _c _t) = z
prev (ZipList (x:xs) c t) = ZipList xs x (c:t)
