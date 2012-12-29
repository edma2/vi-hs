module Zipper where

data Zipper a = Zipper a [a] [a] | Empty deriving (Eq, Show)

mkZipper :: [a] -> Zipper a
mkZipper [] = Empty
mkZipper (x:xs) = Zipper x [] xs

-- | Moves focus left or right.
left, right :: Zipper a -> Zipper a

left (Zipper t (l:ls) rs) = Zipper l ls (t:rs)
left z = z

right (Zipper t ls (r:rs)) = Zipper r (t:ls) rs
right z = z

-- | Traverse until focus satisfies predicate
rightUntil, leftUntil :: (a -> Bool) -> Zipper a -> Zipper a

rightUntil f z = until pred right z
  where pred = \z -> case z of Empty -> True
                               (Zipper _ _ []) -> True
                               (Zipper t _ _) -> f t

leftUntil f z = until pred left z
  where pred = \z -> case z of Empty -> True
                               (Zipper _ [] _) -> True
                               (Zipper t _ _) -> f t

-- | Inserts item after focus and sets as new focus.
insert :: a -> Zipper a -> Zipper a
insert x Empty = Zipper x [] []
insert x (Zipper t ls rs) = Zipper x (t:ls) rs

-- | Removes focused item.
-- New focus is set to item on rhs. If rhs is empty then
-- right-most lhs item is set as the new focus.
delete :: Zipper a -> Zipper a
delete Empty = Empty
delete (Zipper _ ls (r:rs)) = Zipper r ls rs
delete (Zipper _ (l:ls) []) = Zipper l ls []
delete (Zipper _ [] []) = Empty

-- | Modifies focused element.
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper t ls rs) = Zipper (f t) ls rs
modify f Empty = Empty
