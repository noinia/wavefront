module Codec.Wavefront.SizedDList
  ( SizedDList
  , append
  , empty
  , fromList
  , snoc
  ) where

import Data.Foldable
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Monoid as Monoid
--------------------------------------------------------------------------------

-- | A DList that supports O(1) length
data SizedDList a = SizedDList {-#UNPACK#-}!Int (DList.DList a)
                  deriving (Show,Eq)

instance Foldable SizedDList where
  {-# INLINE fold #-}
  fold = Monoid.mconcat . toList
  {-# INLINE foldMap #-}
  foldMap f = foldMap f . toList
  {-# INLINE foldr #-}
  foldr f x = List.foldr f x . toList
  {-# INLINE foldl #-}
  foldl f x = List.foldl f x . toList
  {-# INLINE foldr1 #-}
  foldr1 f = List.foldr1 f . toList
  {-# INLINE foldl1 #-}
  foldl1 f = List.foldl1 f . toList
  {-# INLINE foldr' #-}
  foldr' f x = foldr' f x . toList
  {-# INLINE toList #-}
  toList (SizedDList _ xs) = toList xs
  {-# INLINE length #-}
  length (SizedDList n _) = n
  {-# INLINE null #-}
  null (SizedDList n _) = n == 0


-- | Create an empty sized DList
empty :: SizedDList a
empty = SizedDList 0 DList.empty

-- | Appends two sized DLists
append :: SizedDList a -> SizedDList a -> SizedDList a
append (SizedDList n xs) (SizedDList m ys) = SizedDList (n+m) (DList.append xs ys)

-- | Convert a list into a sized DList
fromList :: [a] -> SizedDList a
fromList xs = SizedDList (length xs) (DList.fromList xs)

-- | Append an element
snoc :: SizedDList a -> a -> SizedDList a
snoc (SizedDList n xs) x = SizedDList (n+1) (DList.snoc xs x)
