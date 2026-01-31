{-# LANGUAGE DeriveTraversable  #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-----------------------------------------------------------------------------

module Codec.Wavefront.Element (
    -- * Element
    ElementF(..)
  , Element
  , RawElement
  , toRawElement
  ) where

import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor
import Data.Text ( Text )
import Numeric.Natural ( Natural )
import Codec.Wavefront.Material (Material, materialName)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- |An element holds a value along with the user-defined object’s name (if any), the associated
-- groups, the used material and the smoothing group the element belongs to (if any). Those values
-- can be used to sort the data per object or per group and to lookup materials.
data ElementF material a = Element {
    elObject :: Maybe Text
  , elGroups :: [Text]
  , elMtl :: material
  , elSmoothingGroup :: Natural
  , elValue :: a
  } deriving (Eq,Show,Functor,Foldable,Traversable,Generic)

-- | An element whose material has not been dereferenced yet
type RawElement = ElementF (Maybe Text)

-- | An element whose material has been dereferenced.
type Element = ElementF (Maybe Material)


instance Bifunctor ElementF where
  bimap f g (Element obj grs mtl smthgr val) = Element obj grs (f mtl) smthgr (g val)

instance Bifoldable ElementF where
  bifoldMap f g el = f (elMtl el) <> g (elValue el)

instance Bitraversable ElementF where
  bitraverse f g el = (\material val -> el { elMtl   = material
                                           , elValue = val
                                           }
                      ) <$> f (elMtl el) <*> g (elValue el)

-- | convert an element into a raw element
toRawElement   :: Element a -> RawElement a
toRawElement e = e { elMtl = fmap materialName (elMtl e) }
