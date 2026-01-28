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

module Codec.Wavefront.Object where

import Data.Foldable ( toList )
import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Lexer ( Ctxt(..) )
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Material (MaterialLib, Material)
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.SizedDList ( SizedDList)
import Codec.Wavefront.TexCoord
import Data.DList ( DList )
import Data.Text ( Text )
import Data.Vector ( Vector, fromList )

--------------------------------------------------------------------------------

-- | Prototype result for a WaveFrontOBJ File
--
-- Note according to the OBJ-spec, locations are "1-indexed"; i.e. in
-- the range [1..n] rather than [0..n-1]. Hence, the indices in the
-- Points, Lines, and Faces all refer to these 1-indexed points.
data WavefrontOBJF mtlLib material = WavefrontOBJ {
    -- |Locations.
    objLocations :: Vector Location
    -- |Texture coordinates.
  , objTexCoords :: Vector TexCoord
    -- |Normals.
  , objNormals :: Vector Normal
    -- |Points.
  , objPoints :: Vector (ElementF material Point)
    -- |Lines.
  , objLines :: Vector (ElementF material Line)
    -- |Faces.
  , objFaces :: Vector (ElementF material Face)
    -- |Material libraries.
  , objMtlLibs :: Vector mtlLib
  } deriving (Eq,Show)

-- | The content of a WavefrontOBJ file, in which the materials have been dereferenced.
type WavefrontOBJ = WavefrontOBJF MaterialLib (Maybe Material)

-- | A wavefront object whose material has not been dereferenced yet
type RawWavefrontOBJ = WavefrontOBJF FilePath (Maybe Text)

ctxtToWavefrontOBJ :: Ctxt -> RawWavefrontOBJ
ctxtToWavefrontOBJ ctxt = WavefrontOBJ {
    objLocations = fromSizedDList (ctxtLocations ctxt)
  , objTexCoords = fromSizedDList (ctxtTexCoords ctxt)
  , objNormals = fromSizedDList (ctxtNormals ctxt)
  , objPoints = fromDList (ctxtPoints ctxt)
  , objLines = fromDList (ctxtLines ctxt)
  , objFaces = fromDList (ctxtFaces ctxt)
  , objMtlLibs = fromDList (ctxtMtlLibs ctxt)
  }

fromDList :: DList a -> Vector a
fromDList = fromList . toList

fromSizedDList :: SizedDList a -> Vector a
fromSizedDList = fromList . toList
