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

module Codec.Wavefront.Lexer where

import Codec.Wavefront.Element
import Codec.Wavefront.Face
import Codec.Wavefront.Line
import Codec.Wavefront.Location
import Codec.Wavefront.Normal
import Codec.Wavefront.Point
import Codec.Wavefront.Token
import Codec.Wavefront.TexCoord
import Codec.Wavefront.SizedDList (SizedDList)
import qualified Codec.Wavefront.SizedDList as SizedDList
import qualified Data.DList as DList
import Data.DList ( DList ) --, append, empty, fromList, snoc )
import Data.Text ( Text )
import Control.Monad.State ( State, execState, gets, modify )
import Data.Foldable ( traverse_ )
import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------

-- |The lexer context. The result of lexing a stream of tokens is this exact type.
data Ctxt = Ctxt {
    -- |Locations.
    ctxtLocations :: SizedDList Location
    -- |Texture coordinates.
  , ctxtTexCoords :: SizedDList TexCoord
    -- |Normals.
  , ctxtNormals :: SizedDList Normal
    -- |Points.
  , ctxtPoints :: DList (RawElement Point)
    -- |Lines.
  , ctxtLines :: DList (RawElement Line)
    -- |Faces.
  , ctxtFaces :: DList (RawElement Face)
    -- |Current object.
  , ctxtCurrentObject :: Maybe Text
    -- |Current groups.
  , ctxtCurrentGroups :: [Text]
    -- |Current material.
  , ctxtCurrentMtl :: Maybe Text
    -- |Material libraries.
  , ctxtMtlLibs :: DList FilePath
    -- |Current smoothing group.
  , ctxtCurrentSmoothingGroup :: Natural
  } deriving (Eq,Show)

-- |The empty 'Ctxt'. Such a context exists at the beginning of the token stream and gets altered
-- as we consume tokens.
emptyCtxt :: Ctxt 
emptyCtxt = Ctxt {
    ctxtLocations = SizedDList.empty
  , ctxtTexCoords = SizedDList.empty
  , ctxtNormals = SizedDList.empty
  , ctxtPoints = DList.empty
  , ctxtLines = DList.empty
  , ctxtFaces = DList.empty
  , ctxtCurrentObject = Nothing
  , ctxtCurrentGroups = ["default"]
  , ctxtCurrentMtl = Nothing
  , ctxtMtlLibs = DList.empty
  , ctxtCurrentSmoothingGroup = 0
  }

-- |The lexer function, consuming tokens and yielding a 'Ctxt'.
lexer :: TokenStream -> Ctxt
lexer stream = execState (traverse_ consume stream) emptyCtxt
  where
    consume tk = case tk of
      TknV v -> do
        locations <- gets ctxtLocations
        modify $ \ctxt -> ctxt { ctxtLocations = locations `SizedDList.snoc` v }
      TknVN vn -> do
        normals <- gets ctxtNormals
        modify $ \ctxt -> ctxt { ctxtNormals = normals `SizedDList.snoc` vn }
      TknVT vt -> do
        texCoords <- gets ctxtTexCoords
        modify $ \ctxt -> ctxt { ctxtTexCoords = texCoords `SizedDList.snoc` vt }
      TknP p -> do
        (pts,element) <- prepareElement ctxtPoints
        modify $ \ctxt -> ctxt { ctxtPoints = pts `DList.append` fmap element (DList.fromList p) }
      TknL l -> do
        (lns,element) <- prepareElement ctxtLines
        modify $ \ctxt -> ctxt { ctxtLines = lns `DList.append` fmap element (DList.fromList l) }
      TknF f -> do
        (fcs,element) <- prepareElement ctxtFaces
        modify $ \ctxt -> let numLoc      = length . ctxtLocations $ ctxt
                              numTextures = length . ctxtTexCoords $ ctxt
                              numNormals  = length . ctxtNormals   $ ctxt
                              f' = mapFaceIndices (toAbsoluteIndex numLoc numTextures numNormals) f
                          in ctxt { ctxtFaces = fcs `DList.snoc` element f' }
      TknG g -> modify $ \ctxt -> ctxt { ctxtCurrentGroups = g }
      TknO o -> modify $ \ctxt -> ctxt { ctxtCurrentObject = Just o }
      TknMtlLib l -> do
        libs <- gets ctxtMtlLibs
        modify $ \ctxt -> ctxt { ctxtMtlLibs = libs `DList.append` DList.fromList l }
      TknUseMtl mtl -> modify $ \ctxt -> ctxt { ctxtCurrentMtl = Just mtl }
      TknS sg -> modify $ \ctxt -> ctxt { ctxtCurrentSmoothingGroup = sg }

-- Prepare to create a new 'RawElement by retrieving its associated list.
prepareElement :: (Ctxt -> DList (RawElement a)) -> State Ctxt (DList (RawElement a),a -> RawElement a)
prepareElement field = do
  (aList,obj,grp,mtl,sg) <- gets $ (\ctxt -> (field ctxt,ctxtCurrentObject ctxt,ctxtCurrentGroups ctxt,ctxtCurrentMtl ctxt,ctxtCurrentSmoothingGroup ctxt))
  pure (aList,Element obj grp mtl sg)

-- | map some function over the face indices
mapFaceIndices                    :: (FaceIndex -> FaceIndex) -> Face -> Face
mapFaceIndices f (Face i j k is) = Face (f i) (f j) (f k) (map f is)

-- | Given the current number of locations, the current number of
-- texture coordinates, and the current number of normals, make sure
-- that the faceIndex uses absolute indices rather than relative ones.
toAbsoluteIndex :: Int -> Int -> Int -> FaceIndex -> FaceIndex
toAbsoluteIndex nLoc nTex nNorm (FaceIndex li ti ni) =
    FaceIndex (toAbs nLoc li) (toAbs nTex <$> ti) (toAbs nNorm <$> ni)
  where
    toAbs n i | i >= 0    = i
              | otherwise = n + i -- == n - abs i
