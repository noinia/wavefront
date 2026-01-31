{-# LANGUAGE LambdaCase #-}
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

module Codec.Wavefront.IO where

import           Data.Bitraversable
import           Codec.Wavefront.Lexer ( lexer )
import           Codec.Wavefront.Object
import           Codec.Wavefront.Element
import           Codec.Wavefront.Material ( MaterialLib , materials )
import           Codec.Wavefront.Token ( tokenize )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.Text (Text)
import qualified Data.Text.IO as T ( readFile )
import           System.FilePath (replaceFileName)
import qualified Data.Map as Map
import qualified Codec.Wavefront.Material as Material
import           Data.Vector (Vector)

--------------------------------------------------------------------------------

-- | Extract a 'WavefrontOBJ' from a Wavefront OBJ formatted file. If
-- the file specifies mtl files, these are parsed and their
-- information is parsed as well.
fromFile    :: MonadIO m => FilePath -> m (Either String WavefrontOBJ)
fromFile fd = rawFromFile fd >>= \case
    Left err  -> pure $ Left err
    Right obj -> do let materialFiles = materialLibPath fd <$> objMtlLibs obj
                    materialLibs <- traverse Material.fromFile materialFiles
                    pure $ sequence materialLibs >>= dereferenceMaterials obj

-- | Extract a raw 'WavefrontOBJ' from a Wavefront OBJ formatted
-- file. This only parses the given file, and does not dereference any materials.
rawFromFile :: (MonadIO m) => FilePath -> m (Either String RawWavefrontOBJ)
rawFromFile fd = liftIO $ fmap fromText (T.readFile fd)

-- | Extract a 'WavefrontOBJ' from an OBJ formatted text.
fromText :: Text -> Either String RawWavefrontOBJ
fromText = fmap (ctxtToWavefrontOBJ . lexer) . tokenize

--------------------------------------------------------------------------------

-- | given an obj file and its path to an mtl file; compute the absolute path
materialLibPath :: FilePath -> FilePath -> FilePath
materialLibPath = replaceFileName

-- | Given a raw OBJ Object, and a vector of MaterialLibs, tries to
-- dereference all materials mentioned in the file.
dereferenceMaterials :: RawWavefrontOBJ -> Vector MaterialLib -> Either String WavefrontOBJ
dereferenceMaterials obj materialLibs = do
    points' <- traverse dereference (objPoints obj)
    lines'  <- traverse dereference (objLines obj)
    faces'  <- traverse dereference (objFaces obj)
    pure $ obj { objPoints    = points'
               , objLines     = lines'
               , objFaces     = faces'
               , objMtlLibs   = materialLibs
               }
  where
    materialLib = foldMap materials materialLibs
    dereference :: RawElement a -> Either String (Element a)
    dereference = firstA $ traverse $ \name -> case materialLib Map.!? name of
      Nothing -> Left $ "Material " <> show name <> " not found."
      Just m  -> Right m
