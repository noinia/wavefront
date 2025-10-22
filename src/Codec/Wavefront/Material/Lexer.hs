{-# LANGUAGE LambdaCase #-}
module Codec.Wavefront.Material.Lexer where

import           Codec.Wavefront.Material.Token (Token(..), TokenStream)
import           Codec.Wavefront.Material.Type
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- | The lexer function, consuming tokens and produces a Map of Materials
lexer        :: TokenStream -> Map.Map MaterialName Material
lexer stream = case splitWith getMaterialName stream of
                 (_, materialDefs) -> foldMap toMaterial materialDefs
  where
    getMaterialName = \case
      TknName name -> Just name
      _            -> Nothing

    toMaterial (name, decls) = Map.singleton name material
      where
        material = foldl' assign (defaultMaterial name) decls
        assign m = \case
          TknName _      -> m -- this should not happen
          TknKa r        -> m { ambientReflexivity  = Just r }
          TknKd r        -> m { diffuseReflexivity  = Just r }
          TknKe r        -> m { specularReflexivity = Just r }
          TknKs r        -> m { emissiveReflexivity = Just r }
          TknTf r        -> m { transmissionFilter  = Just r }
          TknIm im       -> m { iluminationModel    = Just im }
          TknD d         -> m { disolveFactor       = Just d }
          TknSn specExp  -> m { specularExponent    = Just specExp }
          TknS s         -> m { sharpness           = Just s }
          TknDensity od  -> m { opticalDensity      = Just od }

-- | split when we produce a Just
splitWith   :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
splitWith f = go
  where
    go []     = ([],[])
    go (x:xs) = let (pref,rest) = go xs
                in case f x of
                     Nothing -> (x:pref, rest)
                     Just y  -> ([], (y,x:pref) : rest)
