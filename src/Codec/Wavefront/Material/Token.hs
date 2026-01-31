{-# LANGUAGE OverloadedStrings #-}
module Codec.Wavefront.Material.Token where

import           Control.Applicative
import           Codec.Wavefront.Material.Type (Reflexivity(..), IluminationModel(..), RGB(..), CIEXYZ(..))
import           Data.Attoparsec.Text as AP
import           Data.Text ( Text )
import           Prelude hiding ( lines )
import           Codec.Wavefront.Token ( name, comment, float, eol, skipHSpace, untilEnd
                                       , cleanupTokens, analyseResult
                                       )

--------------------------------------------------------------------------------


data Token = TknName Text
           | TknKa Reflexivity
           | TknKd Reflexivity
           | TknKe Reflexivity
           | TknKs Reflexivity
           | TknTf Reflexivity
           | TknIm IluminationModel
           | TknD Float
           | TknSn Float
           | TknS Float
           | TknDensity Float
           deriving (Show,Eq)


-- |A stream of 'Token'.
type TokenStream = [Token]

tokenize :: Text -> Either String TokenStream
tokenize = fmap cleanupTokens . analyseResult False . parse (untilEnd tokenizer)
  where
    tokenizer = choice
      [ fmap (Just . TknName)    material
      , fmap (Just . TknKa)      ambient
      , fmap (Just . TknKd)      diffuse
      , fmap (Just . TknKe)      specular
      , fmap (Just . TknKs)      emissive
      , fmap (Just . TknTf)      transmissionFilter
      , fmap (Just . TknIm)      iluminationModel
      , fmap (Just . TknD)       disolveFilter
      , fmap (Just . TknSn)      specularExponent
      , fmap (Just . TknS)       sharpness
      , fmap (Just . TknDensity) opticalDensity
      , Nothing <$ comment
      ]

-- analyseResult :: Bool -> Result res -> Either String res
-- analyseResult partial r = case r of
--   Done _ tkns -> Right tkns
--   Fail i _ e -> Left $ "`" ++ Prelude.take 100 (unpack i) ++ "` [...]: " ++ e
--   Partial p -> if partial then Left "not completely tokenized"
--                           else analyseResult True (p T.empty)

-- cleanupTokens :: [Maybe Token] -> TokenStream
-- cleanupTokens = catMaybes

--------------------------------------------------------------------------------

material :: Parser Text
material = skipSpace *> string "newmtl " *> name <* eol

ambient :: Parser Reflexivity
ambient = skipSpace *> string "Ka " *> reflexivity <* eol'


reflexivity :: Parser Reflexivity
reflexivity = choice [                        ReflexicityRGB <$> rgb
                     , string "spectral " *> (ReflexivitySpectral <$>
                                               filePath <*> option Nothing (Just <$> float))
                     , string "xyz "      *> (ReflexivityCIE <$> cie)
                     ]

rgb :: Parser RGB
rgb = do rgb' <- float `sepBy1` skipHSpace
         case rgb' of
           [r]       -> pure $ RGB r r r
           [r,g,b]   -> pure $ RGB r g b
           _ -> fail "wrong number of r, g and b arguments for rgb"

cie :: Parser CIEXYZ
cie = do xyz <- float `sepBy1` skipHSpace
         case xyz of
           [x]       -> pure $ CIEXYZ x x x
           [x,y,z]   -> pure $ CIEXYZ x y z
           _ -> fail "wrong number of x, y and z arguments for xyz"

filePath :: Parser FilePath
filePath = undefined

diffuse :: Parser Reflexivity
diffuse = skipSpace *> string "Kd " *> reflexivity <* eol'

specular  :: Parser Reflexivity
specular = skipSpace *> string "Ks " *> reflexivity <* eol'

emissive  :: Parser Reflexivity
emissive = skipSpace *> string "Ke " *> reflexivity <* eol'

transmissionFilter :: Parser Reflexivity
transmissionFilter = skipSpace *> string "Tf " *> reflexivity <* eol'

iluminationModel :: Parser IluminationModel
iluminationModel = do skipSpace <* string "illum "
                      i <- decimal
                      eol'
                      if 1 <= i && i <= 10 then pure $ toEnum (i-1)
                                           else fail "invalid illumination model"

disolveFilter :: Parser Float
disolveFilter = skipSpace *> (disolve <|> transparent) <* eol'
  where
    disolve     = string "d "  *> float
    transparent = string "Tr " *> ((1 -) <$> float)


specularExponent :: Parser Float
specularExponent = skipSpace *> string "Ns " *> float <* eol'

sharpness :: Parser Float
sharpness = skipSpace *> string "sharpness " *> float <* eol'

opticalDensity :: Parser Float
opticalDensity = skipSpace *> string "Ni " *> float <* eol'

-- | Comment or end of line
eol' = comment <|> eol
