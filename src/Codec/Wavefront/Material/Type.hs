module Codec.Wavefront.Material.Type where

import Data.Text ( Text )


data RGB = RGB {-#UNPACK#-}!Float {-#UNPACK#-}!Float {-#UNPACK#-}!Float
  deriving (Show,Eq)

data CIEXYZ = CIEXYZ {-#UNPACK#-}!Float {-#UNPACK#-}!Float {-#UNPACK#-}!Float
  deriving (Show,Eq)


data Reflexivity = ReflexicityRGB {-#UNPACK#-}!RGB
                 | ReflexivitySpectral FilePath (Maybe Float)
                          -- multiplication factor; default is one
                 | ReflexivityCIE {-#UNPACK#-}!CIEXYZ
                 deriving (Show,Eq)

data Material = Material { materialName        :: Text
                         , ambientReflexivity  :: Maybe Reflexivity
                         , diffuseReflexivity  :: Maybe Reflexivity
                         , specularReflexivity :: Maybe Reflexivity
                         , emmisiveReflexivity :: Maybe Reflexivity
                         , transmissionFilter  :: Maybe Reflexivity
                         , iluminationModel    :: Maybe IluminationModel
                         , disolveFactor       :: Maybe Float
                         , specularExponent    :: Maybe Float
                         , sharpness           :: Maybe Float
                         , opticalDensity      :: Maybe Float
                         } deriving (Show,Eq)


data IluminationModel = ColorOnly                     --  0
                      | ColorAndAmbient               --  1
                      | Higlight                      --  2
                      | ReflectionAndRayTrace         --  3
                      | GlassAndRayTrace              --  4
                      | FresnelAndRayTrace            --  5
                      | RefractionAndRayTrace         --  6
                      | RefactionFresnelAndRayTrace   --  7
                      | ReflectionOnly                --  8
                      | GlassOnly                     -- 9
                      | CastsShadows                  -- 10
                      deriving (Show,Eq,Enum,Bounded)


--  0           Color on and Ambient off
--  1           Color on and Ambient on
--  2           Highlight on
--  3           Reflection on and Ray trace on
--  4           Transparency: Glass on
--              Reflection: Ray trace on
--  5           Reflection: Fresnel on and Ray trace on
--  6           Transparency: Refraction on
--              Reflection: Fresnel off and Ray trace on
-- 7            Transparency: Refraction on
--              Reflection: Fresnel on and Ray trace on
--   8          Reflection on and Ray trace off
--   9          Transparency: Glass on
--              Reflection: Ray trace off
--  10          Casts shadows onto invisible surfaces
