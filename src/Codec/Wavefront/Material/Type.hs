module Codec.Wavefront.Material.Type where

import Data.Text ( Text )
import GHC.Generics (Generic)


--------------------------------------------------------------------------------

-- | Specification of a Material
data Material = Material { materialName        :: MaterialName
                         , ambientReflexivity  :: Maybe Reflexivity
                         , diffuseReflexivity  :: Maybe Reflexivity
                         , specularReflexivity :: Maybe Reflexivity
                         , emissiveReflexivity :: Maybe Reflexivity
                         , transmissionFilter  :: Maybe Reflexivity
                         , iluminationModel    :: Maybe IluminationModel
                         , disolveFactor       :: Maybe Float
                         , specularExponent    :: Maybe Float
                         , sharpness           :: Maybe Float
                         , opticalDensity      :: Maybe Float
                         } deriving (Show,Eq,Generic)

-- | Construct a default material given a name
defaultMaterial      :: MaterialName -> Material
defaultMaterial name = Material { materialName        = name
                                , ambientReflexivity  = Nothing
                                , diffuseReflexivity  = Nothing
                                , specularReflexivity = Nothing
                                , emissiveReflexivity = Nothing
                                , transmissionFilter  = Nothing
                                , iluminationModel    = Nothing
                                , disolveFactor       = Nothing
                                , specularExponent    = Nothing
                                , sharpness           = Nothing
                                , opticalDensity      = Nothing
                                }


--------------------------------------------------------------------------------

type MaterialName = Text

data RGB = RGB {-#UNPACK#-}!Float {-#UNPACK#-}!Float {-#UNPACK#-}!Float
  deriving (Show,Eq,Generic)

data CIEXYZ = CIEXYZ {-#UNPACK#-}!Float {-#UNPACK#-}!Float {-#UNPACK#-}!Float
  deriving (Show,Eq,Generic)


data Reflexivity = ReflexicityRGB {-#UNPACK#-}!RGB
                 | ReflexivitySpectral FilePath (Maybe Float)
                          -- multiplication factor; default is one
                 | ReflexivityCIE {-#UNPACK#-}!CIEXYZ
                 deriving (Show,Eq,Generic)




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
                      deriving (Show,Eq,Enum,Bounded,Generic)


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



--------------------------------------------------------------------------------

-- -- | Possible Material specifications
-- data MaterialSpec = NoMaterial
--                   | ReferencedMaterial MaterialName
--                   | AMaterial Material
--                   deriving (Show,Eq)
