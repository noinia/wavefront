module Codec.Wavefront.Material
  ( MaterialLib(..)
  , fromFile
  , fromText
  , Material(..)
  , MaterialName
  , RGB(..)
  , CIEXYZ(..)
  , Reflexivity(..)
  , IluminationModel(..)
  ) where

import           Codec.Wavefront.Material.Type
import           Codec.Wavefront.Material.Lexer ( lexer )
import           Codec.Wavefront.Material.Token ( tokenize )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.Text (Text)
import qualified Data.Text.IO as T ( readFile )
import qualified Data.Map as Map
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A MaterialLib
data MaterialLib = MaterialLib { materialLibPath :: FilePath
                               , materials       :: Map.Map MaterialName Material
                               }
                   deriving (Show,Eq,Generic)

-- | Extract Materials from a Wavefront MTL formatted file.
fromFile    :: (MonadIO m) => FilePath -> m (Either String MaterialLib)
fromFile fp = liftIO $ fmap (fmap (MaterialLib fp) . fromText) (T.readFile fp)

-- | Extract Materials from a Wavefront MTL formatted text.
fromText :: Text -> Either String (Map.Map MaterialName Material)
fromText = fmap lexer . tokenize
