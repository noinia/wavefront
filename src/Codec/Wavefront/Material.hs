module Codec.Wavefront.Material
  ( MaterialLib
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

--------------------------------------------------------------------------------

-- | A MaterialLib
type MaterialLib = Map.Map MaterialName Material

-- | Extract Materials from a Wavefront MTL formatted file.
fromFile    :: (MonadIO m) => FilePath -> m (Either String MaterialLib)
fromFile fd = liftIO $ fmap fromText (T.readFile fd)

-- | Extract Materials from a Wavefront MTL formatted text.
fromText :: Text -> Either String MaterialLib
fromText = fmap lexer . tokenize
