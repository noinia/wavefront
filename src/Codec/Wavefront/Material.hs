module Codec.Wavefront.Material
  ( fromFile
  , fromText
  , Material(..)
  ) where

import           Codec.Wavefront.Material.Type
import           Codec.Wavefront.Material.Lexer ( lexer )
import           Codec.Wavefront.Material.Token ( tokenize )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Data.Text (Text)
import qualified Data.Text.IO as T ( readFile )
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- | Extract Materials from a Wavefront MTL formatted file.
fromFile    :: (MonadIO m) => FilePath -> m (Either String (Map.Map MaterialName Material))
fromFile fd = liftIO $ fmap fromText (T.readFile fd)

-- | Extract Materials from a Wavefront MTL formatted text.
fromText :: Text -> Either String (Map.Map MaterialName Material)
fromText = fmap lexer . tokenize
