module Codec.Wavefront.Material.Token where

import Codec.Wavefront.Material.Type
import Data.Attoparsec.Text as AP
import Data.Char ( isSpace )
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack, strip )
import qualified Data.Text as T ( empty )
import Numeric.Natural ( Natural )
import Prelude hiding ( lines )

--------------------------------------------------------------------------------
