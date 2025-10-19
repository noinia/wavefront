{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings  #-}
module ParseSpec where

import Data.Foldable
import Test.Hspec
import Codec.Wavefront.IO
import Codec.Wavefront.Object
import Codec.Wavefront.Face
import Data.Either(isRight)
import qualified Paths_wavefront as Paths
import Codec.Wavefront.Face
import Codec.Wavefront.Element
import Codec.Wavefront.Token
import Codec.Wavefront.Location

import qualified Data.Text.IO as T ( readFile )


import Debug.Trace
--------------------------------------------------------------------------------

cubeFile :: IO FilePath
cubeFile = Paths.getDataFileName "cube.obj"

spec :: Spec
spec = describe "parsing tests" $ do
         res <- runIO $ cubeFile >>= fromFile
         it "it parses my cube file" $
           res `shouldSatisfy` isRight
         case res of
           Left msg  -> error msg
           Right obj -> do
             let numLocs  = length $ objLocations obj
                 -- numTex   = length $ objTexCoords obj
                 -- numNorms = length $ objNormals   obj
             it ("faces have valid location indices" <> show numLocs) $ do
               for_ (objFaces obj) $ \el -> let face = elValue el in
                                     face `shouldSatisfy` all (`inRangeOf` numLocs) . faceIndices
         it "tokenizes a vertex correctly" $
           tokenize "v   1.00  0.00   0.99" `shouldBe` Right [TknV $ Location 1 0 0.99 1]

         it "tokenizes vertices correctly" $
           tokenize myStr `shouldBe` Right [ TknV $ Location (-1.01) 0 0.99 1
                                           , TknV $ Location 1 0 0.99 1
                                           ]
         cornelBoxStr <- runIO $ Paths.getDataFileName "CornellBox-Mirror.obj" >>= T.readFile
         it "tokenize cornelBox" $
           tokenize cornelBoxStr `shouldSatisfy` isRight

myStr = "v  -1.01  0.00   0.99\nv   1.00  0.00   0.99"



i `inRangeOf` n = 0 <= i && i < n

-- locIndices obj = foldMap (\el -> let  = elValue el
--                                  in
--                          ) (objFaces obj)

faceIndices (Face i j k is) = faceLocIndex <$> i:j:is
