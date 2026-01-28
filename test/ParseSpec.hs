{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings  #-}
module ParseSpec where

import           Data.Foldable
import           Test.Hspec
import           Codec.Wavefront.IO
import           Codec.Wavefront.Object
import           Codec.Wavefront.Face
import           Data.Either (isRight)
import qualified Paths_wavefront as Paths
import           Codec.Wavefront.Face
import           Codec.Wavefront.Element
import           Codec.Wavefront.Token
import           Codec.Wavefront.Location
import qualified Data.Text.IO as T ( readFile )
import           Data.Text (pack)
import qualified Codec.Wavefront.Material as M
import qualified Codec.Wavefront.Material.Token as T
import           Codec.Wavefront.Material.Type (Reflexivity(..), IluminationModel(..), RGB(..), CIEXYZ(..))
import qualified Data.Attoparsec.Text as AP


--------------------------------------------------------------------------------

cubeFile :: IO FilePath
cubeFile = Paths.getDataFileName "cube.obj"

spec :: Spec
spec = describe "parsing tests" $ do
         res <- runIO $ cubeFile >>= rawFromFile
         it "it parses my cube.obj file" $
           res `shouldSatisfy` isRight

         res <- runIO $ cubeFile >>= fromFile
         it "it parses my cube file including its materials" $
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


             it "face has a color" $
               for_ (objFaces obj) $ \el ->
                 faceColor el `shouldBe` Just (ReflexicityRGB (RGB 1 1 1))

         it "tokenizes a vertex correctly" $
           tokenize "v   1.00  0.00   0.99" `shouldBe` Right [TknV $ Location 1 0 0.99 1]

         it "tokenizes vertices correctly" $
           tokenize myStr `shouldBe` Right [ TknV $ Location (-1.01) 0 0.99 1
                                           , TknV $ Location 1 0 0.99 1
                                           ]

         cornelBoxStr <- runIO $ Paths.getDataFileName "CornellBox-Mirror.obj" >>= T.readFile
         it "tokenize cornelBox" $
           tokenize cornelBoxStr `shouldSatisfy` isRight

         mtlSpec




myStr = "v  -1.01  0.00   0.99\nv   1.00  0.00   0.99"


faceColor el = elMtl el >>= M.ambientReflexivity

i `inRangeOf` n = 1 <= i && i <= n

-- locIndices obj = foldMap (\el -> let  = elValue el
--                                  in
--                          ) (objFaces obj)

faceIndices (Face i j k is) = faceLocIndex <$> i:j:is


tokenizer' = AP.choice
             [ fmap (Just . T.TknName)    T.material
             , fmap (Just . T.TknKa)      T.ambient
             , Nothing <$ comment
             ]


mtlSpec = describe "mtl parsing test" $ do
            describe "material" $ do
              it "parse" $
                AP.parse T.material "  newmtl leftWall\n" `shouldSatisfy` (\case
                  AP.Done _ _ -> True
                  _           -> False
                )

              it "parse until end " $
                AP.parseOnly (untilEnd T.material) "  newmtl leftWall\n"
                `shouldSatisfy`
                isRight

              it "parse until end no newline" $
                AP.parseOnly (untilEnd T.material) "  newmtl leftWall"
                `shouldSatisfy`
                isRight

              it "parse tokenizer' until end no newline" $
                AP.parseOnly (untilEnd tokenizer') "  newmtl leftWall"
                `shouldSatisfy`
                isRight

              it "parse tokenizer' until end with newline" $
                AP.parseOnly (untilEnd tokenizer') "  newmtl leftWall\n"
                `shouldSatisfy`
                isRight

              it "parse tokenizer' on small string" $
                AP.parseOnly (untilEnd tokenizer') "newmtl leftWall\n"
                `shouldSatisfy`
                isRight

              it "tokenizes" $
                T.tokenize "  newmtl leftWall\n" `shouldSatisfy` isRight

            describe "material" $ do
              it "tokenizes ambient correctly" $
                T.tokenize "  Ka 0.63 0.065 0.05\n"
                `shouldBe`
                Right [ T.TknKa (ReflexicityRGB (RGB 0.63 0.065 0.05))
                      ]

              it "tokenizes ambient with comment correctly" $
                T.tokenize "  Ka 0.63 0.065 0.05  # Red \n"
                `shouldBe`
                Right [ T.TknKa (ReflexicityRGB (RGB 0.63 0.065 0.05))
                      ]

            describe "tokenize full files" $ do
              tokenizeFile "small.mtl"
              describe "cornellbox" $ do
                let versions = [ "CornellBox-Mirror.mtl"
                               , "CornellBox-Original.mtl"
                               , "CornellBox-Sphere.mtl"
                               , "CornellBox-Water.mtl"
                               , "water.mtl"
                               , "CornellBox-Empty-CO.mtl"
                               , "CornellBox-Empty-RG.mtl"
                               , "CornellBox-Empty-Squashed.mtl"
                               , "CornellBox-Empty-White.mtl"
                               , "CornellBox-Glossy-Floor.mtl"
                               , "CornellBox-Glossy.mtl"
                               ]
                for_ versions $ \version ->
                  tokenizeFile ("cornellbox/" <> version)

tokenizeFile    :: FilePath -> Spec
tokenizeFile fp = do str <- runIO $ Paths.getDataFileName fp >>= T.readFile
                     it ("tokenize " <> fp) $
                       T.tokenize str `shouldSatisfy` isRight
                     it ("parse into materials " <> fp) $
                       M.fromText str `shouldSatisfy` isRight
