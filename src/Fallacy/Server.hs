{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans (liftIO)
import Control.Monad (msum)
import Happstack.Server
import qualified Data.Text as T
import           Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

import qualified NLP.POS as POS
import Fallacy.TextToLogical
import Fallacy.Detector

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

myApp :: ServerPart Response
myApp = do decodeBody myPolicy 
           msum [ 
             dir "detect" $ detect,
             home 
             ]

appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        body

home :: ServerPart Response
home =
    do method GET
       ok $ toResponse $
        H.html $ do
         H.head $ do
         H.title "FallacyHunter"
         H.body $ do
          H.p "Please enter your sentence(s) in the following text box and press submit:"
          H.form ! A.enctype "multipart/form-data"
                 ! A.method "POST"
                 ! A.action "/detect" $ do
                     H.p $ do
                       H.textarea ! A.cols "50" ! A.rows "5" ! A.name "text" $ H.toHtml T.empty
                     H.p $ do
                       H.input ! A.type_ "submit" ! A.value "Submit"

fallacies input = 
        do 
           tagger <- POS.defaultTagger 
           let logicalForm = toLogicalForm (T.unpack input) tagger 
               result = findFallacies logicalForm 
               in return (toText logicalForm, toText result)
               where toText x = T.pack $ show x

detect :: ServerPart Response
detect =
   do method POST
      input <- lookText' "text"
      (lf, result) <- liftIO $ fallacies input
      ok $ toResponse $
         H.html $ do
           H.head $ do
             H.title "Fallacy Detector"
           H.body $ mkBody input lf result
    where
      mkBody text lf result = do
        H.p $ do 
               H.b $ H.toHtml $ T.pack "Input: "  
               H.toHtml text
        H.p $ do 
               H.b $ H.toHtml $ T.pack "Logical Form: "  
               H.toHtml $ lf
        H.p $ do 
               H.b $ H.toHtml $ T.pack "Detected Fallacies: "  
               H.toHtml $ result
        H.a ! A.href "/detect" $ "Try again"

main :: IO ()
main = simpleHTTP nullConf myApp
{-main = simpleHTTP nullConf $ ok "Hello, World!"-}
