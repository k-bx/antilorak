{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow              (second)
import           Control.Exception.Enclosed
import           Control.Lens               hiding (argument)
import qualified Data.Aeson                 as J
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid
import qualified Data.String.Class          as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Network.HTTP.Client        as HTTPClient
import qualified Network.HTTP.Client.TLS    as HTTPClient
import           Network.HTTP.Types.URI     (urlEncode)
import           Network.Wreq
import           Options.Applicative.Simple
import           Safe                       hiding (at)

main :: IO ()
main = do
  (article,()) <-
     simpleOptions "1.0.0"
                   "Antilorak -- improving Pantheon"
                   ("This tool displays article views in its all languages.\n\n"<>
                    "Example usage: stack exec antilorak -- Ani_Lorak")
                   (argument str (metavar "ARTICLE_NAME"))
                   empty

  mgr <- HTTPClient.newManager HTTPClient.tlsManagerSettings
  let opts = defaults & manager .~ (Right mgr)
  let langlinksUrl =
        "https://en.wikipedia.org/w/api.php?action=query&titles="
        <> article <> "&prop=langlinks&format=json&lllimit=500"
  rsp <- getWith opts (S.toString langlinksUrl)
  let langlinkObjs = (rsp ^. responseBody) ^.. key "query" . key "pages" . _Object
                                             . to toList . to headMay . _Just
                                             . key "langlinks" . _Array . traverse
  let toPair o = (o ^. key "lang" . _String,
                  o ^. key "*" . _String)
      escName = S.toText
              . urlEncode False
              . S.toStrictByteString
              . T.replace " " "_"
      links :: [(Text,Text)]
      links = map (second escName . toPair)
                  langlinkObjs
  results <- forM links $ \(lang, articleName) -> do
    let uri = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/"
              <> lang <> ".wikipedia/all-access/all-agents/" <> articleName
              <> "/daily/20150101/20160101"
    mres <- (fmap Just (getWith opts (S.toString uri)))
              `catchAny` (\_ -> return Nothing)
    let views = case mres of
            Nothing -> 0
            Just res ->
                sum ((res^.responseBody) ^.. key "items" . _Array . traverse
                       . key "views" . _Number . to round)
    return (lang, views)
  let overallViews = sum (map snd results)
  forM_ (sortBy ((flip compare) `on` snd) results) $ \(l,v) -> do
      let percents = fromIntegral v * 100 / fromIntegral overallViews
      putStrLn (S.toString l <> ": " <> show v <> " (" <> show percents <> "%)")
