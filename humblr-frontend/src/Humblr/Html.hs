{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Humblr.Html (
  toStandaloneHtml,
  articlePage,
  articleTable,
  RenderingOptions (..),
  articleCard,
) where

import CMark (commonmarkToNode)
import qualified CMark as CM
import Control.Lens
import Control.Monad (forM_)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time (TimeZone, defaultTimeLocale, utcToZonedTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics (Generic)
import Humblr.Types
import Lucid

toStandaloneHtml :: T.Text -> Html () -> Html ()
toStandaloneHtml title body = doctypehtml_ do
  head_ do
    meta_ [charset_ "utf-8"]
    title_ $ toHtml title
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@1.0.1/css/bulma.min.css"]
  body_ body

articlePage :: RenderingOptions -> Article -> Html ()
articlePage opts Article {..} = do
  div_ [class_ "main-content"] $
    div_ [class_ "container"] $
      div_ [class_ "columns is-multiline is-centered"] $
        div_ [class_ "column is-8"] $ div_ [class_ "box"] do
          div_ [class_ "content-wrapper"] $
            toHtmlRaw $
              CM.nodeToHtml [] $
                transformOf gplate (rewriteImageLinks opts) $
                  CM.commonmarkToNode [] body
          div_ [class_ "end-post-details"] do
            div_ [class_ "is-pulled-left"] $ do
              i_ $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ utcToZonedTime opts.timeZone updatedAt
            div_ [class_ "is-pulled-right"] $ forM_ tags \tag -> do
              a_ [href_ $ opts.tagBase <> "/" <> tag] $ "#" <> toHtml tag

rewriteImageLinks :: RenderingOptions -> CM.Node -> CM.Node
rewriteImageLinks opts (CM.Node pos (CM.IMAGE url alt) ns) =
  CM.Node pos (CM.IMAGE (adjustImgSrc opts url) alt) ns
rewriteImageLinks _ n = n

adjustImgSrc :: RenderingOptions -> CM.Url -> CM.Url
adjustImgSrc opts src
  | "http:" `T.isPrefixOf` src || "https:" `T.isPrefixOf` src = src
  | otherwise = T.dropWhileEnd (== '/') opts.imageBase <> "/" <> src

articleTable :: RenderingOptions -> [Article] -> Html ()
articleTable opts arts = div_ [class_ "grid"] do
  forM_ arts $ div_ [class_ "cell"] . articleCard opts

data RenderingOptions = RenderingOptions
  { imageBase, articleBase, tagBase :: !T.Text
  , timeZone :: !TimeZone
  }
  deriving (Show, Eq, Ord, Generic)

articleCard :: RenderingOptions -> Article -> Html ()
articleCard opts Article {..} = div_ [class_ "box"] $ div_ [class_ "card"] do
  let nodes = commonmarkToNode [] body
      imgs =
        nodes ^? gplate . #_Node . _2 . #_IMAGE
      articleLink = T.dropWhileEnd (== '/') opts.articleBase <> "/" <> slug
      summary = trimImages nodes ^? gplate . filtered (\(CM.Node _ ty _) -> ty == CM.PARAGRAPH)
  forM_ imgs $ \(i, alt) -> do
    div_ [class_ "card-image"] do
      figure_ [class_ "image is-4by3"] $
        a_ [href_ articleLink] $
          img_ [src_ $ T.dropWhileEnd (== '/') opts.imageBase <> "/" <> i, alt_ alt]
  div_ [class_ "card-content"] do
    div_ [class_ "content"] do
      forM_ summary $ \(CM.Node pos ty ns) ->
        toHtmlRaw $
          CM.nodeToHtml [] $
            CM.Node pos ty $
              [CM.Node Nothing (CM.LINK articleLink "") ns]
      a_ [href_ articleLink]
        $ time_
          [datetime_ $ T.pack $ iso8601Show createdAt]
        $ toHtml
        $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        $ utcToZonedTime opts.timeZone createdAt

trimImages :: CM.Node -> CM.Node
trimImages = transformOf gplate \(CM.Node p ty chs) ->
  CM.Node p ty $ filter (not . isEmptyPara) $ filter (not . isImage) chs

isEmptyPara :: CM.Node -> Bool
isEmptyPara (CM.Node _ CM.PARAGRAPH []) = True
isEmptyPara _ = False

isImage :: CM.Node -> Bool
isImage (CM.Node _ CM.IMAGE {} _) = True
isImage (CM.Node _ _ _) = False
