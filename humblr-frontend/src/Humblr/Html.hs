{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Html (
  toStandaloneHtml,
  PageOptions (..),
  articlePage,
  articleTable,
  RenderingOptions (..),
  articleCard,
  getSummary,
  nodeToPlainText,
) where

import CMark (commonmarkToNode)
import qualified CMark as CM
import Control.Lens
import Control.Monad (forM_)
import Data.Foldable (fold)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time (TimeZone, defaultTimeLocale, utcToZonedTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics (Generic)
import Humblr.Types
import Lucid

data PageOptions = PageOptions
  { title :: !T.Text
  , siteName :: !T.Text
  }
  deriving (Show, Eq, Ord, Generic)

toStandaloneHtml :: PageOptions -> Html () -> Html ()
toStandaloneHtml opts body = doctypehtml_ do
  head_ do
    meta_ [charset_ "utf-8"]
    title_ $ toHtml opts.title
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@1.0.1/css/bulma.min.css"]
  body_ do
    div_ [class_ "header-content"] do
      section_ [class_ "hero is-light"] $
        div_ [class_ "hero-body"] $
          div_ [class_ "container has-text-centered"] do
            h1_ [class_ "title"] $ toHtml opts.siteName

    div_ [class_ "main-content"] body

articlePage :: RenderingOptions -> Article -> Html ()
articlePage opts Article {..} = do
  div_ [class_ "container block"] $
    div_ [class_ "columns is-multiline is-centered"] $
      div_ [class_ "column is-8"] $ div_ [class_ "box"] do
        div_ [class_ "content-wrapper"] $
          toHtmlRaw $
            CM.nodeToHtml [] $
              transform (rewriteImageLinks opts) $
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
        nodes ^? deep (#_Node . _2 . #_IMAGE)
      articleLink = T.dropWhileEnd (== '/') opts.articleBase <> "/" <> slug
      summary = getSummary nodes
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

instance Plated CM.Node where
  plate :: Traversal' CM.Node CM.Node
  plate = #_Node . _3 . each

getSummary :: CM.Node -> Maybe CM.Node
getSummary nodes = trimImages nodes ^? deep (filtered (\(CM.Node _ ty _) -> ty == CM.PARAGRAPH))

trimImages :: CM.Node -> CM.Node
trimImages = transform \(CM.Node p ty chs) ->
  CM.Node p ty $ filter (not . isEmptyPara) $ filter (not . isImage) chs

isEmptyPara :: CM.Node -> Bool
isEmptyPara (CM.Node _ CM.PARAGRAPH []) = True
isEmptyPara _ = False

isImage :: CM.Node -> Bool
isImage (CM.Node _ CM.IMAGE {} _) = True
isImage (CM.Node _ _ _) = False

nodeToPlainText :: CM.Node -> T.Text
nodeToPlainText = para \cases
  (CM.Node _ CM.DOCUMENT _) ps -> foldMap (<> "\n\n") ps
  (CM.Node _ (CM.TEXT t) _) _ -> t
  (CM.Node _ CM.SOFTBREAK _) _ -> " "
  (CM.Node _ CM.LINEBREAK _) _ -> "\n"
  (CM.Node _ CM.THEMATIC_BREAK _) _ -> "\n"
  (CM.Node _ CM.PARAGRAPH _) ps -> foldMap (<> "\n\n") ps <> "\n"
  (CM.Node _ CM.BLOCK_QUOTE _) ps -> fold ps
  (CM.Node _ CM.HTML_BLOCK {} _) _ -> mempty
  (CM.Node _ CM.HTML_INLINE {} _) _ -> mempty
  (CM.Node _ CM.CUSTOM_BLOCK {} _) _ -> mempty
  (CM.Node _ CM.CUSTOM_INLINE {} _) _ -> mempty
  (CM.Node _ (CM.CODE_BLOCK _ code) _) _ -> code <> "\n\n"
  (CM.Node _ (CM.CODE code) _) _ -> code
  (CM.Node _ (CM.HEADING n) _) ps -> fold (replicate n "#") <> fold ps <> "\n\n"
  (CM.Node _ (CM.LIST _) _) ps -> foldMap ((<> "\n") . ("- " <>)) ps
  (CM.Node _ CM.ITEM _) ps -> foldMap ((<> "\n")) ps
  (CM.Node _ CM.EMPH _) ps -> fold ps
  (CM.Node _ CM.STRONG _) ps -> fold ps
  (CM.Node _ CM.LINK {} _) ps -> fold ps
  (CM.Node _ (CM.IMAGE _ alt) _) _ -> alt
