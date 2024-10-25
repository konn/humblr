{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Frontend.View (viewModel) where

import Data.Generics.Labels ()
import Data.String (fromString)
import Data.Time (defaultTimeLocale, formatTime)
import Humblr.CMark qualified as CM
import Humblr.Frontend.Actions
import Humblr.Frontend.Types
import Miso
import Miso.String (toMisoString)
import Servant.Auth.Client ()

viewModel :: Model -> View Action
viewModel m@Model {..} =
  section_
    [class_ "section"]
    $ headerView m
      : mainView m
      ++ [footerView]

mainView :: Model -> [View Action]
mainView m = case m.mode of
  Idle ->
    [ div_ [class_ "content"] [progress_ [class_ "progress is-large"] ["Loading..."]]
    ]
  TopPage topPage -> topPageView topPage
  ArticlePage art -> []
  EditingArticle art seed -> []
  CreatingArticle slug edition -> []
  TagArticles tag cur arts -> []
  ErrorPage MkErrorPage {..} ->
    [ h2_ [class_ "title"] [text title]
    , p_ [class_ "content"] [text message]
    ]

topPageView :: TopPage -> [View Action]
topPageView MkTopPage {..} =
  [ h2_
      [class_ "title"]
      [ "Recent Articles ("
      , fromString $ show $ page * 10 + 1
      , "-"
      , fromString $ show $ page * 10 + fromIntegral (length articles)
      , ")"
      ]
  , p_
      [class_ "content"]
      [ div_
          [class_ "fixed-grid has-1-cols-mobile has-3-cols-tablet has-4-cols"]
          [ div_
              [class_ "grid"]
              [div_ [class_ "cell"] (map articleOverview articles)]
          ]
      ]
  ]

articleOverview :: Article -> View Action
articleOverview Article {..} =
  let linkToArticle = a_ [onClick $ openArticle slug]
   in div_
        [class_ "box theme-light"]
        [ article_
            [class_ "media"]
            [ div_
                [class_ "media-content"]
                [ div_
                    [class_ "content"]
                    [ p_ [] [linkToArticle [rawHtml $ CM.commonmarkToHtml [] body]]
                    ]
                , nav_
                    [class_ "level"]
                    [ div_ [class_ "level-left"] []
                    , div_
                        [class_ "level-right"]
                        [linkToArticle [small_ [] [fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" createdAt]]]
                    ]
                ]
            ]
        ]

headerView :: Model -> View Action
headerView _ =
  section_
    [class_ "hero"]
    [ div_
        [class_ "hero-body"]
        [ div_
            [class_ "container has-text-centered"]
            [ h1_ [class_ "title"] [a_ [onClick $ openTopPage Nothing] ["ごはんぶらー"]]
            ]
        ]
    ]

footerView :: View Action
footerView =
  footer_
    [class_ "footer"]
    [ div_
        [class_ "content has-text-centered"]
        [ p_
            []
            [ "Powered by "
            , a_ [href_ "https://haskell.org"] ["Haskell"]
            , ", "
            , a_ [href_ "https://haskell-miso.org"] ["Miso"]
            , ", "
            , a_ [href_ "https://www.cloudflare.com/developer-platform/workers/"] ["Cloudflare Workers"]
            , ", and "
            , a_ [href_ "https://bulma.io"] ["Bulma"]
            , "."
            ]
        , p_ [] ["(c) 2024-present Hiromi ISHII"]
        ]
    ]

{-

toStandaloneHtml :: PageOptions -> Html () -> Html ()
toStandaloneHtml opts body = doctypehtml_ do
  head_ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ toHtml opts.title
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@1.0.1/css/bulma.min.css"]
  body_ do
    div_ [class_ "header-content"] do
      section_ [class_ "hero is-light"] $
        div_ [class_ "hero-body"] $
          div_ [class_ "container has-text-centered"] do
            h1_ [class_ "title"] $ a_ [href_ opts.topPage] $ toHtml opts.siteName

    div_ [class_ "main-content"] body

articlePage :: RenderingOptions -> Article -> Html ()
articlePage opts Article {..} = do
  div_ [class_ "container"] $
    div_ [class_ "columns is-multiline is-centered"] $
      div_ [class_ "column is-8"] $ div_ [class_ "box"] do
        div_ [class_ "content is-large"] $ div_ [class_ "block"] do
          div_ [class_ "block"] $
            toHtmlRaw $
              CM.nodeToHtml [] $
                transform (rewriteImageLinks opts) $
                  CM.commonmarkToNode [] body
          div_ [class_ "level"] do
            div_ [class_ "level-left"] $ do
              p_ $ toHtml $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ utcToZonedTime opts.timeZone updatedAt
            div_ [class_ "level-right"] $ forM_ tags \tag -> do
              p_ $ a_ [class_ "tag", href_ $ opts.tagBase <> "/" <> tag] $ "#" <> toHtml tag

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
 -}
