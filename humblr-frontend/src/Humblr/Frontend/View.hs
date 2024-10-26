{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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

import Control.Lens ((^.))
import Data.Bool (bool)
import Data.Char qualified as C
import Data.Foldable (toList)
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (fromString)
import Data.Time (defaultTimeLocale, formatTime)
import GHC.IsList qualified as G
import Humblr.CMark qualified as CM
import Humblr.Frontend.Actions
import Humblr.Frontend.Types
import Miso hiding (view)
import Miso.String (MisoString, toMisoString)
import Miso.String qualified as MS
import Servant.Auth.Client ()

viewModel :: Model -> View Action
viewModel m@Model {..} =
  div_ [] $
    headerView m
      : section_ [class_ "section"] (mainView m)
      : [ div_
            [class_ "notification is-danger", style_ $ G.fromList [("position", "absolute"), ("bottom", "12pt")]]
            [ button_ [class_ "delete", onClick DismissError] []
            , h3_ [class_ "subtitle"] [text title]
            , text message
            ]
        | MkErrorMessage {..} <- maybeToList errorMessage
        ]
      ++ [footerView]

mainView :: Model -> [View Action]
mainView m = case m.mode of
  Idle ->
    [ div_ [class_ "content"] [progress_ [class_ "progress is-large"] ["Loading..."]]
    ]
  TopPage topPage -> topPageView topPage
  ArticlePage art -> articleView FrontEndArticle art
  EditingArticle edit -> editView edit
  CreatingArticle newArticle -> newArticleView newArticle
  TagArticles tagArticles ->
    articlesList
      [ "Articles tagged with "
      , span_ [class_ "tag is-large"] [text tagArticles.tag]
      ]
      tagArticles.articles
  ErrorPage MkErrorPage {..} ->
    [ h2_ [class_ "title"] [text title]
    , p_ [class_ "content"] [text message]
    ]

newArticleView :: NewArticle -> [View Action]
newArticleView na =
  h2_ [class_ "title"] [text "New Article"]
    : generalEditView na

editView :: EditedArticle -> [View Action]
editView ea@EditedArticle {..} =
  h2_ [class_ "title"] [text "Editing ", code_ [] [text original.slug]]
    : generalEditView ea

generalEditView :: forall state. (HasEditView state) => state -> [View Action]
generalEditView ea =
  let curSlug = ea ^. slugG
      isValidArticle =
        not (MS.null curSlug)
          && MS.isAscii curSlug
          && MS.all (\c -> C.isAlphaNum c || c == '-' || c == '_') curSlug
          && not (MS.null $ ea ^. bodyL)
   in [ div_
          [class_ "content"]
          [ div_
              [class_ "tabs"]
              [ ul_
                  []
                  [ li_
                      attrs
                      [ a_ linkAtts [toEditIcon mode, text (toMisoString $ show mode)]
                      ]
                  | mode <- [Edit, Preview]
                  , let isActive = mode == ea ^. viewStateL
                        (attrs, linkAtts) =
                          if isActive
                            then ([class_ "is-active"], [])
                            else ([], [onClick $ SwitchEditViewState mode])
                  ]
              ]
          , div_ [class_ "box"] $
              [ div_ [class_ "field is-grouped"] $
                  [ label_ [class_ "label"] ["Slug"]
                  , div_
                      [class_ "control  has-icons-left"]
                      [ input_
                          [ class_ "input"
                          , type_ "input"
                          , onInput $ SetEditedSlug . MS.strip
                          ]
                      , iconLeft "link"
                      ]
                  ]
              | DynamicSlug {} <- [slugMode @state]
              ]
                <> editMainView (ea ^. viewStateL) ea
          , div_
              [class_ "field is-grouped is-grouped-right"]
              [ -- TODO: Confirm before cancel
                div_
                  [class_ "control"]
                  [ button_
                      [ class_ "button is-light"
                      , onClick $ openArticle $ ea ^. slugG
                      ]
                      ["Cancel"]
                  ]
              , div_
                  [class_ "control"]
                  [ button_
                      ( if isValidArticle
                          then
                            [ class_ "button is-primary"
                            , onClick $ saveAction state
                            ]
                          else [class_ "button is-disabled", disabled_ True]
                      )
                      ["Submit"]
                  ]
              ]
          ]
      ]

data ArticleViewMode = PreviewArticle | FrontEndArticle
  deriving (Show, Eq)

editMainView :: (HasEditView ea) => EditViewState -> ea -> [View Action]
editMainView Edit art =
  let newTag = MS.strip $ art ^. newTagL
      validTagName =
        not (MS.null newTag)
          && MS.all (not . C.isSpace) newTag
   in [ div_
          [class_ "field"]
          [ div_
              [class_ "control"]
              [ textarea_
                  [ class_ "textarea is-large"
                  , rows_ "5"
                  , onInput SetEditingArticleContent
                  ]
                  [text $ art ^. bodyL]
              ]
          ]
      , let btnCls =
              class_ $
                MS.unwords $
                  "button"
                    : if validTagName
                      then ["is-link"]
                      else ["is-link is-light is-disabled"]
            btnAction =
              if validTagName
                then onClick AddEditingTag
                else disabled_ True
            btnAttrs = [btnCls, btnAction, onInput $ SetNewTagName . MS.strip]
            inputAttrs =
              class_ "input"
                : id_ newTagInputId
                : onChange (SetNewTagName . MS.strip)
                : onInput (SetNewTagName . MS.strip)
                : [ onEnter AddEditingTag
                  | validTagName
                  ]
         in div_
              [class_ "field has-addons"]
              [ label_ [class_ "label"] ["New Tag"]
              , div_
                  [class_ "control has-icons-left"]
                  [ input_ inputAttrs
                  , iconLeft "sell"
                  ]
              , div_
                  [class_ "control"]
                  [button_ btnAttrs [icon "add"]]
              ]
      , div_ [class_ "field is-grouped is-grouped-multiline"] $
          [ div_
              [class_ "control"]
              [ div_
                  [class_ "tags has-addons"]
                  [ a_ [class_ "tag"] [text tag]
                  , a_
                      [ class_ "tag is-delete"
                      , onClick $ DeleteEditingTag tag
                      ]
                      []
                  ]
              ]
          | tag <- F.toList $ art ^. tagsL
          ]
      ]
editMainView Preview art =
  [ div_ [class_ "content"]
      $ articleView
        PreviewArticle
      $ currentArticle art
  ]

toEditIcon :: EditViewState -> View Action
toEditIcon Edit = icon "edit"
toEditIcon Preview = icon "play_circle"

articleView :: ArticleViewMode -> Article -> [View Action]
articleView mode Article {..} =
  let linkToArticle =
        case mode of
          FrontEndArticle -> a_ [onClick $ openArticle slug]
          PreviewArticle -> a_ []
      tagsView =
        div_ [class_ "field is-grouped is-grouped-multiline"] $
          [ div_
              [class_ "control"]
              [ div_
                  [class_ "tags"]
                  [ span_ [class_ "tag"] [linkToTag mode tag [text tag]]
                  ]
              ]
          | tag <- tags
          ]
   in [ div_
          [class_ "box is-four-fifth"]
          [ div_
              [class_ "content is-size-3"]
              [rawHtml $ CM.commonmarkToHtml [] body]
          , nav_
              [class_ "level"]
              [ div_
                  [class_ "level-left"]
                  [ tagsView
                  ]
              , div_
                  [class_ "level-right"]
                  [linkToArticle [small_ [] [text "Posted ", fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" createdAt]]]
              ]
          ]
      ]

linkToTag :: ArticleViewMode -> MisoString -> [View Action] -> View Action
linkToTag FrontEndArticle tag = a_ [onClick $ openTagArticles tag Nothing]
linkToTag PreviewArticle _ = a_ []

topPageView :: TopPage -> [View Action]
topPageView MkTopPage {..} = articlesList ["Recent Articles"] articles

articlesList :: [View Action] -> PagedArticles -> [View Action]
articlesList title PagedArticles {..} =
  [ h2_
      [class_ "title"]
      $ title
        <> [ " ("
           , fromString $ show $ page * 10 + 1
           , "-"
           , fromString $ show $ page * 10 + fromIntegral (length articles)
           , ")"
           ]
  , p_
      [class_ "content"]
      [ div_
          [class_ "grid"]
          $ map (div_ [class_ "cell"] . pure . articleOverview)
          $ toList articles
      ]
  ]

articleOverview :: Article -> View Action
articleOverview Article {..} =
  let linkToArticle = a_ [onClick $ openArticle slug]
      nodes = CM.commonmarkToNode [] body
   in div_
        [class_ "box theme-light"]
        [ article_
            [class_ "media"]
            [ div_
                [class_ "media-content"]
                [ div_
                    [class_ "content is-size-3"]
                    [ p_ [] [linkToArticle [text $ CM.nodeToPlainText $ fromMaybe nodes $ CM.getSummary nodes]]
                    ]
                , nav_
                    [class_ "level"]
                    [ div_
                        [class_ "level-left"]
                        [ div_
                            [class_ "tags are-normal"]
                            [span_ [class_ "tag"] [linkToTag FrontEndArticle tag [text tag]] | tag <- tags]
                        ]
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

icon :: MisoString -> View a
icon name =
  span_
    [class_ "icon"]
    [ span_
        [ class_ "material-symbols-outlined"
        ]
        [text name]
    ]

iconLeft :: MisoString -> View a
iconLeft name =
  span_
    [class_ "icon is-left"]
    [ span_
        [ class_ "material-symbols-outlined"
        ]
        [text name]
    ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)

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
