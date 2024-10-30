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
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.Frontend.View (viewModel) where

import Control.Lens ((^.))
import Data.Aeson (withObject, (.:))
import Data.Bool (bool)
import Data.Char qualified as C
import Data.Foldable (toList)
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
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
      ++ [modalView v | v <- maybeToList modal]
      ++ [footerView]

modalView :: Modal -> View Action
modalView (Share ShareInfo {title, url}) =
  div_
    [class_ "modal is-active"]
    [ div_ [class_ "modal-background", onClick DismissModal] []
    , div_
        [class_ "modal-content"]
        [ div_
            [class_ "box"]
            [ div_
                [class_ "level"]
                [ div_ [class_ "level-left"] [h3_ [class_ "title level-item"] ["Share"]]
                , div_
                    [class_ "level-right"]
                    [button_ [class_ "button is-primary level-item", onClick $ CopyValueById shareAreaId] [icon "content_copy"]]
                ]
            , div_
                [class_ "field"]
                [ div_
                    [class_ "control"]
                    [ textarea_
                        [class_ "textarea", readonly_ True, id_ shareAreaId]
                        [text $ title <> "\n" <> toMisoString (show url)]
                    ]
                ]
            , div_
                [class_ "field is-grouped is-grouped-right"]
                [ div_
                    [class_ "control"]
                    [button_ [class_ "button is-link", onClick DismissModal] ["Close"]]
                ]
            ]
        ]
    , button_ [class_ "modal-close is-large", P "aria-label" "close", onClick DismissModal] []
    ]

mainView :: Model -> [View Action]
mainView m = case m.mode of
  Idle ->
    [ div_ [class_ "content"] [progress_ [class_ "progress is-large"] ["Loading..."]]
    ]
  TopPage topPage -> topPageView topPage
  AdminPage adminPage -> adminPageView adminPage
  ArticlePage art -> articleView FrontEndArticle art
  EditingArticle edit -> editView edit
  CreatingArticle newArticle -> newArticleView newArticle
  TagArticles tagArticles ->
    articlesList
      ( \cursor ->
          [ "Articles tagged with "
          , span_ [class_ "tag is-large"] [text tagArticles.tag]
          , " "
          ]
            ++ cursor
      )
      tagArticles
  ErrorPage MkErrorPage {..} ->
    [ h2_ [class_ "title"] [text title]
    , p_ [class_ "content"] [text message]
    ]

adminPageView :: AdminPage -> [View Action]
adminPageView =
  articlesList $
    \cursor ->
      [ div_
          [class_ "level"]
          [ div_ [class_ "level-left"] $ "Admin: Recent Articles " : cursor
          , div_
              [class_ "level-right"]
              [button_ [class_ "button is-link", onClick $ openNewArticle] [icon "add"]]
          ]
      ]

newArticleView :: NewArticle -> [View Action]
newArticleView na =
  h2_ [class_ "title"] [text "New Article"]
    : generalEditView na

editView :: EditedArticle -> [View Action]
editView ea@EditedArticle {..} =
  h2_ [class_ "title"] [text "Editing ", code_ [] [text original.slug]]
    : generalEditView ea

generalEditView ::
  forall state.
  (HasEditView state) =>
  state ->
  [View Action]
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
              [ div_ [class_ "field is-horizontal"] $
                  [ div_ [class_ "field-label"] [label_ [class_ "label"] ["Slug"]]
                  , div_
                      [class_ "field-body"]
                      [ div_
                          [class_ "field"]
                          [ div_
                              [class_ "control  has-icons-left"]
                              [ input_ $
                                  [ class_ "input"
                                  , type_ "input"
                                  , onInput $ SetEditedSlug . MS.strip
                                  , id_ slugFieldId
                                  , onCreated $ SetFieldValue slugFieldId $ ea ^. slugG
                                  ]
                              , iconLeft "link"
                              ]
                          ]
                      ]
                  ]
              | DynamicSlug {} <- [slugMode @state]
              ]
                <> editMainView (ea ^. viewStateL) ea
          , div_
              [class_ "level"]
              [ div_
                  [class_ "level-left"]
                  [ div_
                      [class_ "level-item"]
                      [ div_
                          [class_ "field"]
                          [ div_
                              [class_ "control"]
                              [ button_
                                  [class_ "button is-danger", onClick $ DeleteArticle curSlug]
                                  [icon "delete"]
                              ]
                          ]
                      ]
                  ]
              , div_
                  [class_ "level-right"]
                  [ div_
                      [class_ "level-item field is-grouped is-grouped-right"]
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
          [class_ "field is-horizontal"]
          [ div_
              [class_ "field-label"]
              [label_ [class_ "label"] ["Body"]]
          , div_
              [class_ "field-body"]
              [ div_
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
              ]
          ]
      , div_
          [class_ "field is-horizontal"]
          [ div_
              [class_ "field-label"]
              [label_ [class_ "label"] ["Attachments"]]
          , div_
              [class_ "field-body"]
              [ div_
                  [class_ "control file is-info has-name"]
                  [ label_
                      [class_ "file-label"]
                      [ input_
                          [ class_ "file-input"
                          , id_ fileInputId
                          , type_ "file"
                          , accept_ "image/png, image/jpeg"
                          , multiple_ True
                          , onChangeId FileChanged
                          ]
                      , span_
                          [class_ "file-cta"]
                          [ span_ [class_ "file-icon"] [icon "upload_file"]
                          , span_ [class_ "file-label"] ["Choose…"]
                          ]
                      , span_
                          [class_ "file-name"]
                          [ if null (art ^. blobURLsL).urls
                              then "No File"
                              else text $ toMisoString (length (art ^. blobURLsL).urls) <> " Files"
                          ]
                      ]
                  ]
              ]
          ]
      , div_
          [class_ "field is-horizontal"]
          [ div_
              [class_ "field-label"]
              [label_ [class_ "label"] [""]]
          , div_
              [class_ "field-body"]
              [ div_
                  [class_ "control"]
                  [ p_
                      [class_ "content"]
                      [ div_
                          [class_ "grid"]
                          [ div_
                              [ class_ "cell"
                              ]
                              [ figure_
                                  [class_ "image is-128by128"]
                                  [ img_ [src_ img]
                                  , a_
                                      [ class_ "delete is-large"
                                      , onClick (RemoveBlobURL img)
                                      , style_ $ Map.fromList [("position", "absolute"), ("top", "5pt"), ("right", "5pt")]
                                      ]
                                      []
                                  ]
                              ]
                          | img <- F.toList (art ^. blobURLsL).urls
                          ]
                      ]
                  ]
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
              [class_ "field is-horizontal"]
              [ div_ [class_ "field-label"] [label_ [class_ "label"] ["New Tag"]]
              , div_
                  [class_ "field-body"]
                  [ div_
                      [class_ "field has-addons"]
                      [ div_
                          [class_ "control has-icons-left"]
                          [ input_ inputAttrs
                          , iconLeft "sell"
                          ]
                      , div_
                          [class_ "control"]
                          [button_ btnAttrs [icon "add"]]
                      ]
                  ]
              ]
      , div_ [class_ "field is-horizontal"] $
          [ div_ [class_ "field-label"] [label_ [class_ "label"] ["Tags"]]
          , div_
              [class_ "field-body"]
              [ div_
                  [class_ "field is-grouped is-grouped-multiline"]
                  [ div_
                      [class_ "control"]
                      [ div_
                          [class_ "tags has-addons"]
                          [ a_ [class_ "tag"] [text tag]
                          , a_ [class_ "tag is-delete", onClick $ DeleteEditingTag tag] []
                          ]
                      ]
                  | tag <- F.toList $ art ^. tagsL
                  ]
              ]
          ]
      ]
editMainView Preview art =
  [ div_ [class_ "content"]
      $ articleView
        PreviewArticle
      $ currentArticle art
  ]

onChangeId :: (ElementId -> action) -> Attribute action
onChangeId =
  on
    "change"
    Decoder
      { decoder = withObject "target" $ \o -> ElementId <$> o .: "id"
      , decodeAt = DecodeTarget ["target"]
      }

toEditIcon :: EditViewState -> View Action
toEditIcon Edit = icon "edit"
toEditIcon Preview = icon "play_circle"

articleView :: ArticleViewMode -> Article -> [View Action]
articleView mode art@Article {..} =
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
          $ [ div_
                [class_ "content is-size-3"]
                [rawHtml $ CM.commonmarkToHtml [] body]
            ]
            <> [ nav_
                  [class_ "level"]
                  [ div_ [class_ "level-left"] []
                  , div_
                      [class_ "level-right"]
                      [ div_ [class_ "level-item"] [shareButton art]
                      ]
                  ]
               | mode == FrontEndArticle
               ]
            <> [ nav_
                  [class_ "level"]
                  [ div_
                      [class_ "level-left"]
                      [ div_ [class_ "level-item"] [tagsView]
                      ]
                  , div_
                      [class_ "level-right"]
                      [ div_
                          [class_ "level-item"]
                          [linkToArticle [small_ [] [text "Posted ", fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" createdAt]]]
                      ]
                  ]
               ]
      ]

shareButton :: Article -> View Action
shareButton art =
  button_
    [ class_ "level-item button is-rounded is-primary is-small"
    , onClick $ ShareArticle art
    ]
    [iconSmall "share"]

linkToTag :: ArticleViewMode -> MisoString -> [View Action] -> View Action
linkToTag FrontEndArticle tag = a_ [onClick $ openTagArticles tag Nothing]
linkToTag PreviewArticle _ = a_ []

topPageView :: TopPage -> [View Action]
topPageView top = articlesList (\curs -> "Recent Articles " : curs) top

articlesList :: forall arts. (HasArticles arts) => ([View Action] -> [View Action]) -> arts -> [View Action]
articlesList title as =
  let PagedArticles {..} = as ^. articlesL
   in [ h2_
          [class_ "title"]
          $ title
            [ " ("
            , fromString $ show $ page * 10 + 1
            , "-"
            , fromString $ show $ page * 10 + fromIntegral (length articles)
            , ")"
            ]
      , p_
          [class_ "content"]
          [ div_
              [class_ "grid is-col-min-10"]
              $ map (div_ [class_ "cell"] . pure . articleOverview arts)
              $ toList articles
          ]
      ]

articleOverview :: forall arts -> (HasArticles arts) => Article -> View Action
articleOverview arts art@Article {..} =
  let linkToArticle = a_ [onClick $ articleAction arts slug]
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
                        [ div_ [class_ "level-item"] [linkToArticle [small_ [] [fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" createdAt]]]
                        , shareButton art
                        ]
                    ]
                ]
            ]
        ]

headerView :: Model -> View Action
headerView m =
  if isAdminMode m.mode
    then
      section_
        [class_ "hero has-background-light has-text-black-bis"]
        [ div_
            [class_ "hero-body"]
            [ div_
                [class_ "container has-text-centered"]
                [ h1_ [class_ "title"] [a_ [onClick $ openAdminPage Nothing] ["Admin ごはんぶらー"]]
                ]
            ]
        ]
    else
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

iconSmall :: MisoString -> View a
iconSmall name =
  span_
    [class_ "icon is-small mdi"]
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
