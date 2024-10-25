{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Humblr.CMark (
  module CMark,
  getSummary,
  trimImages,
  isEmptyPara,
  isImage,
  nodeToPlainText,
) where

import CMark
import Control.Lens
import Data.Foldable (fold)
import Data.Generics.Labels ()
import Data.Text qualified as T

instance Plated Node where
  plate :: Traversal' Node Node
  plate = #_Node . _3 . each

getSummary :: Node -> Maybe Node
getSummary nodes = trimImages nodes ^? deep (filtered (\(Node _ ty _) -> ty == PARAGRAPH))

trimImages :: Node -> Node
trimImages = transform \(Node p ty chs) ->
  Node p ty $ filter (not . isEmptyPara) $ filter (not . isImage) chs

isEmptyPara :: Node -> Bool
isEmptyPara (Node _ PARAGRAPH []) = True
isEmptyPara _ = False

isImage :: Node -> Bool
isImage (Node _ IMAGE {} _) = True
isImage (Node _ _ _) = False

nodeToPlainText :: Node -> T.Text
nodeToPlainText = para \cases
  (Node _ DOCUMENT _) ps -> foldMap (<> "\n\n") ps
  (Node _ (TEXT t) _) _ -> t
  (Node _ SOFTBREAK _) _ -> " "
  (Node _ LINEBREAK _) _ -> "\n"
  (Node _ THEMATIC_BREAK _) _ -> "\n"
  (Node _ PARAGRAPH _) ps -> foldMap (<> "\n\n") ps <> "\n"
  (Node _ BLOCK_QUOTE _) ps -> fold ps
  (Node _ HTML_BLOCK {} _) _ -> mempty
  (Node _ HTML_INLINE {} _) _ -> mempty
  (Node _ CUSTOM_BLOCK {} _) _ -> mempty
  (Node _ CUSTOM_INLINE {} _) _ -> mempty
  (Node _ (CODE_BLOCK _ code) _) _ -> code <> "\n\n"
  (Node _ (CODE code) _) _ -> code
  (Node _ (HEADING n) _) ps -> fold (replicate n "#") <> fold ps <> "\n\n"
  (Node _ (LIST _) _) ps -> foldMap ((<> "\n") . ("- " <>)) ps
  (Node _ ITEM _) ps -> foldMap ((<> "\n")) ps
  (Node _ EMPH _) ps -> fold ps
  (Node _ STRONG _) ps -> fold ps
  (Node _ LINK {} _) ps -> fold ps
  (Node _ (IMAGE _ alt) _) _ -> alt