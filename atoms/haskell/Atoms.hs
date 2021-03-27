#!/usr/bin/env stack
{- stack
  script
  --resolver lts-17.4
  --package bifunctors,containers,megaparsec
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Atoms where

import Data.Bifunctor (second)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser =
  Parsec Void String

main :: IO ()
main = do
  getArgs >>= \case
    formula : _ | not $ null formula -> do
      putStrLn $ "Parsing formula '" <> formula <> "'"

      putStrLn $ case parse pComps "" formula of
        Left err ->
          errorBundlePretty err
        Right res ->
          "Atom counts: " <> displayCounts res
    _ ->
      putStrLn "No input formula provided..."

pComps :: Parser (Map String Int)
pComps =
  L.foldl' merge M.empty <$> some (pSingle <|> pGroup)

pGroup :: Parser (Map String Int)
pGroup = do
  comps <- single '(' *> pComps <* single ')'
  num <- pNum

  pure $ (* num) <$> comps

pSingle :: Parser (Map String Int)
pSingle =
  M.singleton <$> pAtom <*> pNum

pAtom :: Parser String
pAtom =
  (:) <$> upperChar <*> many lowerChar

pNum :: Parser Int
pNum =
  optional decimal >>= \case
    Nothing ->
      pure 1
    Just n
      | n > 1 ->
        pure n
    _ ->
      fail "Explicit count must be at least 2"

displayCounts :: Map String Int -> String
displayCounts =
  ("{ " <>)
    . (<> " }")
    . L.intercalate ", "
    . fmap displayCount
    . M.toList

displayCount :: (String, Int) -> String
displayCount (atom, count) =
  atom <> " : " <> show count

merge :: (Ord k, Num a) => Map k a -> Map k a -> Map k a
merge =
  M.merge
    M.preserveMissing
    M.preserveMissing
    (M.zipWithMatched $ const (+))
