{-# LANGUAGE OverloadedStrings, ExistentialQuantification,
  ScopedTypeVariables, RankNTypes #-}

module EquationParser where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP
import Data.ByteString (ByteString)
import Parser

data NParser = NParser
  { name :: ByteString
  , psr :: forall a. [NParser] -> Parser a -> Parser (EQUATION, a)
  }

parsers :: [NParser]
parsers =
  [ NParser "=" parseEqu
  , NParser "-" parseMinus
  , NParser "+" parsePlus
  , NParser "/" parseDiv
  , NParser "*" parseTimes
  ]

parseEquation :: Parser a -> Parser (EQUATION, a)
parseEquation nxt =
  parseEqu parsers nxt <|> parseMinus parsers nxt <|> parsePlus parsers nxt <|> parseDiv parsers nxt <|>
  parseTimes parsers nxt <|>
  parseVal nxt

parseBoolEqu ::
     ByteString
  -> (EQUATION -> EQUATION -> EQUATION)
  -> [NParser]
  -> Parser a
  -> Parser (EQUATION, a)
parseBoolEqu sym dat psrs nxt = do
  first <- fst <$> parseExcept psrs sym (whitespace *> string sym <* whitespace)
  rest <- parseEquation nxt
  return (dat first (fst rest), snd rest)

parseExcept :: forall a. [NParser] -> ByteString -> Parser a -> Parser (EQUATION, a)
parseExcept psrs sym nxt = pNew
  where
    filtered :: [NParser]
    filtered = filter ((/= sym) . name) psrs
    pMap :: [Parser (EQUATION, a)]
    pMap = fmap (\p -> psr p filtered nxt) filtered
    pNew :: Parser (EQUATION, a)
    pNew = foldr (<|>) (parseVal nxt) pMap

parseVal :: Parser a -> Parser (EQUATION, a)
parseVal nxt = liftA2 (,) (VAL . fst) snd <$> anyUntilThat (whitespace *> nxt <* whitespace)

parseEqu :: [NParser] -> Parser a -> Parser (EQUATION, a)
parseEqu = parseBoolEqu "=" EQU

parsePlus :: [NParser] -> Parser a -> Parser (EQUATION, a)
parsePlus = parseBoolEqu "+" PLUS

parseMinus :: [NParser] -> Parser a -> Parser (EQUATION, a)
parseMinus = parseBoolEqu "-" MINUS

parseTimes :: [NParser] -> Parser a -> Parser (EQUATION, a)
parseTimes = parseBoolEqu "*" TIMES

parseDiv :: [NParser] -> Parser a -> Parser (EQUATION, a)
parseDiv = parseBoolEqu "/" DIV
-- parseEquation :: Parser a -> Parser (EQUATION, a)
-- parseEquation nxt = parseEqu nxt <|> parsePlus nxt <|> parseVal nxt
--   -- foldr (<|>) (parseVal nxt) . fmap ($ (whitespace *> nxt <* whitespace)) $ parsers
-- 
-- parseSymbolEqu ::
--      ByteString -> (EQUATION -> EQUATION -> EQUATION) -> Parser a -> Parser (EQUATION, a)
-- parseSymbolEqu sym dat nxt = do
--   first <- fst <$> parseVal (whitespace *> string sym <* whitespace)
--   rest <- parseEquation nxt
--   return (dat first (fst rest), snd rest)
-- 
-- parseVal :: Parser a -> Parser (EQUATION, a)
-- parseVal nxt = liftA2 (,) (VAL . fst) snd <$> anyUntilThat (whitespace *> nxt <* whitespace)
-- 
-- parseEqu :: Parser a -> Parser (EQUATION, a)
-- parseEqu nxt -- parseSymbolEqu "=" EQU
--  = do
--   first <-
--     fst <$>
--     (parsePlus (whitespace *> string "=" <* whitespace) <|>
--      parseVal (whitespace *> string "=" <* whitespace))
--   rest <- parseEquation nxt
--   return (EQU first (fst rest), snd rest)
-- 
-- parsePlus :: Parser a -> Parser (EQUATION, a)
-- parsePlus nxt -- parseSymbolEqu "+" PLUS
--  = do
--   first <-
--     fst <$>
--     (parseEqu (whitespace *> string "+" <* whitespace) <|>
--      parseVal (whitespace *> string "+" <* whitespace))
--   rest <- parseEquation nxt
--   return (PLUS first (fst rest), snd rest)
-- parseFunction :: Parser a -> Parser (EQUATION, a)
-- parseFunction nxt = do
--   name <- anyUntilThat $ string "(" <* whitespace
--   args <- parseArg
--   rest <- nxt
--   return (FUNC (fst name) args, rest)
--   where
--     parseArg :: Parser [EQUATION]
--     parseArg =
--       do arg <- fst <$> parseEquation (string ",")
--          fmap ((:) arg) parseArg
--      <|> pure . fst <$> parseEquation (string ")")
