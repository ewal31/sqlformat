{-# LANGUAGE OverloadedStrings #-}

module EquationParser where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP
import Data.ByteString (ByteString)
import Parser

parseFunction :: Parser a -> Parser (EQUATION, a)
parseFunction nxt = do
  name <- anyUntilThat $ string "(" <* whitespace
  args <- parseArg
  rest <- nxt
  return (FUNC (fst name) args, rest)
  where
    parseArg :: Parser [EQUATION]
    parseArg =
      do arg <- VAL . fst <$> anyUntilThat (whitespace *> string "," <* whitespace)
         fmap ((:) arg) parseArg
     <|> pure . VAL . fst <$> anyUntilThat (whitespace *> string ")" <* whitespace)
