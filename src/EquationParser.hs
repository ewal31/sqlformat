{-# LANGUAGE OverloadedStrings, ExistentialQuantification,
  ScopedTypeVariables, RankNTypes #-}

module EquationParser where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP (Parser, many', string)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Stack
import Parser

parseEquation :: Parser a -> Parser (EQUATION, a)
parseEquation nxt = run
  where
    run = do
      eqs <- BP.many' parseBoolSymbol
      rest <- liftA2 (,) (VAL . fst) snd <$> anyUntilThat (whitespace *> nxt <* whitespace)
      pure (reduce (fst rest) eqs, snd rest)

parseBoolSymbol :: Parser (EQUATION, PrecendenceParser)
parseBoolSymbol = liftA2 (,) (VAL . fst) snd <$> anyUntilThat (whitespace *> psrs <* whitespace)
  where
    psrs =
      (string "=" $> PParser 0 EQU) <|> (string "+" $> PParser 1 PLUS) <|>
      (string "-" $> PParser 1 MINUS) <|>
      (string "*" $> PParser 2 TIMES) <|>
      (string "/" $> PParser 2 DIV)

parseFunction :: Parser a -> Parser (EQUATION, a)
parseFunction nxt = do
  name <- anyUntilThat $ string "(" <* whitespace
  args <- parseArg
  rest <- nxt
  return (FUNC (fst name) args, rest)
  where
    parseArg :: Parser [EQUATION]
    parseArg =
      do arg <- fst <$> parseEquation (string ",")
         fmap ((:) arg) parseArg
     <|> pure . fst <$> parseEquation (string ")")

data PrecendenceParser = PParser
  { prec :: Int
  , psr :: EQUATION -> EQUATION -> EQUATION
  }

reduce :: EQUATION -> [(EQUATION, PrecendenceParser)] -> EQUATION
reduce end = run stackNew
  where
    run :: Stack (EQUATION, PrecendenceParser) -> [(EQUATION, PrecendenceParser)] -> EQUATION
    run _ [] = end
    run stk [x@(xe, xpp)] =
      case stackIsEmpty stk of
        True -> psr xpp xe end
        False ->
          if prec spp >= prec xpp
            then run stk' [comb s x]
            else unravel stk (psr xpp xe end)
          where top'@(stk', s@(se, spp)) = top stk
    run stk (x@(xe, xpp):y@(ye, ypp):zs) =
      case stackIsEmpty stk of
        True ->
          if prec xpp >= prec ypp
            then run stk (comb x y : zs)
            else run (stackPush stk x) (y : zs)
        False ->
          if prec xpp >= prec ypp
            then run stk' (s : comb x y : zs)
            else run (stackPush stk x) (y : zs)
          where top'@(stk', s) = top stk
    unravel :: Stack (EQUATION, PrecendenceParser) -> EQUATION -> EQUATION
    unravel stk el =
      case stackIsEmpty stk of
        True -> el
        False -> unravel (fst top') (psr (snd . snd $ top') (fst . snd $ top') el)
          where top' = top stk
    comb (xe, xpp) (ye, ypp) = (psr xpp xe ye, ypp)
    top = fromJust . stackPop
