{-# LANGUAGE OverloadedStrings, ExistentialQuantification,
  ScopedTypeVariables, RankNTypes, TupleSections #-}

module EquationParser where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP (Parser, many', string)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Stack
import Parser

parseSymbol :: forall a. Parser a -> Parser (EQUATION, a)
parseSymbol nxt =
  (whitespace *> anyCaseString "CASE" *>| parseCase nxt) <|>
  (handler =<<
   anyUntilThat (fmap Left (string "(") <|> fmap Right (whitespace *> nxt <* whitespace)))
  where
    handler :: (ByteString, Either ByteString a) -> Parser (EQUATION, a)
    handler (bs, Left _) = parseFunction bs nxt
    handler (bs, Right nxt) = pure (VAL bs, nxt)

parseBoolSymbol :: Parser PrecendenceParser
parseBoolSymbol =
  (string "=" $> PParser 0 EQU) <|> (string "+" $> PParser 1 PLUS) <|>
  (string "-" $> PParser 1 MINUS) <|>
  (string "*" $> PParser 2 TIMES) <|>
  (string "/" $> PParser 2 DIV)

parseFunction :: ByteString -> Parser a -> Parser (EQUATION, a)
parseFunction name nxt = do
  args <- parseArgs
  n <- nxt
  pure (FUNC name args, n)
  where
    parseArgs =
      do arg <- fst <$> parseEquation (string ",")
         fmap ((:) arg) parseArgs
     <|> pure . fst <$> parseEquation (string ")")

--   | CASE (Maybe EQUATION)
--          [WHENTHEN]
--          (Maybe ELSE)
-- data WHENTHEN =
--
--   WHENTHEN EQUATION
--            EQUATION
--   deriving (Eq, Show)
-- 
-- type ELSE = EQUATION
parseCase :: Parser a -> Parser (EQUATION, a)
parseCase nxt = do
  init <- Just <$> eq "WHEN" <|> (word "WHEN" $> Nothing)
  cs <- uncurry (CASE init) <$> parseStmts
  (cs, ) <$> nxt
  where
    parseStmts :: Parser ([WHENTHEN], Maybe ELSE)
    parseStmts =
      do a <- eq "THEN"
         b <- eq "WHEN"
         fmap (liftA2 (,) ((:) (WHENTHEN a b) . fst) snd) parseStmts
     <|> do
        a <- eq "THEN"
        b <- eq "END"
        return ([WHENTHEN a b], Nothing)
    word w = whitespace *> anyCaseString w <* whitespace
    eq w = fst <$> (parseEquation . word) w

parseEquation :: Parser a -> Parser (EQUATION, a)
parseEquation nxt = run
  where
    run = do
      eqs <- BP.many' $ parseSymbol parseBoolSymbol
      rest <- parseSymbol nxt
      pure (reduce (fst rest) eqs, snd rest)

data PrecendenceParser = PParser
  { prec :: Int
  , psr :: EQUATION -> EQUATION -> EQUATION
  }

reduce :: EQUATION -> [(EQUATION, PrecendenceParser)] -> EQUATION
reduce end = run stackNew
  where
    comb (xe, xpp) (ye, ypp) = (psr xpp xe ye, ypp)
    run :: Stack (EQUATION, PrecendenceParser) -> [(EQUATION, PrecendenceParser)] -> EQUATION
    run _ [] = end
    run stk [x@(xe, xpp)] =
      case stackIsEmpty stk of
        True -> psr xpp xe end
        False ->
          if prec spp >= prec xpp
            then run stk' [comb s x]
            else unravel stk (psr xpp xe end)
          where top@(stk', s@(se, spp)) = fromJust . stackPop $ stk
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
          where top@(stk', s) = fromJust . stackPop $ stk
    unravel :: Stack (EQUATION, PrecendenceParser) -> EQUATION -> EQUATION
    unravel stk el =
      case stackIsEmpty stk of
        True -> el
        False -> unravel (fst top) (psr (snd . snd $ top) (fst . snd $ top) el)
          where top = fromJust . stackPop $ stk
