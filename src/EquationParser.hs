{-# LANGUAGE OverloadedStrings, ExistentialQuantification,
  ScopedTypeVariables, RankNTypes, TupleSections #-}

module EquationParser where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP (Parser, many', string)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Stack
import Parser

data Return a
  = Next a
  | Bool PrecendenceParser
  | Func

parseEquation :: forall a. Parser a -> Parser (EQUATION, a)
parseEquation nxt = do
  result@(lst, (end, n)) <- run
  pure (reduce end lst, n)
  where
    run :: Parser ([(EQUATION, PrecendenceParser)], (EQUATION, a))
    run = do
      arg <-
        brackets <|> case' <|>
        (handler =<<
         anyUntilThat
           (fmap Next (whitespace *> nxt <* whitespace) <|> ((string "(" <* whitespace) $> Func) <|>
            fmap Bool (whitespace *> parseBoolSymbol <* whitespace)))
      either (return . ([], )) (\a -> fmap (liftA2 (,) ((:) a . fst) snd) run) arg
    handler :: (ByteString, Return a) -> Parser (Either (EQUATION, a) (EQUATION, PrecendenceParser))
    handler (bs, Next nxt) = pure $ Left (VAL bs, nxt)
    handler (bs, Bool pp) = pure $ Right (VAL bs, pp)
    handler (bs, Func) = func' bs
    brackets = either' parseBrackets nxt parseBoolSymbol
    case' = either' parseCase nxt parseBoolSymbol
    func' :: ByteString -> Parser (Either (EQUATION, a) (EQUATION, PrecendenceParser))
    func' bs = either' (parseFunction bs) nxt parseBoolSymbol

either' ::
     (forall a. Parser a -> Parser (b, a)) -> Parser c -> Parser d -> Parser (Either (b, c) (b, d))
either' psr a b = fmap Left (psr a) <|> fmap Right (psr b)

-- TODO make a type to wrap this in 
parseBrackets :: Parser a -> Parser (EQUATION, a)
parseBrackets nxt = do
  whitespace *> string "(" <* whitespace
  exp <- fst <$> parseEquation (string ")")
  n <- whitespace *> nxt
  pure (exp, n)

parseFunction :: ByteString -> Parser a -> Parser (EQUATION, a)
parseFunction name nxt
  -- string "(" <* whitespace
 = do
  args <- parseArgs
  n <- whitespace *> nxt
  pure (FUNC name args, n)
  where
    parseArgs =
      do arg <- fst <$> parseEquation (string ",")
         fmap ((:) arg) parseArgs
     <|> pure . fst <$> parseEquation (string ")")

-- TODO else
parseCase :: Parser a -> Parser (EQUATION, a)
parseCase nxt = do
  whitespace *> anyCaseString "CASE" <* whitespace
  init <- (word "WHEN" $> Nothing) <|> (Just <$> eq "WHEN")
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

parseBoolSymbol :: Parser PrecendenceParser
parseBoolSymbol =
  (string "=" $> PParser 0 EQU) <|> (string "+" $> PParser 1 PLUS) <|>
  (string "-" $> PParser 1 MINUS) <|>
  (string "*" $> PParser 2 TIMES) <|>
  (string "/" $> PParser 2 DIV)

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
