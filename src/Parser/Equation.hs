{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections
  #-}

module Parser.Equation where

import AST (ELSE, EQUATION(..), WHENTHEN(..))
import Control.Applicative ((<|>), liftA2)
import Data.Attoparsec.ByteString as BP (Parser, many', string)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Functor (($>))
import Data.Maybe (maybe)
import Parser.Util

type BinomOp = PrecedenceOperator EQUATION

data Return a
  = Next a
  | Bool BinomOp
  | Func

parseEquation :: forall a. Parser a -> Parser (EQUATION, a)
parseEquation nxt = do
  result@(lst, (end, n)) <- run
  pure (reduce end lst, n)
  where
    run :: Parser ([(EQUATION, BinomOp)], (EQUATION, a))
    run = do
      arg <-
        brackets' <|> case' <|>
        (handler =<<
         anyUntilThat
           (fmap Next (whitespace *> nxt <* whitespace) <|>
            ((whitespace *> string "(" <* whitespace) $> Func) <|>
            fmap Bool (whitespace *> parseBoolSymbol <* whitespace)))
      either (return . ([], )) (\a -> fmap (liftA2 (,) ((:) a . fst) snd) run) arg
    handler :: (ByteString, Return a) -> Parser (Either (EQUATION, a) (EQUATION, BinomOp))
    handler (bs, Next nxt) = pure $ Left (VAL bs, nxt)
    handler (bs, Bool pp) = pure $ Right (VAL bs, pp)
    handler (bs, Func) = func' bs
    brackets' = either' parseBrackets nxt parseBoolSymbol
    case' = either' parseCase nxt parseBoolSymbol
    func' bs = either' (parseFunction bs) nxt parseBoolSymbol

-- imprecidative 
-- newtype F = F
--   { u :: forall a. Parser a -> Parser a
--   }
-- asdf :: [F] -> Parser (ByteString, a)
-- asdf lst = anyUntilThat (foldl (<|>) (u . head $ lst) (u <$> tail lst))
-- lab :: [F]
-- lab = undefined
parseBrackets :: Parser a -> Parser (EQUATION, a)
parseBrackets nxt = do
  whitespace *> string "(" <* whitespace
  exp <- fst <$> parseEquation (string ")")
  n <- whitespace *> nxt
  pure (BRACKETS exp, n)

parseFunction :: ByteString -> Parser a -> Parser (EQUATION, a)
parseFunction name nxt = do
  args <- parseArgs
  n <- whitespace *> nxt
  pure (FUNC name args, n)
  where
    parseArgs =
      do arg <- fst <$> parseEquation (string ",")
         fmap ((:) arg) parseArgs
     <|> pure . fst <$> parseEquation (string ")")

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
        b <- fmap Just (eq "ELSE") <|> pure Nothing
        c <- eq "END"
        maybe (return ([WHENTHEN a c], Nothing)) (\b -> return ([WHENTHEN a b], Just c)) b
    word w = whitespace *> anyCaseString w <* whitespace
    eq w = fst <$> (parseEquation . word) w

-- Missing unary operators still
-- '-'
-- 'NOT'
parseBoolSymbol :: Parser BinomOp
parseBoolSymbol =
  (string "<>" $> POP 2 NEQ) <|> (string "!=" $> POP 2 NEQ) <|> (anyCaseString "AND" $> POP 0 AND) <|>
  (anyCaseString "OR" $> POP 0 OR) <|>
  (anyCaseString "IS" $> POP 2 IS) <|>
  (string "<=" $> POP 2 LESSEQ) <|>
  (string ">=" $> POP 2 GREATEQ) <|>
  (string "<" $> POP 2 LESS) <|>
  (string ">" $> POP 2 GREAT) <|>
  (string "=" $> POP 2 EQU) <|>
  (string "+" $> POP 4 PLUS) <|>
  (string "-" $> POP 4 MINUS) <|>
  (string "*" $> POP 6 TIMES) <|>
  (string "/" $> POP 6 DIV)
