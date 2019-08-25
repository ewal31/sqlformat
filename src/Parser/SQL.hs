{-# LANGUAGE OverloadedStrings, TupleSections, RankNTypes,
  ScopedTypeVariables #-}

module Parser.SQL where

import AST.SQL
import Control.Applicative (Alternative, (<|>), liftA, liftA2)
import Data.Attoparsec.ByteString as BP
import qualified Data.ByteString as BS
       (append, concat, foldr, head, map, pack, readFile, tail)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Parser.Util
import Tuple

class SubParser b where
  parseSub :: forall a. Parser a -> Parser (b, a)

parseSelectExp :: (SubParser b) => Parser a -> Parser (SELECT_EXP b, a)
parseSelectExp nxt =
  fmap (`applyFnToTuple` ((,) ... SELECT)) $
  parseWithExp $
  parseColumnsExp $
  parseFromExp $
  parseJoinExp $
  parseWhereExp $ parseGroupByExp $ parseHavingExp $ parseOrderByExp $ parseLimitExp $ finally nxt

parseWithExp :: (SubParser b) => Parser a -> Parser ([WITH_EXP b], a)
parseWithExp nxt = (anyCaseString "WITH" *>| run nxt) <|> fmap ([], ) nxt
  where
    run nxt = do
      name <- fst <$> anyUntilThat (whitespace *> anyCaseString "AS")
      exp <- whitespace *> parseSubExp' parseSelectExp
      fmap
        (liftA2 (,) (liftA2 (:) (pure $ WITH name exp) fst) snd)
        (whitespace *> (string "," *>| run nxt) <|> fmap ([], ) nxt)

parseColumnsExp :: (SubParser b) => Parser a -> Parser (COLUMNS_EXP b, a)
parseColumnsExp nxt = do
  anyCaseString "SELECT" <* whitespace
  mod <- parseSelectMod <* whitespace
  fmap (liftA2 (,) (COLUMNS mod . fst) snd) (parseColumnExp (whitespace *> nxt))
  where
    parseSelectMod =
      (anyCaseString "DISTINCT" $> Just DISTINCT) <|> (anyCaseString "ALL" $> Just ALL) <|>
      pure Nothing

parseFromExp :: (SubParser b) => Parser a -> Parser (FROM_EXP b, a)
parseFromExp nxt = do
  anyCaseString "FROM" *> whitespace
  select <- parseSubExp parseSelectExp
  fmap (liftA2 (,) (FROM select . fst) snd) (anyUntilThat (whitespace *> nxt))

parseJoinExp :: (SubParser b) => Parser a -> Parser ([JOIN_EXP b], a)
parseJoinExp nxt =
  do joinType <-
       join (anyCaseString "INNER" <|> pure ()) INNER <|>
       join (anyCaseString "LEFT" *>| (anyCaseString "OUTER" <|> pure ())) LEFT <|>
       join (anyCaseString "RIGHT" *>| (anyCaseString "OUTER" <|> pure ())) RIGHT <|>
       join (anyCaseString "FULL" *>| (anyCaseString "OUTER" <|> pure ())) FULL
     select <- parseSubExp parseSelectExp
     fmap
       (liftA2
          (,)
          (liftA2 (:) (liftA2 (JOIN joinType select) fst (fst . snd)) (fst . snd . snd))
          (snd . snd . snd))
       (whitespace *> anyUntilThat (whitespace *> maybeEqu (parseJoinExp nxt)))
     <|> fmap ([], ) (whitespace *> nxt)
  where
    join psr res = psr *>| anyCaseString "JOIN" $> res <* whitespace
    maybeEqu nxt =
      fmap (liftA2 (,) (Just . fst) snd) (anyCaseString "ON" *>| parseSub nxt) <|>
      fmap (Nothing, ) nxt

parseWhereExp :: (SubParser b) => Parser a -> Parser (Maybe (WHERE_EXP b), a)
parseWhereExp nxt = (anyCaseString "WHERE" *> whitespace *> whereExp) <|> fmap (Nothing, ) nxt
  where
    whereExp = do
      (wh, n) <- parseSub nxt
      return (Just . WHERE $ wh, n)

parseGroupByExp :: Parser a -> Parser (Maybe GROUP_BY_EXP, a)
parseGroupByExp nxt =
  (anyCaseString "GROUP BY" *>|
   fmap (liftA2 (,) (Just . GROUP_BY . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseHavingExp :: (SubParser b) => Parser a -> Parser (Maybe (HAVING_EXP b), a)
parseHavingExp nxt =
  (anyCaseString "HAVING" *>|
   fmap (liftA2 (,) (Just . HAVING . fst) snd) (parseSub (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseOrderByExp :: Parser a -> Parser (Maybe ORDER_BY_EXP, a)
parseOrderByExp nxt =
  (anyCaseString "ORDER BY" *>|
   fmap (liftA2 (,) (Just . ORDER_BY . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseLimitExp :: Parser a -> Parser (Maybe LIMIT_EXP, a)
parseLimitExp nxt =
  (anyCaseString "LIMIT" *>|
   fmap (liftA2 (,) (Just . LIMIT . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseColumnExp :: (SubParser b) => Parser a -> Parser ([COLUMN_EXP b], a)
parseColumnExp nxt =
  fmap
    (liftA2 (,) (liftA2 (:) (liftA2 COLUMN fst (fst . snd)) (fst . snd . snd)) (snd . snd . snd))
    -- (all (as nxt) <|> parseEquation (whitespace *> as nxt))
    (parseSub (whitespace *> as nxt))
  where
    as :: (SubParser b) => Parser a -> Parser (Maybe Alias, ([COLUMN_EXP b], a))
    as nxt =
      fmap
        (liftA2 (,) (Just . fst) snd)
        (anyCaseString "AS" *> whitespace *> anyUntilThat (whitespace *> end nxt)) <|>
      fmap (Nothing, ) (whitespace *> end nxt)
    end nxt = (string "," *>| parseColumnExp nxt) <|> fmap ([], ) (whitespace *> nxt)
    -- all nxt = do
    --   whitespace
    --   name <- (liftA2 BS.append fst snd <$> anyAlphaUntilThat (string ".*")) <|> string "*"
    --   whitespace
    --   (mkEquation name, ) <$> nxt
