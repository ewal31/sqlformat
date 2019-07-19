{-# LANGUAGE OverloadedStrings, TupleSections, AllowAmbiguousTypes,
  TypeFamilies, FlexibleInstances, RankNTypes, UndecidableInstances,
  IncoherentInstances, NoMonomorphismRestriction,
  FunctionalDependencies, FlexibleContexts #-}

module Parser where

import AST
import Control.Applicative (Alternative, (<|>), liftA, liftA2)
import Data.Attoparsec.ByteString as BP
import qualified Data.ByteString as BS
       (concat, foldr, head, map, pack, readFile, tail)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Tuple

parseSelectExp :: Parser a -> Parser (SELECT_EXP, a)
parseSelectExp nxt =
  fmap (`applyFnToTuple` ((,) ... SELECT)) $
  parseWithExp $
  parseColumnsExp $
  parseFromExp $
  parseJoinExp $
  parseWhereExp $ parseGroupByExp $ parseHavingExp $ parseOrderByExp $ parseLimitExp $ finally nxt

parseWithExp :: Parser a -> Parser ([WITH_EXP], a)
parseWithExp nxt = (anyCaseString "WITH" *>| run nxt) <|> fmap ([], ) nxt
  where
    run nxt = do
      name <- BS.pack . fst <$> anyUntilThat (whitespace *> anyCaseString "AS")
      exp <- whitespace *> parseSubExp' parseSelectExp
      fmap
        (liftA2 (,) (liftA2 (:) (pure $ WITH name exp) fst) snd)
        (whitespace *> (string "," *>| run nxt) <|> fmap ([], ) nxt)

parseColumnsExp :: Parser a -> Parser (COLUMNS_EXP, a)
parseColumnsExp nxt = do
  anyCaseString "SELECT" <* whitespace
  mod <- parseSelectMod <* whitespace
  fmap (liftA2 (,) (COLUMNS mod . fst) snd) (parseColumnExp (whitespace *> nxt))
  where
    parseSelectMod =
      (anyCaseString "DISTINCT" $> Just DISTINCT) <|> (anyCaseString "ALL" $> Just ALL) <|>
      pure Nothing

parseFromExp :: Parser a -> Parser (FROM_EXP, a)
parseFromExp nxt = do
  anyCaseString "FROM" *> whitespace
  select <- parseSubExp parseSelectExp
  fmap (liftA2 (,) (FROM select . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))

parseJoinExp :: Parser a -> Parser ([JOIN_EXP], a)
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
          (liftA2 (:) (liftA2 (JOIN joinType select) (BS.pack . fst) (fst . snd)) (fst . snd . snd))
          (snd . snd . snd))
       (whitespace *> anyUntilThat (whitespace *> parseOnExp (parseJoinExp nxt)))
     <|> fmap ([], ) (whitespace *> nxt)
  where
    join psr res = psr *>| anyCaseString "JOIN" $> res <* whitespace

parseWhereExp :: Parser a -> Parser ([WHERE_EXP], a)
parseWhereExp = run "WHERE" WHERE
  where
    run :: ByteString -> (ByteString -> WHERE_EXP) -> Parser a -> Parser ([WHERE_EXP], a)
    run key dat nxt = parseKeyword key dat nxt <|> fmap ([], ) (whitespace *> nxt)
    parseKeyword key dat nxt =
      fmap (liftA2 (,) (liftA2 (:) (dat . BS.pack . fst) (fst . snd)) (snd . snd)) $
      anyCaseString key *>| anyUntilThat (whitespace *> run "AND" W_AND nxt)

parseGroupByExp :: Parser a -> Parser (Maybe GROUP_BY_EXP, a)
parseGroupByExp nxt =
  (anyCaseString "GROUP BY" *>|
   fmap (liftA2 (,) (Just . GROUP_BY . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseHavingExp :: Parser a -> Parser (Maybe HAVING_EXP, a)
parseHavingExp nxt =
  (anyCaseString "HAVING" *>|
   fmap (liftA2 (,) (Just . HAVING . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseOrderByExp :: Parser a -> Parser (Maybe ORDER_BY_EXP, a)
parseOrderByExp nxt =
  (anyCaseString "ORDER BY" *>|
   fmap (liftA2 (,) (Just . ORDER_BY . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseLimitExp :: Parser a -> Parser (Maybe LIMIT_EXP, a)
parseLimitExp nxt =
  (anyCaseString "LIMIT" *>|
   fmap (liftA2 (,) (Just . LIMIT . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

parseSubExp :: (forall a. Parser a -> Parser (b, a)) -> Parser (Maybe b)
parseSubExp f = fmap Just (parseSubExp' f) <|> pure Nothing

parseSubExp' :: (forall a. Parser a -> Parser (b, a)) -> Parser b
parseSubExp' f = string "(" *> whitespace *> fmap fst (f $ whitespace *> string ")" <* whitespace)

parseOnExp :: Parser a -> Parser ([ON_EXP], a)
parseOnExp = run "ON" ON
  where
    run :: ByteString -> (ByteString -> ON_EXP) -> Parser a -> Parser ([ON_EXP], a)
    run key dat nxt = parseKeyword key dat nxt <|> fmap ([], ) (whitespace *> nxt)
    parseKeyword key dat nxt =
      fmap (liftA2 (,) (liftA2 (:) (dat . BS.pack . fst) (fst . snd)) (snd . snd)) $
      anyCaseString key *>| anyUntilThat (whitespace *> run "AND" O_AND nxt)

parseColumnExp :: Parser a -> Parser ([COLUMN_EXP], a)
parseColumnExp nxt =
  fmap
    (liftA2
       (,)
       (liftA2 (:) (liftA2 COLUMN (BS.pack . fst) (fst . snd)) (fst . snd . snd))
       (snd . snd . snd)) $
  anyUntilThat $ whitespace *> as nxt
  where
    as :: Parser a -> Parser (Maybe Alias, ([COLUMN_EXP], a))
    as nxt =
      fmap
        (liftA2 (,) (Just . BS.pack . fst) snd)
        (anyCaseString "AS" *> whitespace *> anyUntilThat (whitespace *> end nxt)) <|>
      fmap (Nothing, ) (whitespace *> end nxt)
    end nxt = (string "," *>| parseColumnExp nxt) <|> fmap ([], ) (whitespace *> nxt)

finally :: Parser a -> Parser (a, ())
finally = (*>) whitespace . fmap (, ())

upper8 :: Word8 -> Word8
upper8 c =
  if c < 123 && c > 96
    then c - 32
    else c

lower8 :: Word8 -> Word8
lower8 c =
  if c < 91 && c > 64
    then c + 32
    else c

upperB :: ByteString -> ByteString
upperB = BS.map upper8

anyCaseString :: ByteString -> Parser ()
anyCaseString = BS.foldr ((*>) . anyCase) (pure ())
  where
    anyCase c = (<|>) (word8 . lower8 $ c) (word8 . upper8 $ c)

whitespace :: Parser ()
whitespace = skipMany (word8 9 <|> word8 10 <|> word8 13 <|> word8 32)

(*>|) :: Parser a -> Parser b -> Parser b
(*>|) a b = a *> whitespace *> b

anyTill :: Parser a -> Parser [Word8]
anyTill next = manyTill anyWord8 (endOfInput <|> next $> ())

untilThat :: Parser a -> Parser b -> Parser ([a], b)
untilThat p nxt = scan
  where
    scan =
      (([], ) <$> nxt) <|> do
        x <- p
        y <- scan
        return (x : fst y, snd y)

anyUntilThat :: Parser a -> Parser ([Word8], a)
anyUntilThat = untilThat anyWord8
