{-# LANGUAGE OverloadedStrings, TupleSections, AllowAmbiguousTypes,
  TypeFamilies, FlexibleInstances, RankNTypes, UndecidableInstances,
  IncoherentInstances, NoMonomorphismRestriction,
  FunctionalDependencies, FlexibleContexts #-}

module Parser where

import Control.Applicative (Alternative, (<|>), liftA, liftA2)
import Data.Attoparsec.ByteString as BP
import qualified Data.ByteString as BS
       (concat, foldr, head, map, pack, readFile, tail)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Word (Word8)

type family TupleFn ty out where
  TupleFn () output = output
  TupleFn (a, b) output = a -> TupleFn b output

class ApplyFnToTuple a out where
  applyFnToTuple :: a -> TupleFn a out -> out

instance ApplyFnToTuple b out => ApplyFnToTuple (a, b) out where
  applyFnToTuple (a, b) fn = applyFnToTuple b (fn a)

instance ApplyFnToTuple () out where
  applyFnToTuple _ fn = fn

tstapplytuplefn :: String
tstapplytuplefn = applyFnToTuple ("hello", ('y', ('s', ()))) $ \a b c -> a ++ [b, c]

-- https://stackoverflow.com/questions/9656797/variadic-compose-function
class Comp a b c | a b -> c where
  (...) :: a -> b -> c

instance (a ~ c, r ~ b) => Comp (a -> b) c r where
  f ... g = f g

instance (Comp (a -> b) d r1, r ~ (c -> r1)) => Comp (a -> b) (c -> d) r where
  f ... g = \c -> f ... g c

type TableName = ByteString

data ON_EXP
  = ON ByteString
  | AND ByteString
  deriving (Eq, Show)

data SELECT_MOD
  = DISTINCT
  | ALL
  deriving (Eq, Show)

data JOINTYPE
  = INNER
  | LEFT
  | RIGHT
  | FULL
  deriving (Eq, Show)

data COLUMNS_EXP =
  COLUMNS (Maybe SELECT_MOD)
          ByteString
  deriving (Eq, Show)

data FROM_EXP =
  FROM (Maybe SELECT_EXP)
       TableName
  deriving (Eq, Show)

data JOIN_EXP =
  JOIN JOINTYPE
       (Maybe SELECT_EXP)
       TableName
       [ON_EXP]
  deriving (Eq, Show)

newtype WHERE_EXP =
  WHERE ByteString
  deriving (Eq, Show)

newtype GROUP_BY_EXP =
  GROUP_BY ByteString
  deriving (Eq, Show)

newtype HAVING_EXP =
  HAVING ByteString
  deriving (Eq, Show)

newtype ORDER_BY_EXP =
  ORDER_BY ByteString
  deriving (Eq, Show)

newtype LIMIT_EXP =
  LIMIT ByteString
  deriving (Eq, Show)

data SELECT_EXP =
  SELECT COLUMNS_EXP
         FROM_EXP
         (Maybe WHERE_EXP)
         (Maybe GROUP_BY_EXP)
         (Maybe HAVING_EXP)
         (Maybe ORDER_BY_EXP)
         (Maybe LIMIT_EXP)
  deriving (Eq, Show)

parseSelectExp :: Parser a -> Parser (SELECT_EXP, a)
parseSelectExp nxt =
  fmap (`applyFnToTuple` ((,) ... SELECT)) $
  parseColumnsExp $
  parseFromExp $
  parseWhereExp $ parseGroupByExp $ parseHavingExp $ parseOrderByExp $ parseLimitExp $ finally nxt

parseColumnsExp :: Parser a -> Parser (COLUMNS_EXP, a)
parseColumnsExp nxt = do
  anyCaseString "SELECT" <* whitespace
  mod <- parseSelectMod <* whitespace
  fmap (liftA2 (,) (COLUMNS mod . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))
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
parseJoinExp nxt = joins <|> fmap ([], ) (whitespace *> nxt)
  where
    join psr res = psr *>| anyCaseString "JOIN" $> res <* whitespace
    joins = do
      joinType <-
        join (anyCaseString "INNER" <|> pure ()) INNER <|>
        join (anyCaseString "LEFT" *>| (anyCaseString "OUTER" <|> pure ())) LEFT <|>
        join (anyCaseString "RIGHT" *>| (anyCaseString "OUTER" <|> pure ())) RIGHT <|>
        join (anyCaseString "FULL" *>| (anyCaseString "OUTER" <|> pure ())) FULL
      select <- parseSubExp parseSelectExp
      rest <- anyUntilThat (whitespace *> nxt)
      return $ liftA2 (,) ((: []) . flip (JOIN joinType select) [] . BS.pack . fst) snd rest

parseWhereExp :: Parser a -> Parser (Maybe WHERE_EXP, a)
parseWhereExp nxt =
  (anyCaseString "WHERE" *>|
   fmap (liftA2 (,) (Just . WHERE . BS.pack . fst) snd) (anyUntilThat (whitespace *> nxt))) <|>
  fmap (Nothing, ) (whitespace *> nxt)

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
parseSubExp f = fmap (Just . fst) subExp <|> pure Nothing
  where
    subExp = do
      string "(" *> whitespace
      f $ whitespace *> string ")" <* whitespace

parseOnExp :: Parser a -> Parser ([ON_EXP], a)
parseOnExp nxt = parse <|> fmap ([], ) (whitespace *> nxt)
  where
    exp key nxt = anyCaseString key *>| anyUntilThat (whitespace *> nxt)
    and nxt = fmap (liftA2 (,) (AND . BS.pack . fst) snd) $ exp "AND" nxt
    parse = do
      res <- fmap (liftA2 (,) (ON . BS.pack . fst) snd) $ exp "ON" $ and nxt
      --return ([ON . BS.pack . fst $ res], snd res)
      return ([], snd . snd $ res)

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
