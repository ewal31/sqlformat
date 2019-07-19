{-# LANGUAGE OverloadedStrings, TupleSections, RankNTypes #-}

module Parser where

import Control.Applicative (Alternative, (<|>), liftA, liftA2)
import Data.Attoparsec.ByteString as BP
import qualified Data.ByteString as BS (foldr, map, pack)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Word (Word8)

parseSubExp :: (forall a. Parser a -> Parser (b, a)) -> Parser (Maybe b)
parseSubExp f = fmap Just (parseSubExp' f) <|> pure Nothing

parseSubExp' :: (forall a. Parser a -> Parser (b, a)) -> Parser b
parseSubExp' f = string "(" *> whitespace *> fmap fst (f $ whitespace *> string ")" <* whitespace)

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

anyUntilThat :: Parser a -> Parser (ByteString, a)
anyUntilThat nxt = liftA2 (,) (BS.pack . fst) snd <$> untilThat anyWord8 nxt
