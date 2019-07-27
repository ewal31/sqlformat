{-# LANGUAGE OverloadedStrings, TupleSections, RankNTypes,
  ScopedTypeVariables #-}

module Parser.Util where

import Control.Applicative (Alternative, (<|>), liftA, liftA2)
import Data.Attoparsec.ByteString as BP
import qualified Data.ByteString as BS (foldr, map, pack)
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Stack
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

alpha :: Parser Word8
alpha = foldl1 (<|>) $ fmap word8 $ [65 .. 90] ++ [97 .. 122]

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

anyAlphaUntilThat :: Parser a -> Parser (ByteString, a)
anyAlphaUntilThat nxt = liftA2 (,) (BS.pack . fst) snd <$> untilThat alpha nxt

either' ::
     (forall a. Parser a -> Parser (b, a)) -> Parser c -> Parser d -> Parser (Either (b, c) (b, d))
either' psr a b = fmap Left (psr a) <|> fmap Right (psr b)

data PrecedenceOperator a = POP
  { prec :: Int
  , psr :: a -> a -> a
  }

reduce :: forall a. a -> [(a, PrecedenceOperator a)] -> a
reduce end = run stackNew
  where
    comb (xe, xpp) (ye, ypp) = (psr xpp xe ye, ypp)
    run :: Stack (a, PrecedenceOperator a) -> [(a, PrecedenceOperator a)] -> a
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
    unravel :: Stack (a, PrecedenceOperator a) -> a -> a
    unravel stk el =
      case stackIsEmpty stk of
        True -> el
        False -> unravel (fst top) (psr (snd . snd $ top) (fst . snd $ top) el)
          where top = fromJust . stackPop $ stk
