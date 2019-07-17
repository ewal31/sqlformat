{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import qualified Data.Attoparsec.ByteString as BP
       (endOfInput, parseOnly, takeByteString, word8)
import qualified Parser as P
import Test.HUnit

tests =
  TestList
    [ testByteStringToUpper
    , testApplyFnToTuple
    , testParseAnyCase
    , testParseLimitExp
    , testParseOrderByExp
    , testParseHavingExp
    , testParseGroupByExp
    , testParseJoinExp
    , testParseFromExp
    , testParseColumnsExp
    , testParseSubExp
    , testParseOnExp
    , testParseSelectExp
    ]

testByteStringToUpper =
  "upperB" ~:
  ["upperB select" ~: "SELECT" ~=? P.upperB "select", "upperB how!" ~: "HOW!" ~=? P.upperB "how!"]

testApplyFnToTuple =
  "applyFnToTuple" ~:
  [ "applyFnToTuple (\"hello\", ('y', ('s', ()))) $ \a b c -> a ++ [b, c]" ~: "helloys" ~=?
    P.applyFnToTuple ("hello", ('y', ('s', ()))) (\a b c -> a ++ [b, c])
  ]

testParseAnyCase =
  "parseAnyCase" ~:
  [ "SELECT in SELECTthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "SELECTthe rest"
  , "SELECT in selectthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "selectthe rest"
  , "SELECT in sELEctthe rest" ~: Right "the rest" ~=?
    BP.parseOnly (P.anyCaseString "SELECT" *> BP.takeByteString) "sELEctthe rest"
  ]

testParseLimitExp =
  "parseLimitExp" ~:
  [ "" ~: Right (Nothing, ()) ~=? BP.parseOnly (P.parseLimitExp BP.endOfInput) ""
  , "    " ~: Right (Nothing, ()) ~=? BP.parseOnly (P.parseLimitExp BP.endOfInput) "    "
  , "LIMIT 1000" ~: Right (Just $ P.LIMIT "1000", ()) ~=?
    BP.parseOnly (P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "LIMIT 10 10A BBB" ~: Right (Just $ P.LIMIT "10 10A", 66) ~=?
    BP.parseOnly (P.parseLimitExp $ BP.word8 66) "LIMIT 10 10A BBB"
  , "BBB" ~: Right (Nothing, 66) ~=? BP.parseOnly (P.parseLimitExp $ BP.word8 66) "BBB"
  ]

testParseOrderByExp =
  "parseOrderByExp" ~:
  [ "LIMIT 1000" ~: Right (Nothing, (Just $ P.LIMIT "1000", ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "ORDER BY id ASC B" ~: Right (Just $ P.ORDER_BY "id ASC", 66) ~=?
    BP.parseOnly (P.parseOrderByExp $ BP.word8 66) "ORDER BY id ASC B"
  , "ORDER BY id ASC" ~: Right (Just $ P.ORDER_BY "id ASC", (Nothing, ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "ORDER BY id ASC"
  , "ORDER BY id ASC LIMIT 1000" ~: Right (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "ORDER BY id ASC LIMIT 1000"
  ]

testParseHavingExp =
  "parseOrderByExp" ~:
  [ "HAVING id = 1" ~: Right (Just $ P.HAVING "id = 1", (Nothing, (Nothing, ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Just $ P.LIMIT "1000", ()))) ~=?
    BP.parseOnly (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "ORDER BY id ASC LIMIT 1000" ~:
    Right (Nothing, (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 LIMIT 1000" ~:
    Right (Just $ P.HAVING "id = 1", (Nothing, (Just $ P.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1 LIMIT 1000"
  ]

testParseGroupByExp =
  "parseGroupByExp" ~:
  [ "GROUP BY id, num" ~: Right (Just $ P.GROUP_BY "id, num", (Nothing, (Nothing, (Nothing, ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "GROUP BY id, num"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Nothing, (Just $ P.LIMIT "1000", ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "LIMIT 1000"
  , "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( Just $ P.GROUP_BY "id, num"
      , (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseWhereExp =
  "parseWhereExp" ~:
  [ "WHERE id = 2" ~: Right ([P.WHERE "id = 2"], (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp' $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2"
  , "WHERE id = 2 AND a = 3" ~:
    Right ([P.WHERE "id = 2", P.W_AND "a = 3"], (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp' $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2 AND a = 3"
  , "LIMIT 1000" ~: Right ([], (Nothing, (Nothing, (Nothing, (Just $ P.LIMIT "1000", ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp' $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "LIMIT 1000"
  , "WHERE id = 2 AND a = 3 AND b = c GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( [P.WHERE "id = 2", P.W_AND "a = 3", P.W_AND "b = c"]
      , ( Just $ P.GROUP_BY "id, num"
        , (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp' $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseJoinExp =
  "parseJoinExp" ~:
  [ "JOIN table" ~: Right ([P.JOIN P.INNER Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table"
  , "INNER JOIN table" ~: Right ([P.JOIN P.INNER Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "INNER JOIN table"
  , "LEFT JOIN table" ~: Right ([P.JOIN P.LEFT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "LEFT JOIN table"
  , "LEFT OUTER JOIN table" ~: Right ([P.JOIN P.LEFT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "LEFT OUTER JOIN table"
  , "RIGHT JOIN table" ~: Right ([P.JOIN P.RIGHT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "RIGHT JOIN table"
  , "RIGHT OUTER JOIN table" ~: Right ([P.JOIN P.RIGHT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "RIGHT OUTER JOIN table"
  , "FULL JOIN table" ~: Right ([P.JOIN P.FULL Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "FULL JOIN table"
  , "FULL OUTER JOIN table" ~: Right ([P.JOIN P.FULL Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "FULL OUTER JOIN table"
  , "JOIN (SELECT * FROM table1) table2" ~:
    Right
      ( [ P.JOIN
            P.INNER
            (Just $
             P.SELECT
               (P.COLUMNS Nothing "*")
               (P.FROM Nothing "table1")
               Nothing
               Nothing
               Nothing
               Nothing
               Nothing)
            "table2"
            []
        ]
      , ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN (SELECT * FROM table1) table2"
  , "JOIN table ON a = b" ~: Right ([P.JOIN P.INNER Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table ON a = b"
  ]

testParseFromExp =
  "parseFromExp" ~:
  [ "FROM articles" ~:
    Right (P.FROM Nothing "articles", (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM articles"
  , "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( P.FROM Nothing "articles"
      , ( Just $ P.WHERE "id = 2"
        , ( Just $ P.GROUP_BY "id, num"
          , (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "FROM ( SELECT * FROM table ) catalog" ~:
    Right
      ( P.FROM
          (Just $
           P.SELECT
             (P.COLUMNS Nothing "*")
             (P.FROM Nothing "table")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing)
          "catalog"
      , (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM ( SELECT * FROM table ) catalog"
  ]

testParseColumnsExp =
  "parseColumnsExp" ~:
  [ "SELECT * FROM table" ~:
    Right
      ( P.COLUMNS Nothing "*"
      , (P.FROM Nothing "table", (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT * FROM table"
  , "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( P.COLUMNS Nothing "*"
      , ( P.FROM Nothing "articles"
        , ( Just $ P.WHERE "id = 2"
          , ( Just $ P.GROUP_BY "id, num"
            , (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( P.COLUMNS (Just P.DISTINCT) "*"
      , ( P.FROM Nothing "articles"
        , ( Just $ P.WHERE "id = 2"
          , ( Just $ P.GROUP_BY "id, num"
            , (Just $ P.HAVING "id = 1", (Just $ P.ORDER_BY "id ASC", (Just $ P.LIMIT "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseSubExp =
  "parseSubExp" ~:
  [ "( SELECT B )" ~: Right (Just $ P.COLUMNS Nothing "B") ~=?
    BP.parseOnly (P.parseSubExp P.parseColumnsExp) "( SELECT B )"
  , "( SELECT B ) something else" ~: Right (Just $ P.COLUMNS Nothing "B") ~=?
    BP.parseOnly (P.parseSubExp P.parseColumnsExp) "( SELECT B ) something else"
  , "something else" ~: Right Nothing ~=?
    BP.parseOnly (P.parseSubExp P.parseColumnsExp) "something else"
  ]

testParseOnExp =
  "parseOnExp" ~:
  [ "ON a = b" ~: Right ([P.ON "a = b"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b"
  , "ON a = b AND c = d" ~: Right ([P.ON "a = b", P.O_AND "c = d"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b AND c = d"
  , "ON a = b AND c = d AND e = f AND g = h" ~:
    Right ([P.ON "a = b", P.O_AND "c = d", P.O_AND "e = f", P.O_AND "g = h"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b AND c = d AND e = f AND g = h"
  , "AND c = d" ~: Left "endOfInput" ~=? BP.parseOnly (P.parseOnExp BP.endOfInput) "AND c = d"
  ]

testParseSelectExp =
  "parseSelectExp" ~:
  [ "SELECT * FROM test" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "*")
          (P.FROM Nothing "test")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test"
  -- , "SELECT * FROM test GROUP BY id WHERE id = 1" ~:
  --  Right
  --    (P.SELECT
  --       (P.COLUMNS Nothing "*")
  --       (P.FROM Nothing "test")
  --       Nothing
  --       Nothing
  --       Nothing
  --       Nothing
  --       Nothing) ~=?
  --  BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test GROUP BY id WHERE id = 1"
  , "SELECT * FROM (SELECT DISTINCT * FROM test) test" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "*")
          (P.FROM
             (Just $
              P.SELECT
                (P.COLUMNS (Just P.DISTINCT) "*")
                (P.FROM Nothing "test")
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             "test")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM (SELECT DISTINCT * FROM test) test"
  , "SELECT a.* FROM test" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "a.*")
          (P.FROM Nothing "test")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT a.* FROM test"
  , "SELECT DISTINCT name FROM test" ~:
    Right
      ( P.SELECT
          (P.COLUMNS (Just P.DISTINCT) "name")
          (P.FROM Nothing "test")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT DISTINCT name FROM test"
  , "SELECT * FROM test LIMIT 1000" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "*")
          (P.FROM Nothing "test")
          Nothing
          Nothing
          Nothing
          Nothing
          (Just $ P.LIMIT "1000")
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test LIMIT 1000"
  , "SELECT * FROM test WHERE id = 23" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "*")
          (P.FROM Nothing "test")
          (Just $ P.WHERE "id = 23")
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test WHERE id = 23"
  , "SELECT id, COUNT(1) FROM test GROUP BY id" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "id, COUNT(1)")
          (P.FROM Nothing "test")
          Nothing
          (Just $ P.GROUP_BY "id")
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT id, COUNT(1) FROM test GROUP BY id"
  , "SELECT id, MAX(id) AS MAX FROM test" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "id, MAX(id) AS MAX")
          (P.FROM Nothing "test")
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT id, MAX(id) AS MAX FROM test"
  , "SELECT name, SUM(val) FROM test GROUP BY name HAVING COUNT(1) > 2" ~:
    Right
      ( P.SELECT
          (P.COLUMNS Nothing "name, SUM(val)")
          (P.FROM Nothing "test")
          Nothing
          (Just $ P.GROUP_BY "name")
          (Just $ P.HAVING "COUNT(1) > 2")
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseSelectExp BP.endOfInput)
      "SELECT name, SUM(val) FROM test GROUP BY name HAVING COUNT(1) > 2"
  ]

main :: IO ()
main = void $ runTestTT tests
