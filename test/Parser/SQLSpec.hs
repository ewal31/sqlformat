{-# LANGUAGE OverloadedStrings #-}

module Parser.SQLSpec
  ( tests
  ) where

import qualified AST.SQL as A
import qualified Data.Attoparsec.ByteString as BP
       (endOfInput, parseOnly, word8)
import qualified Parser.SQL as P
import Test.HUnit

tests =
  TestList
    [ testParseLimitExp
    , testParseOrderByExp
    , testParseHavingExp
    , testParseGroupByExp
    , testParseJoinExp
    , testParseFromExp
    , testParseColumnsExp
    , testParseWithExp
    , testParseOnExp
    , testParseColumnExp
    , testParseSelectExp
    ]

testParseLimitExp =
  "parseLimitExp" ~:
  [ "" ~: Right (Nothing, ()) ~=? BP.parseOnly (P.parseLimitExp BP.endOfInput) ""
  , "    " ~: Right (Nothing, ()) ~=? BP.parseOnly (P.parseLimitExp BP.endOfInput) "    "
  , "LIMIT 1000" ~: Right (Just $ A.LIMIT "1000", ()) ~=?
    BP.parseOnly (P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "LIMIT 10 10A BBB" ~: Right (Just $ A.LIMIT "10 10A", 66) ~=?
    BP.parseOnly (P.parseLimitExp $ BP.word8 66) "LIMIT 10 10A BBB"
  , "BBB" ~: Right (Nothing, 66) ~=? BP.parseOnly (P.parseLimitExp $ BP.word8 66) "BBB"
  ]

testParseOrderByExp =
  "parseOrderByExp" ~:
  [ "LIMIT 1000" ~: Right (Nothing, (Just $ A.LIMIT "1000", ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "ORDER BY id ASC B" ~: Right (Just $ A.ORDER_BY "id ASC", 66) ~=?
    BP.parseOnly (P.parseOrderByExp $ BP.word8 66) "ORDER BY id ASC B"
  , "ORDER BY id ASC" ~: Right (Just $ A.ORDER_BY "id ASC", (Nothing, ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "ORDER BY id ASC"
  , "ORDER BY id ASC LIMIT 1000" ~: Right (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ())) ~=?
    BP.parseOnly (P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "ORDER BY id ASC LIMIT 1000"
  ]

testParseHavingExp =
  "parseOrderByExp" ~:
  [ "HAVING id = 1" ~: Right (Just $ A.HAVING "id = 1", (Nothing, (Nothing, ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Just $ A.LIMIT "1000", ()))) ~=?
    BP.parseOnly (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput) "LIMIT 1000"
  , "ORDER BY id ASC LIMIT 1000" ~:
    Right (Nothing, (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 LIMIT 1000" ~:
    Right (Just $ A.HAVING "id = 1", (Nothing, (Just $ A.LIMIT "1000", ()))) ~=?
    BP.parseOnly
      (P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "HAVING id = 1 LIMIT 1000"
  ]

testParseGroupByExp =
  "parseGroupByExp" ~:
  [ "GROUP BY id, num" ~: Right (Just $ A.GROUP_BY "id, num", (Nothing, (Nothing, (Nothing, ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "GROUP BY id, num"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Nothing, (Just $ A.LIMIT "1000", ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "LIMIT 1000"
  , "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( Just $ A.GROUP_BY "id, num"
      , (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ())))) ~=?
    BP.parseOnly
      (P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseWhereExp =
  "parseWhereExp" ~:
  [ "WHERE id = 2" ~: Right ([A.WHERE "id = 2"], (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2"
  , "WHERE id = 2 AND a = 3" ~:
    Right ([A.WHERE "id = 2", A.W_AND "a = 3"], (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2 AND a = 3"
  , "LIMIT 1000" ~: Right ([], (Nothing, (Nothing, (Nothing, (Just $ A.LIMIT "1000", ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "LIMIT 1000"
  , "WHERE id = 2 AND a = 3 AND b = c GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( [A.WHERE "id = 2", A.W_AND "a = 3", A.W_AND "b = c"]
      , ( Just $ A.GROUP_BY "id, num"
        , (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseJoinExp =
  "parseJoinExp" ~:
  [ "JOIN table" ~: Right ([A.JOIN A.INNER Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table"
  , "INNER JOIN table" ~: Right ([A.JOIN A.INNER Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "INNER JOIN table"
  , "LEFT JOIN table" ~: Right ([A.JOIN A.LEFT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "LEFT JOIN table"
  , "LEFT OUTER JOIN table" ~: Right ([A.JOIN A.LEFT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "LEFT OUTER JOIN table"
  , "RIGHT JOIN table" ~: Right ([A.JOIN A.RIGHT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "RIGHT JOIN table"
  , "RIGHT OUTER JOIN table" ~: Right ([A.JOIN A.RIGHT Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "RIGHT OUTER JOIN table"
  , "FULL JOIN table" ~: Right ([A.JOIN A.FULL Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "FULL JOIN table"
  , "FULL OUTER JOIN table" ~: Right ([A.JOIN A.FULL Nothing "table" []], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "FULL OUTER JOIN table"
  , "JOIN (SELECT * FROM table1) table2" ~:
    Right
      ( [ A.JOIN
            A.INNER
            (Just $
             A.SELECT
               []
               (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
               (A.FROM Nothing "table1")
               []
               []
               Nothing
               Nothing
               Nothing
               Nothing)
            "table2"
            []
        ]
      , ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN (SELECT * FROM table1) table2"
  , "JOIN table ON a = b" ~: Right ([A.JOIN A.INNER Nothing "table" [A.ON "a = b"]], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table ON a = b"
  , "JOIN table ON a = b AND c = d" ~:
    Right ([A.JOIN A.INNER Nothing "table" [A.ON "a = b", A.O_AND "c = d"]], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table ON a = b AND c = d"
  , "JOIN (SELECT * FROM table1) table2 ON a = b AND c = d" ~:
    Right
      ( [ A.JOIN
            A.INNER
            (Just $
             A.SELECT
               []
               (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
               (A.FROM Nothing "table1")
               []
               []
               Nothing
               Nothing
               Nothing
               Nothing)
            "table2"
            [A.ON "a = b", A.O_AND "c = d"]
        ]
      , ()) ~=?
    BP.parseOnly
      (P.parseJoinExp BP.endOfInput)
      "JOIN (SELECT * FROM table1) table2 ON a = b AND c = d"
  , "JOIN table ON a = b AND c = d LEFT JOIN table2 ON apples = oranges" ~:
    Right
      ( [ A.JOIN A.INNER Nothing "table" [A.ON "a = b", A.O_AND "c = d"]
        , A.JOIN A.LEFT Nothing "table2" [A.ON "apples = oranges"]
        ]
      , ()) ~=?
    BP.parseOnly
      (P.parseJoinExp BP.endOfInput)
      "JOIN table ON a = b AND c = d LEFT JOIN table2 ON apples = oranges"
  ]

testParseFromExp =
  "parseFromExp" ~:
  [ "FROM articles" ~:
    Right (A.FROM Nothing "articles", ([], (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM articles"
  , "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.FROM Nothing "articles"
      , ( [A.WHERE "id = 2"]
        , ( Just $ A.GROUP_BY "id, num"
          , (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "FROM ( SELECT * FROM table ) catalog" ~:
    Right
      ( A.FROM
          (Just $
           A.SELECT
             []
             (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
             (A.FROM Nothing "table")
             []
             []
             Nothing
             Nothing
             Nothing
             Nothing)
          "catalog"
      , ([], (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM ( SELECT * FROM table ) catalog"
  , "FROM ( SELECT * FROM table ) catalog LEFT JOIN ( SELECT id, apples FROM table ) table2 ON id = 4 AND apples = 1" ~:
    Right
      ( A.FROM
          (Just $
           A.SELECT
             []
             (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
             (A.FROM Nothing "table")
             []
             []
             Nothing
             Nothing
             Nothing
             Nothing)
          "catalog"
      , ( [ A.JOIN
              A.LEFT
              (Just $
               A.SELECT
                 []
                 (A.COLUMNS Nothing [A.COLUMN "id" Nothing, A.COLUMN "apples" Nothing])
                 (A.FROM Nothing "table")
                 []
                 []
                 Nothing
                 Nothing
                 Nothing
                 Nothing)
              "table2"
              [A.ON "id = 4", A.O_AND "apples = 1"]
          ]
        , ([], (Nothing, (Nothing, (Nothing, (Nothing, ()))))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseJoinExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "FROM ( SELECT * FROM table ) catalog LEFT JOIN ( SELECT id, apples FROM table ) table2 ON id = 4 AND apples = 1"
  ]

testParseColumnsExp =
  "parseColumnsExp" ~:
  [ "SELECT * FROM table" ~:
    Right
      ( A.COLUMNS Nothing [A.COLUMN "*" Nothing]
      , (A.FROM Nothing "table", ([], (Nothing, (Nothing, (Nothing, (Nothing, ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT * FROM table"
  , "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.COLUMNS Nothing [A.COLUMN "*" Nothing]
      , ( A.FROM Nothing "articles"
        , ( [A.WHERE "id = 2"]
          , ( Just $ A.GROUP_BY "id, num"
            , (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.COLUMNS (Just A.DISTINCT) [A.COLUMN "*" Nothing]
      , ( A.FROM Nothing "articles"
        , ( [A.WHERE "id = 2"]
          , ( Just $ A.GROUP_BY "id, num"
            , (Just $ A.HAVING "id = 1", (Just $ A.ORDER_BY "id ASC", (Just $ A.LIMIT "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $
       P.parseFromExp $
       P.parseWhereExp $
       P.parseGroupByExp $ P.parseHavingExp $ P.parseOrderByExp $ P.parseLimitExp BP.endOfInput)
      "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseWithExp =
  "parseWithExp" ~:
  [ "SELECT * FROM test" ~:
    Right
      ( []
      , ( A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
            (A.FROM Nothing "test")
            []
            []
            Nothing
            Nothing
            Nothing
            Nothing
        , ())) ~=?
    BP.parseOnly (P.parseWithExp $ P.parseSelectExp $ BP.endOfInput) "SELECT * FROM test"
  , "WITH a AS ( SELECT * FROM test )" ~:
    Right
      ( [ A.WITH "a" $
          A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
            (A.FROM Nothing "test")
            []
            []
            Nothing
            Nothing
            Nothing
            Nothing
        ]
      , ()) ~=?
    BP.parseOnly (P.parseWithExp BP.endOfInput) "WITH a AS ( SELECT * FROM test )"
  , "WITH a AS ( SELECT * FROM test ), b AS ( SELECT * FROM test2 )" ~:
    Right
      ( [ A.WITH "a" $
          A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
            (A.FROM Nothing "test")
            []
            []
            Nothing
            Nothing
            Nothing
            Nothing
        , A.WITH "b" $
          A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
            (A.FROM Nothing "test2")
            []
            []
            Nothing
            Nothing
            Nothing
            Nothing
        ]
      , ()) ~=?
    BP.parseOnly
      (P.parseWithExp BP.endOfInput)
      "WITH a AS ( SELECT * FROM test ), b AS ( SELECT * FROM test2 )"
  ]

testParseOnExp =
  "parseOnExp" ~:
  [ "ON a = b" ~: Right ([A.ON "a = b"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b"
  , "ON a = b AND c = d" ~: Right ([A.ON "a = b", A.O_AND "c = d"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b AND c = d"
  , "ON a = b AND c = d AND e = f AND g = h" ~:
    Right ([A.ON "a = b", A.O_AND "c = d", A.O_AND "e = f", A.O_AND "g = h"], ()) ~=?
    BP.parseOnly (P.parseOnExp BP.endOfInput) "ON a = b AND c = d AND e = f AND g = h"
  , "AND c = d" ~: Left "endOfInput" ~=? BP.parseOnly (P.parseOnExp BP.endOfInput) "AND c = d"
  ]

testParseColumnExp =
  "parseColumnExp" ~:
  [ "id" ~: Right ([A.COLUMN "id" Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id"
  , "id, name" ~: Right ([A.COLUMN "id" Nothing, A.COLUMN "name" Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id, name"
  , "id AS something, name" ~:
    Right ([A.COLUMN "id" (Just "something"), A.COLUMN "name" Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id AS something, name"
  , "id AS something, name AS something_else, a.*" ~:
    Right
      ( [ A.COLUMN "id" (Just "something")
        , A.COLUMN "name" (Just "something_else")
        , A.COLUMN "a.*" Nothing
        ]
      , ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id AS something, name AS something_else, a.*"
  ]

testParseSelectExp =
  "parseSelectExp" ~:
  [ "SELECT * FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
          (A.FROM Nothing "test")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test"
  , "WITH b AS (SELECT * FROM test) SELECT MAX(id) FROM b" ~:
    Right
      ( A.SELECT
          [ A.WITH "b" $
            A.SELECT
              []
              (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
              (A.FROM Nothing "test")
              []
              []
              Nothing
              Nothing
              Nothing
              Nothing
          ]
          (A.COLUMNS Nothing [A.COLUMN "MAX(id)" Nothing])
          (A.FROM Nothing "b")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseSelectExp BP.endOfInput)
      "WITH b AS (SELECT * FROM test) SELECT MAX(id) FROM b"
  , "SELECT * FROM (SELECT DISTINCT * FROM test) test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
          (A.FROM
             (Just $
              A.SELECT
                []
                (A.COLUMNS (Just A.DISTINCT) [A.COLUMN "*" Nothing])
                (A.FROM Nothing "test")
                []
                []
                Nothing
                Nothing
                Nothing
                Nothing)
             "test")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM (SELECT DISTINCT * FROM test) test"
  , "SELECT a.* FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "a.*" Nothing])
          (A.FROM Nothing "test")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT a.* FROM test"
  , "SELECT DISTINCT name FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS (Just A.DISTINCT) [A.COLUMN "name" Nothing])
          (A.FROM Nothing "test")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT DISTINCT name FROM test"
  , "SELECT * FROM test LIMIT 1000" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
          (A.FROM Nothing "test")
          []
          []
          Nothing
          Nothing
          Nothing
          (Just $ A.LIMIT "1000")
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test LIMIT 1000"
  , "SELECT * FROM test WHERE id = 23" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
          (A.FROM Nothing "test")
          []
          [A.WHERE "id = 23"]
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test WHERE id = 23"
  , "SELECT * FROM test WHERE id = 23 AND ab = ba" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "*" Nothing])
          (A.FROM Nothing "test")
          []
          [A.WHERE "id = 23", A.W_AND "ab = ba"]
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test WHERE id = 23 AND ab = ba"
  , "SELECT id, COUNT(1) FROM test GROUP BY id" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "id" Nothing, A.COLUMN "COUNT(1)" Nothing])
          (A.FROM Nothing "test")
          []
          []
          (Just $ A.GROUP_BY "id")
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT id, COUNT(1) FROM test GROUP BY id"
  , "SELECT id, MAX(id) AS MAX FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "id" Nothing, A.COLUMN "MAX(id)" (Just "MAX")])
          (A.FROM Nothing "test")
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT id, MAX(id) AS MAX FROM test"
  , "SELECT name, SUM(val) FROM test GROUP BY name HAVING COUNT(1) > 2" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "name" Nothing, A.COLUMN "SUM(val)" Nothing])
          (A.FROM Nothing "test")
          []
          []
          (Just $ A.GROUP_BY "name")
          (Just $ A.HAVING "COUNT(1) > 2")
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseSelectExp BP.endOfInput)
      "SELECT name, SUM(val) FROM test GROUP BY name HAVING COUNT(1) > 2"
  , "SELECT name, SUM(val) FROM test RIGHT OUTER JOIN table2 ON name = 'Wendy' GROUP BY name HAVING COUNT(1) > 2" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN "name" Nothing, A.COLUMN "SUM(val)" Nothing])
          (A.FROM Nothing "test")
          [A.JOIN A.RIGHT Nothing "table2" [A.ON "name = 'Wendy'"]]
          []
          (Just $ A.GROUP_BY "name")
          (Just $ A.HAVING "COUNT(1) > 2")
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseSelectExp BP.endOfInput)
      "SELECT name, SUM(val) FROM test RIGHT OUTER JOIN table2 ON name = 'Wendy' GROUP BY name HAVING COUNT(1) > 2"
  ]
