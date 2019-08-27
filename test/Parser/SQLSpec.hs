{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes,
  FlexibleContexts, MonoLocalBinds #-}

module Parser.SQLSpec
  ( tests
  ) where

import qualified AST.Equation as AE
import qualified AST.SQL as A
import qualified Data.Attoparsec.ByteString as BP
       (Parser, endOfInput, parseOnly, word8)
import qualified Parser.Equation as PE
import qualified Parser.SQL as P
import Test.HUnit

tests =
  TestList
    [ testParseLimitExp
    , testParseOrderByExp
    , testParseHavingExp
    , testParseGroupByExp
    , testParseWhereExp
    , testParseJoinExp
    , testParseFromExp
    , testParseColumnsExp
    , testParseWithExp
    , testParseColumnExp
    , testParseSelectExp
    ]

type PFun a = forall b. BP.Parser b -> BP.Parser (a AE.EQUATION, b)

type PFunM a = forall b. BP.Parser b -> BP.Parser (Maybe (a AE.EQUATION), b)

type PFunL a = forall b. BP.Parser b -> BP.Parser ([a AE.EQUATION], b)

testParseLimitExp =
  "parseLimitExp" ~:
  [ "" ~: Right (Nothing, ()) ~=?
    BP.parseOnly ((P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput) ""
  , "    " ~: Right (Nothing, ()) ~=?
    BP.parseOnly ((P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput) "    "
  , "LIMIT 1000" ~: Right (Just $ A.LIMIT $ AE.VAL "1000", ()) ~=?
    BP.parseOnly ((P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput) "LIMIT 1000"
  , "LIMIT 10 10A BBB" ~: Right (Just $ A.LIMIT $ AE.VAL "10 10A", 66) ~=?
    BP.parseOnly ((P.parseLimitExp :: PFunM A.LIMIT_EXP) $ BP.word8 66) "LIMIT 10 10A BBB"
  , "BBB" ~: Right (Nothing, 66) ~=?
    BP.parseOnly ((P.parseLimitExp :: PFunM A.LIMIT_EXP) $ BP.word8 66) "BBB"
  ]

testParseOrderByExp =
  "parseOrderByExp" ~:
  [ "LIMIT 1000" ~: Right (Nothing, (Just $ A.LIMIT $ AE.VAL "1000", ())) ~=?
    BP.parseOnly
      ((P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "LIMIT 1000"
  , "ORDER BY id ASC B" ~: Right (Just $ A.ORDER_BY $ AE.VAL "id ASC", 66) ~=?
    BP.parseOnly ((P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $ BP.word8 66) "ORDER BY id ASC B"
  , "ORDER BY id ASC" ~: Right (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Nothing, ())) ~=?
    BP.parseOnly
      ((P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "ORDER BY id ASC"
  , "ORDER BY id ASC LIMIT 1000" ~:
    Right (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ())) ~=?
    BP.parseOnly
      ((P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "ORDER BY id ASC LIMIT 1000"
  ]

testParseHavingExp =
  "parseOrderByExp" ~:
  [ "HAVING id = 1" ~: Right (Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1"), ()) ~=?
    BP.parseOnly (P.parseHavingExp BP.endOfInput) "HAVING id = 1"
  , "HAVING id = 1" ~:
    Right (Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1"), (Nothing, (Nothing, ()))) ~=?
    BP.parseOnly
      ((P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "HAVING id = 1"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Just $ A.LIMIT $ AE.VAL "1000", ()))) ~=?
    BP.parseOnly
      ((P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "LIMIT 1000"
  , "ORDER BY id ASC LIMIT 1000" ~:
    Right (Nothing, (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ()))) ~=?
    BP.parseOnly
      ((P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
      , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ()))) ~=?
    BP.parseOnly
      ((P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "HAVING id = 1 LIMIT 1000" ~:
    Right
      ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
      , (Nothing, (Just $ A.LIMIT $ AE.VAL "1000", ()))) ~=?
    BP.parseOnly
      ((P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "HAVING id = 1 LIMIT 1000"
  ]

testParseGroupByExp =
  "parseGroupByExp" ~:
  [ "GROUP BY id, num" ~:
    Right (Just $ A.GROUP_BY $ AE.VAL "id, num", (Nothing, (Nothing, (Nothing, ())))) ~=?
    BP.parseOnly
      ((P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "GROUP BY id, num"
  , "LIMIT 1000" ~: Right (Nothing, (Nothing, (Nothing, (Just $ A.LIMIT $ AE.VAL "1000", ())))) ~=?
    BP.parseOnly
      ((P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "LIMIT 1000"
  , "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( Just $ A.GROUP_BY $ AE.VAL "id, num"
      , ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
        , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ())))) ~=?
    BP.parseOnly
      ((P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseWhereExp =
  "parseWhereExp" ~:
  [ "WHERE id = 2" ~:
    Right
      ( Just $ A.WHERE (AE.EQU (AE.VAL "id") (AE.VAL "2"))
      , (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "WHERE id = 2"
  , "WHERE id = 2 AND a = 3" ~:
    Right
      ( Just $
        A.WHERE (AE.AND (AE.EQU (AE.VAL "id") (AE.VAL "2")) (AE.EQU (AE.VAL "a") (AE.VAL "3")))
      , (Nothing, (Nothing, (Nothing, (Nothing, ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "WHERE id = 2 AND a = 3"
  , "LIMIT 1000" ~:
    Right (Nothing, (Nothing, (Nothing, (Nothing, (Just $ A.LIMIT $ AE.VAL "1000", ()))))) ~=?
    BP.parseOnly
      ((P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "LIMIT 1000"
  , "WHERE id = 2 AND a = 3 AND b = c GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( Just $ A.WHERE (AE.EQU (AE.VAL "id") (AE.VAL "2"))
      , ( Just $ A.GROUP_BY $ AE.VAL "id, num"
        , ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
          , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ()))))) ~=?
    BP.parseOnly
      (P.parseWhereExp $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseJoinExp =
  "parseJoinExp" ~:
  [ "JOIN table" ~: Right ([A.JOIN A.INNER Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "JOIN table"
  , "INNER JOIN table" ~: Right ([A.JOIN A.INNER Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "INNER JOIN table"
  , "LEFT JOIN table" ~: Right ([A.JOIN A.LEFT Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "LEFT JOIN table"
  , "LEFT OUTER JOIN table" ~: Right ([A.JOIN A.LEFT Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "LEFT OUTER JOIN table"
  , "RIGHT JOIN table" ~: Right ([A.JOIN A.RIGHT Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "RIGHT JOIN table"
  , "RIGHT OUTER JOIN table" ~: Right ([A.JOIN A.RIGHT Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "RIGHT OUTER JOIN table"
  , "FULL JOIN table" ~: Right ([A.JOIN A.FULL Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "FULL JOIN table"
  , "FULL OUTER JOIN table" ~: Right ([A.JOIN A.FULL Nothing "table" Nothing], ()) ~=?
    BP.parseOnly ((P.parseJoinExp :: PFunL A.JOIN_EXP) BP.endOfInput) "FULL OUTER JOIN table"
  , "JOIN (SELECT * FROM table1) table2" ~:
    Right
      ( [ A.JOIN
            A.INNER
            (Just $
             A.SELECT
               []
               (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
               (A.FROM Nothing "table1")
               []
               Nothing
               Nothing
               Nothing
               Nothing
               Nothing)
            "table2"
            Nothing
        ]
      , ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN (SELECT * FROM table1) table2"
  , "JOIN table ON a = b" ~:
    Right ([A.JOIN A.INNER Nothing "table" (Just $ AE.EQU (AE.VAL "a") (AE.VAL "b"))], ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table ON a = b"
  , "JOIN table ON a = b AND c = d" ~:
    Right
      ( [ A.JOIN
            A.INNER
            Nothing
            "table"
            (Just $ AE.AND (AE.EQU (AE.VAL "a") (AE.VAL "b")) (AE.EQU (AE.VAL "c") (AE.VAL "d")))
        ]
      , ()) ~=?
    BP.parseOnly (P.parseJoinExp BP.endOfInput) "JOIN table ON a = b AND c = d"
  , "JOIN (SELECT * FROM table1) table2 ON a = b AND c = d" ~:
    Right
      ( [ A.JOIN
            A.INNER
            (Just $
             A.SELECT
               []
               (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
               (A.FROM Nothing "table1")
               []
               Nothing
               Nothing
               Nothing
               Nothing
               Nothing)
            "table2"
            (Just $ AE.AND (AE.EQU (AE.VAL "a") (AE.VAL "b")) (AE.EQU (AE.VAL "c") (AE.VAL "d")))
        ]
      , ()) ~=?
    BP.parseOnly
      (P.parseJoinExp BP.endOfInput)
      "JOIN (SELECT * FROM table1) table2 ON a = b AND c = d"
  , "JOIN table ON a = b AND c = d LEFT JOIN table2 ON apples = oranges" ~:
    Right
      ( [ A.JOIN
            A.INNER
            Nothing
            "table"
            (Just $ AE.AND (AE.EQU (AE.VAL "a") (AE.VAL "b")) (AE.EQU (AE.VAL "c") (AE.VAL "d")))
        , A.JOIN A.LEFT Nothing "table2" (Just $ AE.EQU (AE.VAL "apples") (AE.VAL "oranges"))
        ]
      , ()) ~=?
    BP.parseOnly
      (P.parseJoinExp BP.endOfInput)
      "JOIN table ON a = b AND c = d LEFT JOIN table2 ON apples = oranges"
  ]

testParseFromExp =
  "parseFromExp" ~:
  [ "FROM articles" ~:
    Right (A.FROM Nothing "articles", (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      ((P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "FROM articles"
  , "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.FROM Nothing "articles"
      , ( Just $ A.WHERE $ AE.EQU (AE.VAL "id") (AE.VAL "2")
        , ( Just $ A.GROUP_BY $ AE.VAL "id, num"
          , ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
            , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ())))))) ~=?
    BP.parseOnly
      ((P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "FROM ( SELECT * FROM table ) catalog" ~:
    Right
      ( A.FROM
          (Just $
           A.SELECT
             []
             (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
             (A.FROM Nothing "table")
             []
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing)
          "catalog"
      , (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ())))))) ~=?
    BP.parseOnly
      ((P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "FROM ( SELECT * FROM table ) catalog"
  , "FROM ( SELECT * FROM table ) catalog LEFT JOIN ( SELECT id, apples FROM table ) table2 ON id = 4 AND apples = 1" ~:
    Right
      ( A.FROM
          (Just $
           A.SELECT
             []
             (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
             (A.FROM Nothing "table")
             []
             Nothing
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
                 (A.COLUMNS
                    Nothing
                    [A.COLUMN (AE.VAL "id") Nothing, A.COLUMN (AE.VAL "apples") Nothing])
                 (A.FROM Nothing "table")
                 []
                 Nothing
                 Nothing
                 Nothing
                 Nothing
                 Nothing)
              "table2"
              (Just $
               AE.AND (AE.EQU (AE.VAL "id") (AE.VAL "4")) (AE.EQU (AE.VAL "apples") (AE.VAL "1")))
          ]
        , (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ()))))))) ~=?
    BP.parseOnly
      (P.parseFromExp $
       P.parseJoinExp $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "FROM ( SELECT * FROM table ) catalog LEFT JOIN ( SELECT id, apples FROM table ) table2 ON id = 4 AND apples = 1"
  ]

testParseColumnsExp =
  "parseColumnsExp" ~:
  [ "SELECT * FROM table" ~:
    Right
      ( A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing]
      , (A.FROM Nothing "table", (Nothing, (Nothing, (Nothing, (Nothing, (Nothing, ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $(P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "SELECT * FROM table"
  , "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing]
      , ( A.FROM Nothing "articles"
        , ( Just $ A.WHERE $ AE.EQU (AE.VAL "id") (AE.VAL "2")
          , ( Just $ A.GROUP_BY $ AE.VAL "id, num"
            , ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
              , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $(P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "SELECT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  , "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000" ~:
    Right
      ( A.COLUMNS (Just A.DISTINCT) [A.COLUMN (AE.VAL "*") Nothing]
      , ( A.FROM Nothing "articles"
        , ( Just $ A.WHERE $ AE.EQU (AE.VAL "id") (AE.VAL "2")
          , ( Just $ A.GROUP_BY $ AE.VAL "id, num"
            , ( Just $ A.HAVING $ AE.EQU (AE.VAL "id") (AE.VAL "1")
              , (Just $ A.ORDER_BY $ AE.VAL "id ASC", (Just $ A.LIMIT $ AE.VAL "1000", ()))))))) ~=?
    BP.parseOnly
      (P.parseColumnsExp $(P.parseFromExp :: PFun A.FROM_EXP) $
       (P.parseWhereExp :: PFunM A.WHERE_EXP) $
       (P.parseGroupByExp :: PFunM A.GROUP_BY_EXP) $
       (P.parseHavingExp :: PFunM A.HAVING_EXP) $
       (P.parseOrderByExp :: PFunM A.ORDER_BY_EXP) $
       (P.parseLimitExp :: PFunM A.LIMIT_EXP) BP.endOfInput)
      "SELECT DISTINCT * FROM articles WHERE id = 2 GROUP BY id, num HAVING id = 1 ORDER BY id ASC LIMIT 1000"
  ]

testParseWithExp =
  "parseWithExp" ~:
  [ "SELECT * FROM test" ~:
    Right
      ( []
      , ( A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
            (A.FROM Nothing "test")
            []
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        , ())) ~=?
    BP.parseOnly
      ((P.parseWithExp :: PFunL A.WITH_EXP) $ P.parseSelectExp BP.endOfInput)
      "SELECT * FROM test"
  , "WITH a AS ( SELECT * FROM test )" ~:
    Right
      ( [ A.WITH "a" $
          A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
            (A.FROM Nothing "test")
            []
            Nothing
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
            (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
            (A.FROM Nothing "test")
            []
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        , A.WITH "b" $
          A.SELECT
            []
            (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
            (A.FROM Nothing "test2")
            []
            Nothing
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

testParseColumnExp =
  "parseColumnExp" ~:
  [ "id" ~: Right ([A.COLUMN (AE.VAL "id") Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id"
  , "id, name" ~: Right ([A.COLUMN (AE.VAL "id") Nothing, A.COLUMN (AE.VAL "name") Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id, name"
  , "id AS something, name" ~:
    Right ([A.COLUMN (AE.VAL "id") (Just "something"), A.COLUMN (AE.VAL "name") Nothing], ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id AS something, name"
  , "id AS something, name AS something_else, a.*" ~:
    Right
      ( [ A.COLUMN (AE.VAL "id") (Just "something")
        , A.COLUMN (AE.VAL "name") (Just "something_else")
        , A.COLUMN (AE.VAL "a.*") Nothing
        ]
      , ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "id AS something, name AS something_else, a.*"
  , "a.*, id AS something, name AS something_else" ~:
    Right
      ( [ A.COLUMN (AE.VAL "a.*") Nothing
        , A.COLUMN (AE.VAL "id") (Just "something")
        , A.COLUMN (AE.VAL "name") (Just "something_else")
        ]
      , ()) ~=?
    BP.parseOnly (P.parseColumnExp BP.endOfInput) "a.*, id AS something, name AS something_else"
  ]

testParseSelectExp =
  "parseSelectExp" ~:
  [ "SELECT * FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
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
              (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
              (A.FROM Nothing "test")
              []
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
          ]
          (A.COLUMNS Nothing [A.COLUMN (AE.FUNC "MAX" [AE.VAL "id"]) Nothing])
          (A.FROM Nothing "b")
          []
          Nothing
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
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
          (A.FROM
             (Just $
              A.SELECT
                []
                (A.COLUMNS (Just A.DISTINCT) [A.COLUMN (AE.VAL "*") Nothing])
                (A.FROM Nothing "test")
                []
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             "test")
          []
          Nothing
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
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "a.*") Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
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
          (A.COLUMNS (Just A.DISTINCT) [A.COLUMN (AE.VAL "name") Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
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
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
          Nothing
          Nothing
          Nothing
          (Just $ A.LIMIT $ AE.VAL "1000")
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT * FROM test LIMIT 1000"
  , "SELECT * FROM test WHERE id = 23" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
          (A.FROM Nothing "test")
          []
          (Just $ A.WHERE $ AE.EQU (AE.VAL "id") (AE.VAL "23"))
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
          (A.COLUMNS Nothing [A.COLUMN (AE.VAL "*") Nothing])
          (A.FROM Nothing "test")
          []
          (Just $
           A.WHERE $
           AE.AND (AE.EQU (AE.VAL "id") (AE.VAL "23")) (AE.EQU (AE.VAL "ab") (AE.VAL "ba")))
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
          (A.COLUMNS
             Nothing
             [A.COLUMN (AE.VAL "id") Nothing, A.COLUMN (AE.FUNC "COUNT" [AE.VAL "1"]) Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
          (Just $ A.GROUP_BY $ AE.VAL "id")
          Nothing
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly (P.parseSelectExp BP.endOfInput) "SELECT id, COUNT(1) FROM test GROUP BY id"
  , "SELECT id, MAX(id) AS MAX FROM test" ~:
    Right
      ( A.SELECT
          []
          (A.COLUMNS
             Nothing
             [A.COLUMN (AE.VAL "id") Nothing, A.COLUMN (AE.FUNC "MAX" [AE.VAL "id"]) (Just "MAX")])
          (A.FROM Nothing "test")
          []
          Nothing
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
          (A.COLUMNS
             Nothing
             [A.COLUMN (AE.VAL "name") Nothing, A.COLUMN (AE.FUNC "SUM" [AE.VAL "val"]) Nothing])
          (A.FROM Nothing "test")
          []
          Nothing
          (Just (A.GROUP_BY $ AE.VAL "name"))
          (Just $ A.HAVING $ AE.GREAT (AE.FUNC "COUNT" [AE.VAL "1"]) (AE.VAL " 2"))
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
          (A.COLUMNS
             Nothing
             [A.COLUMN (AE.VAL "name") Nothing, A.COLUMN (AE.FUNC "SUM" [AE.VAL "val"]) Nothing])
          (A.FROM Nothing "test")
          [A.JOIN A.RIGHT Nothing "table2" (Just $ AE.EQU (AE.VAL "name") (AE.VAL "'Wendy'"))]
          Nothing
          (Just (A.GROUP_BY $ AE.VAL "name"))
          (Just $ A.HAVING $ AE.GREAT (AE.FUNC "COUNT" [AE.VAL "1"]) (AE.VAL " 2"))
          Nothing
          Nothing
      , ()) ~=?
    BP.parseOnly
      (P.parseSelectExp BP.endOfInput)
      "SELECT name, SUM(val) FROM test RIGHT OUTER JOIN table2 ON name = 'Wendy' GROUP BY name HAVING COUNT(1) > 2"
  ]
