module AST where

import Data.ByteString (ByteString)

type TableName = ByteString

type ColumnName = ByteString

type FunctionName = ByteString

type Alias = ByteString

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

data COLUMN_EXP =
  COLUMN ColumnName
         (Maybe Alias)
  deriving (Eq, Show)

data WHENTHEN =
  WHENTHEN EQUATION
           EQUATION
  deriving (Eq, Show)

type ELSE = EQUATION

data EQUATION
  = Val ByteString
  | EQ EQUATION
       EQUATION
  | PLUS EQUATION
         EQUATION
  | MINUS EQUATION
          EQUATION
  | DIV EQUATION
        EQUATION
  | TIMES EQUATION
          EQUATION
  | FUNC FunctionName
         [EQUATION]
  | AND EQUATION
        EQUATION
  | OR EQUATION
       EQUATION
  | NOT EQUATION
  | IS EQUATION
       EQUATION
  | LESS EQUATION
         EQUATION
  | GREAT EQUATION
          EQUATION
  | NEQ EQUATION
        EQUATION
  | LESSEQ EQUATION
           EQUATION
  | GREATEQ EQUATION
            EQUATION
  | CASE (Maybe EQUATION)
         [WHENTHEN]
         (Maybe ELSE)
  deriving (Eq, Show)

data WITH_EXP =
  WITH Alias
       SELECT_EXP
  deriving (Eq, Show)

data COLUMNS_EXP =
  COLUMNS (Maybe SELECT_MOD)
          [COLUMN_EXP]
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

data WHERE_EXP
  = WHERE ByteString
  | W_AND ByteString
  deriving (Eq, Show)

newtype GROUP_BY_EXP =
  GROUP_BY ByteString
  deriving (Eq, Show)

data ON_EXP
  = ON ByteString
  | O_AND ByteString
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
  SELECT [WITH_EXP]
         COLUMNS_EXP
         FROM_EXP
         [JOIN_EXP]
         [WHERE_EXP]
         (Maybe GROUP_BY_EXP)
         (Maybe HAVING_EXP)
         (Maybe ORDER_BY_EXP)
         (Maybe LIMIT_EXP)
  deriving (Eq, Show)