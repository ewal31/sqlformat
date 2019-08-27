module AST.SQL where

import Data.ByteString (ByteString)

type TableName = ByteString

type ColumnName = ByteString

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

data COLUMN_EXP a =
  COLUMN a
         (Maybe Alias)
  deriving (Eq, Show)

data WITH_EXP a =
  WITH Alias
       (SELECT_EXP a)
  deriving (Eq, Show)

data COLUMNS_EXP a =
  COLUMNS (Maybe SELECT_MOD)
          [COLUMN_EXP a]
  deriving (Eq, Show)

data FROM_EXP a =
  FROM (Maybe (SELECT_EXP a))
       TableName
  deriving (Eq, Show)

data JOIN_EXP a =
  JOIN JOINTYPE
       (Maybe (SELECT_EXP a))
       TableName
       (Maybe a)
  deriving (Eq, Show)

newtype WHERE_EXP a =
  WHERE a
  deriving (Eq, Show)

newtype GROUP_BY_EXP a =
  GROUP_BY a
  deriving (Eq, Show)

newtype HAVING_EXP a =
  HAVING a
  deriving (Eq, Show)

newtype ORDER_BY_EXP a =
  ORDER_BY a
  deriving (Eq, Show)

newtype LIMIT_EXP a =
  LIMIT a
  deriving (Eq, Show)

data SELECT_EXP a =
  SELECT [WITH_EXP a]
         (COLUMNS_EXP a)
         (FROM_EXP a)
         [JOIN_EXP a]
         (Maybe (WHERE_EXP a))
         (Maybe (GROUP_BY_EXP a))
         (Maybe (HAVING_EXP a))
         (Maybe (ORDER_BY_EXP a))
         (Maybe (LIMIT_EXP a))
  deriving (Eq, Show)
