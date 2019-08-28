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
         [COMMENT]
         (COLUMNS_EXP a) -- TODO comments should be handled here
         (FROM_EXP a)
         [COMMENT]
         [JOIN_EXP a]
         [COMMENT]
         (Maybe (WHERE_EXP a))
         [COMMENT]
         (Maybe (GROUP_BY_EXP a))
         [COMMENT]
         (Maybe (HAVING_EXP a))
         [COMMENT]
         (Maybe (ORDER_BY_EXP a))
         [COMMENT]
         (Maybe (LIMIT_EXP a))
  deriving (Eq, Show)

-- /* comment */ anywhere multiline
-- -- comment    until end of line
data COMMENT
  = LINE_COMMENT ByteString
  | BLOCK_COMMENT ByteString
  deriving (Eq, Show)
