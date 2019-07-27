module AST.Equation where

import Data.ByteString (ByteString)

type FunctionName = ByteString

data WHENTHEN =
  WHENTHEN EQUATION
           EQUATION
  deriving (Eq, Show)

type ELSE = EQUATION

data EQUATION
  = VAL ByteString
  | EQU EQUATION -- '='
        EQUATION
  | PLUS EQUATION -- '+'
         EQUATION
  | MINUS EQUATION -- '-'
          EQUATION
  | TIMES EQUATION -- '*'
          EQUATION
  | DIV EQUATION -- '/'
        EQUATION
  | BRACKETS EQUATION -- '(' ')' can also be select expression
  | AND EQUATION -- 'AND'
        EQUATION
  | OR EQUATION -- 'OR'
       EQUATION
  | NOT EQUATION -- 'NOT'
  | IS EQUATION -- 'IS'
       EQUATION
  | LESS EQUATION -- '<'
         EQUATION
  | GREAT EQUATION -- '>'
          EQUATION
  | NEQ EQUATION -- '<>' '!='
        EQUATION
  | LESSEQ EQUATION -- '<='
           EQUATION
  | GREATEQ EQUATION -- '>='
            EQUATION
  | FUNC FunctionName
         [EQUATION]
  | CASE (Maybe EQUATION)
         [WHENTHEN]
         (Maybe ELSE)
  deriving (Eq, Show)
