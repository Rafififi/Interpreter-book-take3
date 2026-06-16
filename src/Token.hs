module Token(TokenType(..), TokenInfo(..)) where

data TokenType = 
    LEFTPAREN
  | RIGHTPAREN
  | LEFTBRACE
  | RIGHTBRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANGEQUAL
  | EQUAL
  | EQUALEQUAL
  | GREATER
  | GREATEREQUAL
  | LESS
  | LESSEQUAL
  | AND
  | CLASS
  | ELSE
  | FUNCTION
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | VAR
  | WHILE
  | EOF
  | BOOLEAN Bool
  | COMMENT String
  | IDENTIFIER String
  | STRING String 
  | NUMBER Double
  deriving (Eq, Ord)

instance Show TokenType where
  show LEFTPAREN = "("
  show RIGHTPAREN = ")"
  show LEFTBRACE = "{"
  show RIGHTBRACE = "}"
  show COMMA = ","
  show DOT = "."
  show MINUS = "-"
  show PLUS = "+"
  show SEMICOLON = ";"
  show SLASH = "/"
  show STAR = "*"
  show BANG = "!"
  show BANGEQUAL = "!="
  show EQUAL = "="
  show EQUALEQUAL = "=="
  show GREATER = ">"
  show GREATEREQUAL = ">="
  show LESS = "<"
  show LESSEQUAL = "<="
  show AND = "and"
  show CLASS = "class"
  show ELSE = "else"
  show FUNCTION = "fun"
  show FOR = "for"
  show IF = "if"
  show NIL = "nil"
  show OR = "or"
  show PRINT = "print"
  show RETURN = "return"
  show SUPER = "super"
  show THIS = "this"
  show VAR = "var"
  show WHILE = "while"
  show EOF = "EOF"
  show (BOOLEAN False) = "false"
  show (BOOLEAN True) = "true"
  show (COMMENT x) = "//" <> x
  show (IDENTIFIER x) = x
  show (STRING x) = x 
  show (NUMBER x) = show x


data TokenInfo = TokenInfo
  { col :: Int
  , row :: Int
  , token :: TokenType
  } deriving (Show, Eq)
