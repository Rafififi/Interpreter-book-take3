{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Token(TokenType(..), TokenInfo(..), printTokens, showLocation) where

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
  | UNEXPECTED Char
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
  show (UNEXPECTED x) = "ERROR: found unxpected value: " <> show x


data TokenInfo = TokenInfo
  { col :: Int
  , row :: Int
  , token :: TokenType
  }

instance Eq TokenInfo where
  (TokenInfo _ _ token) == (TokenInfo _ _ token') =  token == token'

instance Ord TokenInfo where
  compare (TokenInfo _ _ token) (TokenInfo _ _ token') = compare token token'

instance Show TokenInfo where
  show (TokenInfo _ _ token) = show token
  
showLocation :: TokenInfo -> String
showLocation (TokenInfo r c _) = show r <> " " <> show c


printTokens :: Bool -> TokenInfo -> IO Bool
printTokens _ (TokenInfo row col (UNEXPECTED x)) = print ("found unexpected token " <> show x <> " line " <> show row <> " col: " <> show col) >> return True
printTokens res t = print t >> return res
