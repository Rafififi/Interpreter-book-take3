{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Scanner(tokens) where
import Control.Applicative
import Data.Char (isDigit, isAlphaNum)
import Token
import CombinatorBase

type Parser = GenParser Char String


consumeSome  :: (Char -> Bool) -> Parser String
consumeSome  f = some $ satisfy f

stringLiteral :: Parser TokenType
stringLiteral = STRING <$> (char '"' *> many (satisfy (\x -> x /= '"' && x /= '\\') <|> escapeChar) <* char '"') 
  where
    escapeChar = ('"'  <$ string "\\\"")
              <|>('\\' <$ string "\\\\")
              <|>('/'  <$ string "\\/")
              <|>('\b' <$ string "\\b")
              <|>('\f' <$ string "\\f")
              <|>('\r' <$ string "\\r")
              <|>('\t' <$ string "\\t")

double :: Parser TokenType
double = NUMBER <$> (((+) . (fromIntegral :: Int -> Double)
       .   read <$> digits)
      <*> (read . ('0' :) <$> ((:) <$> char '.' <*> digits) <|> pure 0))
  where digits = consumeSome isDigit

comment :: Parser TokenType
comment =  COMMENT <$> (string "//" *> (consumeSome  (/='\n') <|> pure ""))

identifier :: Parser TokenType
identifier =  IDENTIFIER <$> consumeSome  (\x -> isAlphaNum x || x == '_')

singleCharToken :: Parser TokenType
singleCharToken =  (LEFTPAREN  <$ char '(')
               <|> (RIGHTPAREN <$ char ')')
               <|> (LEFTBRACE  <$ char '{')
               <|> (RIGHTBRACE <$ char '}')
               <|> (COMMA      <$ char ',')
               <|> (DOT        <$ char '.')
               <|> (MINUS      <$ char '-')
               <|> (PLUS       <$ char '+')
               <|> (SEMICOLON  <$ char ';')
               <|> (SLASH      <$ char '/')
               <|> (STAR       <$ char '*')
               <|> (BANG       <$ char '!')
               <|> (EQUAL      <$ char '=')
               <|> (GREATER    <$ char '>')
               <|> (LESS       <$ char '<')
               <|> (EOF        <$ char '\0')

doubleCharToken :: Parser TokenType
doubleCharToken =  (BANGEQUAL    <$ string "!=")
               <|> (EQUALEQUAL   <$ string "==")
               <|> (GREATEREQUAL <$ string ">=")
               <|> (LESSEQUAL    <$ string "<=")


keywords :: Parser TokenType
keywords =  (AND     <$ string "and" <* notFollowedByIdentifier)
        <|> (CLASS   <$ string "class" <* notFollowedByIdentifier)
        <|> (ELSE    <$ string "else" <* notFollowedByIdentifier)
        <|> (FUNCTION<$ string "fn" <* notFollowedByIdentifier)
        <|> (FOR     <$ string "for" <* notFollowedByIdentifier)
        <|> (IF      <$ string "if" <* notFollowedByIdentifier)
        <|> (NIL     <$ string "nil" <* notFollowedByIdentifier)
        <|> (OR      <$ string "or" <* notFollowedByIdentifier)
        <|> (PRINT   <$ string "print" <* notFollowedByIdentifier)
        <|> (RETURN  <$ string "return" <* notFollowedByIdentifier)
        <|> (SUPER   <$ string "super" <* notFollowedByIdentifier)
        <|> (THIS    <$ string "this" <* notFollowedByIdentifier)
        <|> (VAR     <$ string "var" <* notFollowedByIdentifier)
        <|> (WHILE   <$ string "while" <* notFollowedByIdentifier)
        <|> (BOOLEAN False <$ string "false" <* notFollowedByIdentifier)
        <|> (BOOLEAN True <$ string "true" <* notFollowedByIdentifier)

notFollowedByIdentifier :: Parser Char
notFollowedByIdentifier = satisfy (\x -> not (isAlphaNum x) && x /= '_' && x /= '-')

escapeChars :: Parser String
escapeChars =  string "\t"
           <|> string "\r"

newLine :: Parser TokenType
newLine = Parser $ \input (col, row) -> 
  case input of 
    [] -> Left [Error col row Empty]
    (x:xs) | x == '\n' -> Right (xs, (STRING "", (0, row+1)))
           | otherwise -> Left [Error col row $ Expected '\n' x]

whiteSpace :: Parser String
whiteSpace =  consumeSome (==' ')

tokens :: Parser [TokenInfo]
tokens = many (discard *> wrapPosition tokenP) <* discard
  where 
    discard = many (escapeChars <|> whiteSpace)
    tokenP =  comment
         <|> stringLiteral 
         <|> doubleCharToken 
         <|> singleCharToken
         <|> double
         <|> keywords 
         <|> identifier
         <|> newLine

wrapPosition :: Parser TokenType -> Parser TokenInfo
wrapPosition p = Parser $ \inp (col, row) -> 
  case runParser p inp (col, row) of 
    Left err -> Left err
    Right (input, (tok, (col, row))) -> 
      Right (input, (TokenInfo col row tok, (col, row)))
