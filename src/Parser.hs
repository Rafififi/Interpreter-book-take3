{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}

module Parser where
import CombinatorBase (GenParser (Parser), ErrorType (EndOfInput, Unexpected), Error (..), sepBy, runParser)
import Token (TokenInfo (TokenInfo), TokenType(..))
import AST(Expression(..))
import Control.Applicative

type ParserC = GenParser TokenInfo String

parseTokens :: [TokenInfo] -> [Expression] -> Either [Error TokenInfo String] [Expression]
parseTokens [] acc     = Right $ map desugar (reverse acc)
parseTokens tokens acc = case runParser varDecl tokens (0,0) of
  Right (unParsed, (parsed, _)) -> parseTokens unParsed (parsed : acc)
  Left xs                       -> Left xs


parserBase :: (TokenInfo -> ErrorType TokenInfo String) -> (TokenInfo -> Bool) -> ParserC TokenInfo
parserBase err pred = Parser $ \input (col, row) -> 
  case input of
    [] -> Left [Error col row EndOfInput]
    (x : xs) | pred x -> Right (xs, (x, (col, row)))
             | otherwise -> Left [Error col row $ err x]

getOp :: [TokenType] -> ParserC TokenInfo
getOp tokens = parserBase Unexpected (`elem` map (TokenInfo 0 0) tokens)

tokenAsExpr :: (TokenType -> Bool) -> ParserC Expression
tokenAsExpr matches = Parser $ \input (col, row) -> case input of
    (tok@(TokenInfo col' row' tt) : xs)
      | matches tt -> Right (xs, (Literal tok, (col', row')))
    (tok@(TokenInfo col' row' _) : _) -> Left [Error col' row' (Unexpected tok)]
    [] -> Left [Error col row EndOfInput]

semiColon :: ParserC TokenInfo
semiColon = getOp [SEMICOLON]

varDecl :: ParserC Expression
varDecl = ( Var <$> (getOp [VAR] *> identifier <* getOp [EQUAL]) <*> expressionStatement)
       <|>(Variable <$> (getOp [VAR] *> identifier <* semiColon))
       <|> funDecl
       <|> statement

statement :: ParserC Expression
statement = ifStatement
       <|> retStatement


retStatement :: ParserC Expression
retStatement = Ret <$> (getOp [RETURN] *> optional expression <* semiColon)

funDecl :: ParserC Expression
funDecl = Fun <$> (getOp [FUNCTION] *> identifier) <*> (getOp [LEFTPAREN] *> sepBy (getOp [COMMA]) identifier <* getOp [RIGHTPAREN]) <*> block

ifStatement :: ParserC Expression
ifStatement =  (IfElse <$> (getOp [IF] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> statement <* getOp [ELSE] <*> statement)
           <|> (If <$> (getOp [IF] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> statement)
           <|> printStatement

printStatement :: ParserC Expression
printStatement = Print <$> (getOp [PRINT] *> expression <* semiColon) 
              <|> while

while :: ParserC Expression
while =  While <$> (getOp [WHILE] *> getOp [LEFTPAREN] *> expression <* getOp [RIGHTPAREN]) <*> block
     <|> forStatements

forStatements :: ParserC Expression
forStatements = For <$> (getOp [FOR] *> getOp [LEFTPAREN] *> initializer) 
             <*> optional expression <* semiColon 
             <*> optional expression <* getOp [RIGHTPAREN] 
             <*> block
             <|> block
  where
    initializer = (Nothing <$ semiColon) <|> optional (varDecl <|> expressionStatement)

desugar :: Expression -> Expression
desugar (For Nothing Nothing Nothing body) = Block [While trueLiteral (desugar body)]
desugar (For Nothing Nothing (Just change) (Block body)) = Block [While trueLiteral (desugar $ Block (body <> [change]))]
desugar (For Nothing (Just cond) Nothing body) = Block [While cond (desugar body)]
desugar (For (Just init') Nothing Nothing body) = Block [init', While trueLiteral (desugar body)]
desugar (For Nothing (Just cond) (Just change) (Block body)) = Block [While cond (desugar $ Block (body <> [change]))]
desugar (For (Just init') Nothing (Just change) (Block body)) = Block [init', While trueLiteral (desugar $ Block (body <> [change]))]
desugar (For (Just init') (Just cond) Nothing body) = Block [init', While cond (desugar body)]
desugar (For (Just init') (Just cond) (Just change) (Block body)) = Block [init', While cond (desugar $ Block (body <> [change]))]
desugar (Block xs) = Block (map desugar xs)
desugar x = x

trueLiteral :: Expression
trueLiteral = Literal (TokenInfo 0 0 $ BOOLEAN True)

block :: ParserC Expression
block =  Block <$> (getOp [LEFTBRACE] *> many varDecl <* getOp [RIGHTBRACE]) 
     <|> expressionStatement

expressionStatement :: ParserC Expression
expressionStatement = expression <* semiColon

expression :: ParserC Expression
expression = assignment 

assignment :: ParserC Expression
assignment = (Assign <$> (Variable <$> equality <* getOp [EQUAL]) <*> assignment) 
          <|> parseOr 

parseOr :: ParserC Expression
parseOr = Logical <$> parseAnd <*> getOp [OR] <*> parseAnd
       <|> parseAnd

parseAnd :: ParserC Expression
parseAnd = Logical <$> equality <*> getOp [AND] <*> equality
        <|> equality

equality :: ParserC Expression
equality = binary comparison [BANGEQUAL, EQUALEQUAL]

comparison :: ParserC Expression
comparison  = binary term [GREATEREQUAL, GREATER, LESS, LESSEQUAL]

term :: ParserC Expression
term = binary factor [MINUS, PLUS]

factor :: ParserC Expression
factor = binary unary [SLASH, STAR]

binary :: ParserC Expression -> [TokenType] -> ParserC Expression
binary next tokens = do
  left <- next
  parseRest left
  where
    parseRest left =
      (do
        op <- getOp tokens
        right <- next
        parseRest (Binary left op right))
      <|> pure left

unary :: ParserC Expression
unary = (Unary <$> getOp [BANG, MINUS] <*> unary) 
     <|> call

call :: ParserC Expression
call = Call <$> primary <*> (getOp [LEFTPAREN] *> sepBy (getOp[COMMA]) expression <* getOp [RIGHTPAREN])
    <|> primary

primary :: ParserC Expression
primary = (Literal <$> getOp [BOOLEAN False, BOOLEAN True, NIL]) 
       <|>(Literal <$> getOp [LEFTPAREN]) *> expression <* (Literal <$> getOp [RIGHTPAREN])
       <|> identifier
       <|> literal

identifier :: ParserC Expression
identifier = tokenAsExpr (\case
    IDENTIFIER _ -> True
    _ -> False)

literal :: ParserC Expression
literal = tokenAsExpr (\case
    NUMBER _ -> True
    STRING _ -> True
    _ -> False)
