{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CombinatorBase where
import Control.Applicative
import Data.List (nub)

data Error inp err = Error Int Int (ErrorType inp err) 
  deriving (Eq)

data ErrorType inp err
  = EndOfInput
  | Unexpected inp
  | Expected inp inp
  | CustomError err
  | ExpectedEof inp
  | Empty
  | ExpectedString [inp] [inp]
  deriving (Eq, Ord)

newtype GenParser inp err out = Parser
  { runParser :: [inp] -> (Int, Int) -> Either [Error inp err] ([inp], (out, (Int, Int)))
  }

instance Functor (GenParser inp err) where
  fmap f (Parser p) = Parser $ \input pos ->
      case p input pos of
        Left err -> Left err
        Right (input', (x, pos')) -> Right (input', (f x, pos'))

instance Applicative (GenParser inp err) where
  pure x = Parser $ \input pos -> Right (input, (x, pos))
  (Parser p1) <*> (Parser p2) =
    Parser $ \input pos -> do
      (input',  (f, pos'))  <- p1 input pos
      (input'', (x, pos'')) <- p2 input' pos'
      pure (input'', (f x, pos''))

instance (Eq inp, Eq err) => Alternative (GenParser inp err) where
  empty = Parser $ \_ (col, row)-> Left [Error col row Empty]
  (Parser p1) <|> (Parser p2) = Parser $ \input pos
    -> case p1 input pos of 
      Right (input', x) -> Right (input', x)
      Left err          -> case p2 input pos of
        Right (input', x) -> Right (input', x)
        Left err'         -> Left $ nub $ err <> err'

instance Monad (GenParser inp err) where
  (Parser p) >>= f = Parser $ \input col -> do
    (input', (x, pos)) <- p input col
    runParser (f x) input' pos

token :: (inp -> ErrorType inp err) -> (inp -> Bool) -> GenParser inp err inp
token err pred = Parser $ \input (col, row) -> 
  case input of
    [] -> Left [Error col row EndOfInput]
    (x : xs) | pred x -> Right (xs, (x, (col + 1, row)))
             | otherwise -> Left [Error col row $ err x]

satisfy :: (inp -> Bool) -> GenParser inp err inp
satisfy = token Unexpected 

is :: Eq inp => inp -> GenParser inp err inp
is inp = satisfy (==inp)

char :: Eq inp => inp -> GenParser inp err inp
char inp = token (Expected inp) (==inp)

string :: (Eq inp, Show inp) => [inp] -> GenParser inp err [inp]
string expected = Parser $ \input (col, row) -> 
  case runParser (traverse char expected) input (col, row) of
    Right res -> Right res
    Left _    -> Left [Error col row (ExpectedString expected (take (length expected)input))]

sepBy :: (Alternative f) => f a -> f sep -> f [a]
sepBy p sep = (:) <$> (many sep *> p) <*> many (some sep *> p) <|> pure []


instance (Show inp, Show err) => Show (ErrorType inp err) where
  show EndOfInput = "no more input" 
  show (Expected x y) = "Expected " <> show x <> " got " <> show y 
  show (ExpectedString x y) = "Expected " <> show x <> " got " <> show y
  show (Unexpected x) = "Got Unexpected: " <> show x 
  show (CustomError x) = show x 
  show (ExpectedEof x) = "Expected EOF got " <> show x 
  show Empty = "AHHH" 

instance (Show inp, Show err) => Show (Error inp err) where
  show (Error col row err) = show err <> " At Column " <> show col <> " & line " <> show row

