module Parser where

import Control.Applicative

newtype GenParser i a = Parser
  { runParser :: i -> Maybe (i, a)
  }

instance Functor (GenParser i) where
  fmap f (Parser p) = 
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative (GenParser i) where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
      (input', f)  <- p1 input
      (input'', x) <- p2 input'
      Just (input'', f x)

instance Alternative (GenParser i) where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad (GenParser i) where
  (Parser p) >>= f = Parser $ \input -> do
    (input', x) <- p input
    runParser (f x) input'
    
