{-# LANGUAGE InstanceSigs #-}
{-
Module : Block3_1
Data type for 'Parser'
-}
module Block3_1
  ( Parser(..)
  ) where

import           Control.Applicative (Alternative (..))

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  -- | Function 'fmap' applies function to NonEmpty elements
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser ((.) (fmap (\(result, rest) -> (f result, rest))) parser)

instance Applicative (Parser s) where
  -- | Function 'pure' returns Parser of one element
  pure :: a -> Parser s a
  pure a = Parser $ \input -> return (a, input)

  -- | Function '<*>' applies Parser s (a -> b) to Parser s a
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) p1 p2 = Parser (\s -> do
    (f, rest1) <- runParser p1 s
    (a, rest2) <- runParser p2 rest1
    return (f a, rest2))

instance Monad (Parser s) where
  -- | Function '>>=' applies function from a to Parser s b to Parser s a
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser p1) f =
    Parser (\s -> do
      (val, rest) <- p1 s
      let (Parser p2) = f val
      p2 rest)

instance Alternative (Parser s) where
  -- | Function 'empty' returns empty Parser
  empty :: Parser s a
  empty = Parser (const Nothing)

  -- | Function '<|>' applies <|> to result os two Parsers,
  --   returns first non-Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser (\s -> ((<|>) (p1 s) (p2 s)))
