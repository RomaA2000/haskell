{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Module : Block3_2
Simple parsers for combination
-}
module Block3_2
  ( -- * The 'ok' parser
    ok,
    -- * The 'eof' parser
    eof,
    -- * The 'satisfy' parser
    satisfy,
    -- * The 'element' parser
    element,
    -- * The 'stream' parser
    stream,
  ) where

import           Block3_1

-- | 'ok' parser, never fails
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | 'eof' parser, fails if input is not empty
eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

-- | 'satisfy' parser, fails if input on function is False
satisfy :: (s -> Bool) -> Parser s s
satisfy f = Parser $ \val -> case val of
    []     -> Nothing
    (x:xs) -> if f x then Just (x, xs) else Nothing

-- | 'element' parser, fails if input is another char
element :: Eq s => s -> Parser s s
element v = satisfy (== v)

-- | 'stream' parser, fails if in input is another char then in stream
stream :: forall s.  Eq s => [s] -> Parser s [s]
stream s = Parser (fmap (\a -> (s, a)) . streamImpl s)
  where
    streamImpl :: [s] -> [s] -> Maybe [s]
    streamImpl (pattern:patterns) (input:inputs) | pattern == input = streamImpl patterns inputs
                                                 | otherwise = Nothing
    streamImpl [] input = Just input
    streamImpl _ _ = Nothing
