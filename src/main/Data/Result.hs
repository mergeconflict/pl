module Data.Result
  ( Result (..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid

data Result l a = Failure | Success l a
  deriving (Eq, Show)

instance Functor (Result l) where
  fmap _ Failure           = Failure
  fmap f (Success label a) = Success label $ f a

instance (Monoid l) => Monad (Result l) where
  return = Success mempty
  (Success la a) >>= f = case f a of
    (Success lb b) -> Success (la <> lb) b
    Failure        -> Failure
  Failure >>= _ = Failure

instance (Monoid l) => MonadPlus (Result l) where
  mzero = Failure
  Failure `mplus` r = r
  l       `mplus` _ = l

instance (Monoid l) => Applicative (Result l) where
  pure  = return
  (<*>) = ap

instance (Monoid l) => Alternative (Result l) where
  empty = mzero
  (<|>) = mplus
