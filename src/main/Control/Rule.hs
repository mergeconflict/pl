{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Rule
  ( Result (..)
  , Rule
  , rule
  , rule'
  , runRule
  ) where

import Control.Applicative
import Control.Arrow
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

newtype Rule l a b = Rule
  { unRule :: WrappedArrow (Kleisli (Result l)) a b
  } deriving (Functor, Applicative, Alternative)

rule :: l -> (a -> Maybe b) -> Rule l a b
rule label f = Rule $ WrapArrow $ Kleisli $ \a -> case f a of
  Just b  -> Success label b
  Nothing -> Failure

rule' :: (Monoid l) => l -> (a -> Result l b) -> Rule l a b
rule' label f = Rule $ WrapArrow $ Kleisli $ \a -> case f a of
  Success label' b -> Success (label' <> label) b
  Failure -> Failure

runRule :: Rule l a b -> a -> Result l b
runRule (Rule (WrapArrow (Kleisli f))) a = f a
