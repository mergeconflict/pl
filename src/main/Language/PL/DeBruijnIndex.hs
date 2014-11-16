module Language.PL.DeBruijnIndex
  ( DeBruijnIndex (..)
  ) where

newtype DeBruijnIndex = Idx
  { unIdx :: Int
  } deriving (Eq, Show)
