module Language.PL.Type
  ( Type (..)
  ) where

data Type = Boolean
          | Function Type Type
  deriving (Eq, Show)
