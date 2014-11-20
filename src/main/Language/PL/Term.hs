module Language.PL.Term
  ( DeBruijnIndex (..)
  , Name (..)
  , Term (..)
  ) where

import Language.PL.DeBruijnIndex
import Language.PL.Name
import Language.PL.Type

data Term = Var DeBruijnIndex
          | Abs Name Type Term
          | App Term Term
          | If Term Term Term
          | T
          | F
  deriving (Eq, Show)
