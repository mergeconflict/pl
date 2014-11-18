module Language.PL.Term
  ( DeBruijnIndex (..)
  , Name (..)
  , Term (..)
  ) where

import Language.PL.DeBruijnIndex
import Language.PL.Name

data Term = Var DeBruijnIndex
          | Abs Name Term
          | App Term Term
  deriving (Eq, Show)
