{-# LANGUAGE OverloadedStrings #-}

module Language.PL.Term
  ( Term (..)
  , Pretty (..)
  ) where

import Data.HashSet as HashSet (HashSet, empty, insert)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Format (Only (..), format)
import Data.Vector as Vector (Vector, cons, empty, length, unsafeIndex)

import Language.PL.DeBruijnIndex
import Language.PL.Name

-- There are two implied, but un-checked structural invariants here:
--   * Any `Var idx` must be contained within `idx + 1` nested `Abs` instances.
--   * Any `App abs arg` must have an `abs` that evaluates to an `Abs` instance.
data Term = Var DeBruijnIndex
          | Abs Name Term
          | App Term Term
  deriving (Eq, Show)

newtype Pretty = Pretty
  { unPretty :: Term
  } deriving Eq

pretty :: Vector Name -> HashSet Name -> Term -> Text
pretty namesVec namesSet =
  let go tm =
        case tm of
          -- The `Var` is bound if there is a name at its `idx in `namesVec`,
          -- and free otherwise. Label free variables with their De Bruijn
          -- indices relative to the top-level term.
          Var (Idx idx) ->
            if idx >= len
            then format "<free variable {}>" (Only $ idx - len)
            else unName $ unsafeIndex namesVec idx
              where len = Vector.length namesVec

          -- Ensure the name introduced by this `Abs` doesn't shadow any others
          -- already present in the environment, by requesting a fresh name. The
          -- new name is added to the environment for formatting the body.
          Abs name body ->
            format "λ{}.{}" (unName name', tm')
              where name' = fresh namesSet name
                    tm'   = pretty (cons name' namesVec) (insert name' namesSet) body

          -- Format the abstraction and its argument independently
          App abs arg ->
            format "({} {})" (go abs, go arg)
  in go

instance Show Pretty where
  show (Pretty tm) = unpack $ pretty Vector.empty HashSet.empty tm
