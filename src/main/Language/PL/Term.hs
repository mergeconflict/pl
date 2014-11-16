{-# LANGUAGE OverloadedStrings #-}

module Language.PL.Term
  ( Term (..)
  ) where

import Data.HashSet as HashSet (HashSet, empty, insert)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Format (format)
import Data.Vector as Vector (Vector, cons, empty, (!))

import Language.PL.DeBruijnIndex
import Language.PL.Name

-- There are two implied, but un-checked structural invariants here:
--   * Any `Var idx` must be contained within `idx + 1` nested `Abs` instances.
--   * Any `App abs arg` must have an `abs` that evaluates to an `Abs` instance.
data Term = Var DeBruijnIndex
          | Abs Name Term
          | App Term Term
  deriving Eq

showTerm :: Vector Name -> HashSet Name -> Term -> Text
showTerm namesVec namesSet =
  let go tm =
        case tm of
          -- There must be a name at the given De Bruijn index in `namesVec`;
          -- if not, `tm` has been constructed incorrectly. See the comment
          -- above about the structural invariant for `Term`.
          Var (Idx idx) ->
            unName $ namesVec ! idx

          -- Ensure the name introduced by this `Abs` doesn't shadow any others
          -- already present in the environment, by requesting a fresh name. The
          -- new name is added to the environment for formatting the body.
          Abs name body ->
            let name' = fresh namesSet name
                tm'   = showTerm (cons name' namesVec) (insert name' namesSet) body
            in format "λ{}.{}" (unName name', tm')

          -- Format the abstraction and its argument independently
          App abs arg -> format "({} {})" (go abs, go arg)
  in go

instance Show Term where
  show tm = unpack $ showTerm Vector.empty HashSet.empty tm
