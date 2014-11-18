module Language.PL.PrettyPrint
  ( term
  ) where

import Data.HashSet as HashSet (HashSet, empty, insert)
import Data.Text.Lazy (unpack)
import Data.Vector as Vector (Vector, cons, empty, length, unsafeIndex)
import Text.PrettyPrint (Doc, char, int, sep, text, (<>), (<+>))

import Language.PL.Name
import Language.PL.Term

term :: Term -> Doc
term =
  let go namesVec namesSet tm =
        case tm of
        -- The `Var` is bound if there is a name at its `idx in `namesVec`,
        -- and free otherwise. Label free variables with their De Bruijn
        -- indices relative to the top-level term.
        Var (Idx idx) ->
          if idx >= len
          then char '<' <> text "free variable" <+> int (idx - len) <> char '>'
          else text $ unpack $ unName $ unsafeIndex namesVec idx
            where len = Vector.length namesVec

        -- Ensure the name introduced by this `Abs` doesn't shadow any others
        -- already present in the environment, by requesting a fresh name. The
        -- new name is added to the environment for formatting the body.
        Abs name body ->
          char 'λ' <> (text $ unpack $ unName name') <> char '.' <> tm'
            where name' = fresh namesSet name
                  tm'   = go (cons name' namesVec) (insert name' namesSet) body

        -- Format the abstraction and its argument independently
        App abs arg ->
          char '(' <> sep [go namesVec namesSet abs,
                           go namesVec namesSet arg] <> char ')'
  in go Vector.empty HashSet.empty
