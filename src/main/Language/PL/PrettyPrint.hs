module Language.PL.PrettyPrint
  ( prettyTerm
  , prettyType
  ) where

import Data.HashSet as HashSet (HashSet, empty, insert)
import Data.Text.Lazy (unpack)
import Data.Vector as Vector (Vector, cons, empty, length, unsafeIndex)
import Text.PrettyPrint

import Language.PL.Name
import Language.PL.Term
import Language.PL.Type

prettyTerm :: Term -> Doc
prettyTerm =
  let go namesVec namesSet tm =
        case tm of
        -- The `Var` is bound if there is a name at its `idx in `namesVec`,
        -- and free otherwise. Label free variables with their De Bruijn
        -- indices relative to the top-level term.
        Var (Idx idx) ->
          if idx >= len
          then hcat [ char '<'
                    , text "free variable" <+> int (idx - len)
                    , char '>'
                    ]
          else text $ unpack $ unName $ unsafeIndex namesVec idx
            where len = Vector.length namesVec

        -- Ensure the name introduced by this `Abs` doesn't shadow any others
        -- already present in the environment, by requesting a fresh name. The
        -- new name is added to the environment for formatting the body.
        Abs name ty body ->
          hcat [ char 'λ'
               , text $ unpack $ unName name'
               , char ':'
               , prettyType ty
               , char '.'
               , tm'
               ]
            where name' = fresh namesSet name
                  tm'   = go (cons name' namesVec) (insert name' namesSet) body

        App abs arg ->
          parens $ sep [go namesVec namesSet abs, go namesVec namesSet arg]

        If pred lhs rhs ->
          sep [ text "if" <+> go namesVec namesSet pred
              , text "then" <+> go namesVec namesSet lhs
              , text "else" <+> go namesVec namesSet rhs
              ]

        _ ->
          text $ show tm

  in go Vector.empty HashSet.empty

prettyType :: Type -> Doc
prettyType ty = case ty of
  Function lhs rhs ->
    parens $ hsep [ prettyType lhs
                  , char '→'
                  , prettyType rhs
                  ]

  _ ->
    text $ show ty
