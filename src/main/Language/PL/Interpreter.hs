module Language.PL.Interpreter
  ( interpret1
  , interpret
  ) where

import Control.Applicative

import Language.PL.DeBruijnIndex
import Language.PL.Term

-- Recursively update each `Var` using a given function `f`, that produces a new
-- term using the number of surrounding `binders` (e.g. `Abs` instances) and the
-- De Bruijn `idx` of the variable.
mapVars :: (Int -> DeBruijnIndex -> Term) -> Term -> Term
mapVars f =
  let go binders tm =
        case tm of
          Var idx -> f binders idx
          Abs name body -> Abs name $ go (binders + 1) body
          App abs arg -> App (go binders abs) (go binders arg)
  in go 0

-- Shift all free variables by a given `depth`. Free variables are those with
-- a De Bruijn `idx` greater than the current number of surrounding `binders`.
shiftFreeVars :: Int -> Term -> Term
shiftFreeVars depth =
  let shift binders (Idx idx) =
        if idx >= binders
        then Var (Idx $ idx + depth)
        else Var (Idx idx)
  in mapVars shift

-- Substitute all references to the outermost variable with a replacement `tm`.
-- These references are those with a De Bruijn `idx` equal to the number of
-- surrounding `binders`. Note that all free variables in the replacement `tm`
-- must be appropriately shifted.
substituteVar :: Term -> Term -> Term
substituteVar tm =
  let substitute binders (Idx idx) =
        if binders == idx
        then shiftFreeVars binders tm
        else Var (Idx idx)
  in mapVars substitute

-- Beta reduction rule: I don't understand this yet lol.
beta :: Term -> Term -> Term
beta old new =
  let expanded    = shiftFreeVars 1 old
      substituted = substituteVar expanded new
      reduced     = shiftFreeVars (-1) substituted
  in reduced

-- Single step reduction
interpret1 :: Term -> Maybe Term
interpret1 tm =
  case tm of

    -- E-AppAbs
    App abs@(Abs _ body) arg@(Abs _ _) ->
      Just $ beta arg body

    -- E-App2
    App abs@(Abs _ _) arg ->
      App <$> pure abs <*> interpret1 arg

    -- E-App1
    App abs arg ->
      App <$> interpret1 abs <*> pure arg

    _ -> Nothing

-- Full reduction
interpret :: Term -> Term
interpret tm =
  case interpret1 tm of
    Just tm' -> interpret tm'
    Nothing  -> tm
