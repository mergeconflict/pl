module Language.PL.Interpreter
  ( interpret1
  , interpret
  ) where

import Control.Applicative
import Control.Rule

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

eApp1 :: Rule [String] Term Term
eApp1 = rule' ["E-App1"] $ \tm -> case tm of
  App abs arg -> App <$> interpret1 abs <*> pure arg
  _ -> Failure

eApp2 :: Rule [String] Term Term
eApp2 = rule' ["E-App2"] $ \tm -> case tm of
  App abs@(Abs _ _) arg -> App <$> pure abs <*> interpret1 arg
  _ -> Failure

eAppAbs :: Rule [String] Term Term
eAppAbs = rule ["E-AppAbs"] $ \tm -> case tm of
  App abs@(Abs _ body) arg@(Abs _ _) -> pure $ beta arg body
  _ -> Nothing

-- Single step reduction
interpret1 :: Term -> Result [String] Term
interpret1 tm = runRule (eAppAbs <|> eApp2 <|> eApp1) tm

-- Full reduction
interpret :: Term -> Result [String] Term
interpret =
  let go tm =
        case tm' of
          Failure -> tm
          _       -> go tm'
        where tm' = tm >>= interpret1
  in go . pure
