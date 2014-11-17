{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}

module Main
  ( main
  ) where

import Control.Applicative
import Control.Rule
import Data.HashSet (HashSet, member)
import Data.Text.Lazy (isPrefixOf)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, arbitrary, choose, label, oneof, property, shrink, (==>))
import Test.QuickCheck.Instances ()

import Language.PL.DeBruijnIndex
import Language.PL.Interpreter
import Language.PL.Name
import Language.PL.Term

deriving instance Arbitrary Name

instance Arbitrary Term where
  arbitrary =
    let go depth size | depth < 0 = oneof [abs, app]
                      | size  > 5 = var
                      | otherwise = oneof [var, abs, app]
          where
            var = Var . Idx <$> choose (0, depth)
            abs = Abs (Name "x") <$> go (depth + 1) (size + 1)
            app = App <$> go depth (size + 1) <*> go depth (size + 1)
    in go (-1) 0

  shrink tm =
    case tm of
      Var (Idx idx) -> Var . Idx <$> [0 .. idx]
      Abs name body -> Abs <$> shrink name <*> shrink body
      App abs arg   -> App <$> shrink abs <*> shrink arg

main :: IO ()
main = defaultMain
  [ testGroup "fresh" freshTests
  , testGroup "interpret1" interpret1tests
  ]

freshTests =
  [ testProperty "fresh name is not in set" $ \names name ->
      not $ fresh names name `member` names

  , testProperty "fresh name starts with original name" $ \names name ->
      unName name `isPrefixOf` (unName $ fresh names name)
  ]

interpret1tests =
  [ testProperty "call by value" $ \tm ->
      case (tm, interpret1 tm) of
        (App abs@(Abs _ body) arg@(Abs _ _), actual) -> label "E-AppAbs" $
          case actual of
            Success labels _ -> last labels == "E-AppAbs"
            Failure -> False

        (App abs@(Abs _ _) arg, actual) -> label "E-App2" $
          actual /= Failure ==> case actual of
            Success labels _ -> last labels == "E-App2"

        (App abs arg, actual) -> label "E-App1" $
          actual /= Failure ==> case actual of
            Success labels _ -> last labels == "E-App1"

        (_, actual) -> property $
          actual == Failure
  ]
