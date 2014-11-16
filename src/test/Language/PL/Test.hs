{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}

module Language.PL.Test
  ( tests
  ) where

import Control.Applicative
import Data.HashSet (HashSet, member)
import Data.Text.Lazy (isPrefixOf)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances

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

tests :: IO [Test]
tests = return
  [ testGroup "fresh" freshTests
  , testGroup "interpret1" interpret1Tests
  ]

freshTests =
  [ testProperty "fresh name is not in set" $ \names name ->
      not $ fresh names name `member` names

  , testProperty "fresh name starts with original name" $ \names name ->
      unName name `isPrefixOf` (unName $ fresh names name)
  ]

interpret1Tests =
  [ testProperty "call by value" $ \tm ->
      case (tm, interpret1 tm) of

        -- E-AppAbs
        (App abs@(Abs _ _) arg@(Abs _ _), actual) -> label "E-AppAbs" $
          case actual of
            Just tm' -> tm' /= tm
            Nothing  -> False

        -- E-App2
        (App abs@(Abs _ _) arg, actual) -> label "E-App2" $
          actual /= Nothing ==> case actual of
            Just (App abs' arg') -> abs' == abs && arg' /= arg
            Just _ -> False

        -- E-App1
        (App abs arg, actual) -> label "E-App1" $
          actual /= Nothing ==> case actual of
            Just (App abs' arg') -> abs' /= abs && arg' == arg
            Just _ -> False

        (_, actual) -> property $
          actual == Nothing
  ]
