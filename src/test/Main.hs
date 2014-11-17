{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}

module Main
  ( main
  ) where

import Control.Applicative
import Data.HashSet (member)
import Data.Result (Result (..))
import Data.Text.Lazy (isPrefixOf)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (Success, Failure)
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

main :: IO ()
main = defaultMain
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
      case interpret1 tm of
        Failure -> property Discard
        Success (rule:_) _ -> case tm of
          App (Abs _ _) (Abs _ _) -> rule === EAppAbs
          App (Abs _ _) _         -> rule === EApp2
          App _         _         -> rule === EApp1
  ]
