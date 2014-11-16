{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PL.Name
  ( Name (..)
  , fresh
  ) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet, member)
import Data.Text.Lazy (Text, snoc)

newtype Name = Name
  { unName :: Text
  } deriving (Eq, Hashable, Show)

tick :: Name -> Name
tick (Name name) = Name $ snoc name '\''

fresh :: HashSet Name -> Name -> Name
fresh names =
  let go name =
        if name `member` names
        then go $ tick name
        else name
  in go
