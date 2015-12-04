{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

-- |
--
-- Module      :  DataAssociation.PostProcess.Descriptor
-- Description :  Post processing descriptors, that can be read from string.
-- License     :  MIT
--
--
--

module DataAssociation.PostProcess.Descriptor (
  PostProcessDescription(..)
, PostProcessDescriptor(..)

, RuleFilter(..)
, RulePart(..)
, ItemsetFilter(..)

, RuleOrder(..)

, RuleGroup

) where

import Data.Typeable

---------------------------------------------------------------------------

class (Show d, Read d, Typeable d, Ord d) => PostProcessDescription d

data PostProcessDescriptor = forall d . PostProcessDescription d => PostProcessDescriptor d

---------------------------------------------------------------------------


data RuleFilter it = RuleFilter RulePart
                               (ItemsetFilter it)
                   deriving (Show, Read, Typeable, Eq, Ord)

instance (Typeable it, Show it, Read it, Ord it) => PostProcessDescription (RuleFilter it)

data RulePart = Left
              | Right
              deriving (Show, Read, Typeable, Eq, Ord)

data ItemsetFilter it = Contains it
                      | Not (ItemsetFilter it)
                      | And (ItemsetFilter it) (ItemsetFilter it)
                      | Or  (ItemsetFilter it) (ItemsetFilter it)
                      deriving (Show, Read, Typeable, Eq, Ord)

---------------------------------------------------------------------------

data SortDir = Ascending | Descending deriving (Show, Read, Typeable, Eq, Ord)

data SortBy it = Support
               | Confidence
               | Item RulePart [it]
               deriving (Show, Read, Typeable, Eq, Ord)


data RuleOrder it  = Sort SortDir (SortBy it)
                   deriving (Show, Read, Typeable, Eq, Ord)

instance (Typeable it, Show it, Read it, Ord it) => PostProcessDescription (RuleOrder it)

---------------------------------------------------------------------------

newtype RuleGroup = RuleGroup RulePart
                  deriving (Show, Read, Typeable, Eq, Ord)
instance PostProcessDescription RuleGroup


