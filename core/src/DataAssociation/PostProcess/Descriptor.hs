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


class (Show d, Typeable d) => PostProcessDescription d

data PostProcessDescriptor = forall d . PostProcessDescription d => PostProcessDescriptor d


---------------------------------------------------------------------------


data RuleFilter = RuleFilter RulePart
                             ItemsetFilter
                deriving (Show, Typeable, Eq, Ord)
instance PostProcessDescription RuleFilter

data RulePart = RuleFrom
              | RuleFollows
              deriving (Show, Typeable, Eq, Ord)
--instance PostProcessDescription RulePart

data ItemsetFilter = TODO
                   deriving (Show, Typeable, Eq, Ord)
--instance PostProcessDescription ItemsetFilter


---------------------------------------------------------------------------

data RuleOrder = RuleOrderTODO


---------------------------------------------------------------------------

type RuleGroup = RulePart


