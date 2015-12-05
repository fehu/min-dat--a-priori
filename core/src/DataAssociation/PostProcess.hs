{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

-- |
--
-- Module      :  DataAssociation.PostProcess
-- Description :  Post processing generated 'AssocRule's.
-- License     :  MIT
--
--
--

module DataAssociation.PostProcess (

  PostProcess(..)
, postProcess

, PostProcessDescription(..)
, PostProcessDescriptor(..)

, RuleFilter(..)
, RulePart(..)
, ItemsetFilter(..)

, RuleOrder(..)

, RuleGroup


, ItemByAttribute(..)

) where

import DataAssociation

import Prelude hiding (Left, Right)
import Data.Typeable

---------------------------------------------------------------------------


newtype PostProcess set it = PostProcess ([AssocRule set it] -> [AssocRule set it])


postProcess :: PostProcess set it -> [AssocRule set it] -> [AssocRule set it]
postProcess (PostProcess f) = f



---------------------------------------------------------------------------

class (Show d, Read d, Typeable d, Ord d) =>
    PostProcessDescription d set it where
        postProcessFromDescriptor :: d -> PostProcess set it

data PostProcessDescriptor set it = forall d . PostProcessDescription d set it =>
     PostProcessDescriptor d

---------------------------------------------------------------------------

class (Itemset set it) =>
    ItemByAttribute set it where containsAnyWithAttr :: set it -> String -> Bool


data RuleFilter it = RuleFilter RulePart
                               (ItemsetFilter it)
                   deriving (Show, Read, Typeable, Eq, Ord)

instance ( Typeable it, Show it, Read it, Ord it, ItemByAttribute set it ) =>
    PostProcessDescription (RuleFilter it) set it where

        postProcessFromDescriptor (RuleFilter Left f)  = ppFilterFromDescr f ruleFrom
        postProcessFromDescriptor (RuleFilter Right f) = ppFilterFromDescr f ruleFollows

ppFilterFromDescr fltr sel = PostProcess $ filter (itemsetFilter fltr . sel)

data RulePart = Left
              | Right
              deriving (Show, Read, Typeable, Eq, Ord)

itemsetFilter :: (ItemByAttribute set it) => ItemsetFilter it -> set it -> Bool
itemsetFilter (Contains x) = (`containsItem` x)
itemsetFilter (Not x)      = not . itemsetFilter x
itemsetFilter (And x y)    = \s -> itemsetFilter x s && itemsetFilter y s
itemsetFilter (Or x y)     = \s -> itemsetFilter x s || itemsetFilter y s
itemsetFilter (Has attr)   = flip containsAnyWithAttr attr

data ItemsetFilter it = Contains it
                      | Has String -- ^ Has Attribute
                      | Not (ItemsetFilter it)
                      | And (ItemsetFilter it) (ItemsetFilter it)
                      | Or  (ItemsetFilter it) (ItemsetFilter it)
                      deriving (Show, Read, Typeable, Eq, Ord)

---------------------------------------------------------------------------

data SortDir = Ascending | Descending deriving (Show, Read, Typeable, Eq, Ord)

data SortBy = Support
            | Confidence
            | Attribute RulePart [String]
            deriving (Show, Read, Typeable, Eq, Ord)


data RuleOrder = Sort SortDir SortBy
               deriving (Show, Read, Typeable, Eq, Ord)

instance (Typeable it, Show it, Read it, Ord it) =>
    PostProcessDescription RuleOrder set it where
        postProcessFromDescriptor (Sort Ascending  sortKeys) = undefined -- TODO
        postProcessFromDescriptor (Sort Descending sortKeys) = undefined -- TODO

---------------------------------------------------------------------------

newtype RuleGroup = RuleGroup RulePart
                  deriving (Show, Read, Typeable, Eq, Ord)
instance PostProcessDescription RuleGroup set it where
    postProcessFromDescriptor (RuleGroup part) = undefined

