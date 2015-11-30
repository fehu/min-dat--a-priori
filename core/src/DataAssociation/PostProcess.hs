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

) where

import DataAssociation

---------------------------------------------------------------------------


newtype PostProcess set it = PostProcess ([AssocRule set it] -> [AssocRule set it])

postProcess :: PostProcess set it -> [AssocRule set it] -> [AssocRule set it]
postProcess (PostProcess f) = f

filterRuleFrom :: Itemset set item => (set item -> Bool) -> PostProcess set item
filterRuleFrom f = PostProcess (filter (f . ruleFrom))

filterRuleFollows :: Itemset set item => (set item -> Bool) -> PostProcess set item
filterRuleFollows f = PostProcess (filter (f . ruleFollows))




--containsItem :: item -> set item -> Bool
--containsItem it set = undefined

--instance Read (PostProcess set it) where
--    readsPrec d r = readParen (d > app_prec)
--                    (\x -> undefined) r
--        where app_prec = 10


