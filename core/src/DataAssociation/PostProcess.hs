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




