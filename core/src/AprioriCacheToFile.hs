-----------------------------------------------------------------------------
--
-- Module      :  AprioriCacheToFile
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--


module Main (main) where

import WekaData
import DataAssociation
import DataAssociation.Itemset.SetImpl
import DataAssociation.APriori

import System.Environment
import System.Exit

import Control.Exception

import Data.Set  (elems)
import Data.List (intercalate)

-----------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= parse


parse ["-h"] = usage >> exitSuccess
parse [srcFile, targetFile] = do
    rawData <- readWekaData srcFile `catch` exitException "read Weka data"

    let sparse = wekaData2Sparse rawData
    let itemsets = map newItemset sparse :: Itemsets String
    let cache = mkAprioriCache itemsets

    writeFile targetFile (show cache) `catch` exitException "write cache file"
    putStrLn $ "wrote cache in " ++ targetFile
    exitSuccess
parse _ = unknownCmd >> usage >> exitFailure


exitException msgPart e = putStrLn ("[ERROR] Failed to " ++ msgPart
                                 ++ ", reason: " ++ show (e :: SomeException))
                       >> exitFailure


unknownCmd = putStrLn "Wrong arguments!"
usage = do putStrLn "Usage: mk-apriori-cache [-h] source target"
           putStrLn "       where source is an *.arff nominal sparse data file"
           putStrLn "             target is the file to write"

