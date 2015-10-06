module Main (
    main
) where

import RunDataAssocWekaApriorySimple
import DataAssociation

import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import Control.Monad

main :: IO ()
main = getArgs >>= parse

data ParsedArg = ParsedArg { filename :: String
                           , minsup   :: Float
                           , minconf  :: Float
                           }


maybeRead = fmap fst . listToMaybe . reads

maybeReadInUnit :: String -> Maybe Float
maybeReadInUnit s = (maybeRead s :: Maybe Float) >>= f
            where f x | x <= 1 && x >= 0 = Just x
                      | otherwise        = Nothing

parse ["-h"] = usage >> exitSuccess
parse [fname, msup, mconf] = do
    let minsup  = maybe minsupError  MinSupport    $ maybeReadInUnit msup
    let minconf = maybe minconfError MinConfidence $ maybeReadInUnit mconf
--    if not . doesFileExist $ fname then error $ "File " ++ fname ++ "not found"
--                                   else mzero
    run fname minsup minconf
parse _ = unknownCmd >> usage >> exitFailure

--parse [fname, msup, mconf] | (maybeRead msup :: Maybe Float) <= 1 = putStrLn ""



unknownCmd = putStrLn "Wrong arguments!"
usage = do putStrLn "Usage: mine-rules [-h] file MinSupport MinConfidence"
           putStrLn "       where file is an *.arff nominal data file"
           putStrLn "             MinSupport and MinConfidence must be Float values in [0, 1]"


minsupError  = boundError "MinSupport"
minconfError = boundError "MinConfidence"
boundError nme = error $ nme ++ " must be a Float in [0, 1]"
