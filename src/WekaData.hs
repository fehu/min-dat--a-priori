-----------------------------------------------------------------------------
-- Works with weka *.arff data files.
-----------------------------------------------------------------------------

module WekaData (

) where

import Data.List
import Data.List.Split
import Data.Char
import Control.Applicative

data RawWekaData = RawWekaData { rwdName      :: String
                               , rwdAttrs     :: [WekaDataAttribute]
                               , rawWekaData  :: [[String]]
                               }
                    deriving Show

data WekaDataAttribute = WekaAttrNum String
                       | WekaAttrNom String [String]
                    deriving Show

readWekaData :: String -> IO RawWekaData
readWekaData filename = do lines <- splitOn "\n" <$> readFile filename
                                 -- same as fmap (splitOn "\n") (readFile filename)
                           return $ readWekaData' lines Nothing [] []

readWekaData' :: [String] -> Maybe String -> [WekaDataAttribute] -> [[String]] -> RawWekaData

-- ignore comments
readWekaData' (l:lines) name attrs dta
    | "%" `isPrefixOf` l = readWekaData' lines name attrs dta

-- handling name
readWekaData' (l:lines) Nothing [] []
    | "@relation " `isPrefixOf` l =
        let name = dropComment $ drop (length "@relation ") l
        in readWekaData' lines (Just name) [] []

-- handling attributes
readWekaData' (l:lines) name@(Just _) attrs []
    | "@attribute " `isPrefixOf` l =
        let attr = readWekaAttr . dropSpaces . dropComment $ l
        in readWekaData' lines name (attr:attrs) []

-- handling data
readWekaData' (l:lines) name@(Just _) attrs@(_:_) dta
    | "@data" `isPrefixOf` l = readWekaData' lines name attrs dta
    | otherwise              = readWekaData' lines name attrs (splitOn "," l : dta)

-- return result
readWekaData' [] (Just name) attrs dta = RawWekaData name attrs dta

dropComment = takeWhile (/= '%')
dropSpaces = dropWhile isSpace

readWekaAttr :: String -> WekaDataAttribute
readWekaAttr line | head l' == '{'            = WekaAttrNom name domain
                  | "numeric" `isPrefixOf` l' = WekaAttrNum name
                  | otherwise = error $ show l'
    where l    = dropSpaces $ drop (length "@attribute ") line
          (name, len) = if head l == '\'' then (takeWhile (/= '\'') (drop 1 l), 2)
                                          else (takeWhile (/= ' ') l, 0)
          l'     = dropSpaces $ drop (length name + len) l
          f      = filter (fmap not $ (||) <$> isSpace <*> (`elem` "{}"))
          domain = map f $ splitOn "," l'
