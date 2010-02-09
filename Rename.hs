{-

Copyright (C) 2009 Tamas Eppel <Tamas.Eppel@gmail.com>

This file is part of Renamer.

Renamer is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Renamer is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Renamer.  If not, see <http://www.gnu.org/licenses/>.

-}

module Rename (
    CheckError,
    isAnyFatalError,
    isAnyForceError,
    RenameResult,
    isDirectory,
    oldName,
    newName,
    collisions,
    renameFilePath,
    rename,
    loadFile,
    writeLine
)
where

import System.FilePath

import System.IO
import System.FilePath
import qualified System.Directory as D
import qualified System.Posix.Files as PF

import Text.Parsec
import Text.Parsec.String
import Text.Printf

import Data.Char
import Data.Maybe (fromMaybe, fromJust)
import Data.List (partition)

import Control.Monad (forM)

import qualified Data.Set as S

data Case = NoChange | LowerCase | UpperCase | FirstUpper
            deriving (Eq, Show, Read, Enum)  

data CheckError = NoError | NewNameCollision | OldNameCollision | Existing
                  deriving (Eq, Show)  

data RenContext = RenContext {
    directory :: FilePath
,   fileName  :: FilePath
,   name      :: String
,   ext       :: String
,   counter   :: Maybe Int
,   result    :: String
} deriving (Show)

data RenameResult = RenameResult  {
    oldName   :: FilePath
,   newName   :: FilePath
,   errors    :: [CheckError]
}

instance Show RenameResult where
    show rr = printf "%s%s%s %s\t->\t%s" newCol exists oldCol (oldName rr) (newName rr)
                where newCol = if isCheckError NewNameCollision rr then "!" else " "
                      exists = if isCheckError Existing rr then "*" else " "
                      oldCol = if isCheckError OldNameCollision rr then "=" else " "

instance Eq RenameResult where
    (==) a b = oldName a == oldName b && newName a == newName b
    (/=) a b = not $ a == b

instance Ord RenameResult where
    compare a b = oldName a `compare` oldName b
    (<)  a b = oldName a <  oldName b
    (>=) a b = oldName a >= oldName b
    (>)  a b = oldName a >  oldName b
    (<=) a b = oldName a <= oldName b
    max  a b = if a >= b then a else b
    min  a b = if a <= b then a else b

type Renamer = RenContext -> RenContext

type Pattern = String

type RenBuilder = Pattern -> Renamer

toRenContext :: String -> RenContext
toRenContext n = RenContext dir fname name (dropWhile (=='.') ext) Nothing ""
                    where (name, ext)  = splitExtension . takeFileName $ n
                          (dir, fname) = splitFileName n 

modifyResult :: RenContext -> String -> RenContext
modifyResult rc val = rc { result = val }

getCase :: String -> Case
getCase (a:b:ls) | isUpper a && isUpper b = UpperCase
                 | isUpper a && isLower b = FirstUpper
                 | isLower a && isLower b = LowerCase
getCase _        = NoChange

applyCase :: Case -> String -> String
applyCase _          [] = []
applyCase NoChange   c  = c
applyCase FirstUpper (c:cs) = toUpper c : map toLower cs
applyCase UpperCase  c  = map toUpper c
applyCase LowerCase  c  = map toLower c

literalBuilder :: String -> Renamer
literalBuilder val c =  modifyResult c (result c ++ val)

counterBuilder :: String -> Int -> Renamer
counterBuilder format diff c = modifyResult c (result c ++ applyFormat format newCount) newCount
                               where newCount = case counter c of
                                                   Nothing -> Just (read format :: Int)
                                                   Just a  -> Just (a + diff)
                                     modifyResult rc val count = rc { result = val, counter = count }
                                     applyFormat f v = replicate time '0' ++ sv
                                         where sv = show . fromJust $ v :: String
                                               time = length format - length sv 

stringFieldBuilder :: (RenContext -> String) -> Case -> Renamer
stringFieldBuilder accessor casing c = modifyResult c (result c ++ applyCase casing (accessor c))

nameBuilder, extBuilder, fileNameBuilder :: Case -> Renamer
nameBuilder     = stringFieldBuilder name
extBuilder      = stringFieldBuilder ext
fileNameBuilder = stringFieldBuilder fileName

---------------- Pattern Processing ----------------------

parsePattern :: GenParser Char st Renamer
parsePattern = do
                    r <- many1 usedPat
                    return (foldr (.) id (reverse r))

usedPat :: GenParser Char st Renamer
usedPat = try namePar     <|>
          try extPar      <|>
          try fileNamePar <|>
          try counterPar  <|>
          try literalPar  <?>
          "unknown pattern"

counterPar :: GenParser Char st Renamer
counterPar = do  
               char '['
               nc <- (char 'C' <|> char 'c')
               cd <- optionMaybe (try coDataPar <|> coDataFormatPar)
               char ']'
               return (cbuilder cd)

cbuilder :: Maybe (String, Int) -> Renamer
cbuilder m = counterBuilder f d
                where (f, d) = fromMaybe ("1", 1) m

coDataFormatPar :: GenParser Char st (String, Int)
coDataFormatPar = do
                    char ':'
                    f <- many1 digit
                    return (f, 1)

coDataPar :: GenParser Char st (String, Int)
coDataPar = do
              char ':'
              f <- many1 digit
              char ','
              d <- many1 digit
              return (f, read d :: Int)

namePar, extPar, fileNamePar :: GenParser Char st Renamer
namePar     = simpleParBuilder 'N' nameBuilder
extPar      = simpleParBuilder 'E' extBuilder
fileNamePar = simpleParBuilder 'F' fileNameBuilder

simpleParBuilder :: Char -> (Case -> Renamer) -> GenParser Char st Renamer
simpleParBuilder c builder = do  
                               char '['
                               nc <- many1 (char (toUpper c) <|> char (toLower c))
                               char ']'
                               return (builder (getCase nc))

literalPar :: GenParser Char st Renamer
literalPar = do
                v <- many1 (noneOf "[]")
                return (literalBuilder v)

---------------- Renaming ----------------------

collisions :: [RenameResult] -> [String]
collisions ls = S.toList ((toSet oldName) `S.intersection` (toSet newName))
                where toSet f = S.fromList . map f $ ls -- puts the names in a RenameResult into a set, f retrieves the name

getRenamer pattern = case parse parsePattern "name" pattern of
                        Right r -> r
                        Left  m -> error . show $  m

renameFilePath :: String -> [String] -> [RenameResult]
renameFilePath _       []     = []
renameFilePath pattern (i:is) = getNames $ ren [(i, renamer . toRenContext $ i)] is
                                  where
                                      renamer = getRenamer pattern -- parse pattern and get renamer function
                                      ren res    []     = res
                                      ren (r:rs) (n:ns) = ren ( (n, renamer nrc) : r : rs) ns -- renames file name, with ren context from old rename to preserve counters
                                          where rc  = toRenContext n
                                                co  = counter . snd $ r
                                                nrc = rc { counter = co }
                                      getNames = map (\(rn, rrc) -> (toRenameResult rn (directory rrc `combine` result rrc)))

rename :: String -> [String] -> IO [RenameResult]
rename p files= do 
                  chrr <- checkResult (renameFilePath p files)
                  sortResults chrr

---------------- Checking ----------------------

type Checker = [RenameResult] -> RenameResult -> IO CheckError

checkExist :: Checker
checkExist _ curr = do
                      let name = newName curr
                      fe <- D.doesFileExist name
                      de <- D.doesDirectoryExist name
                      return (if fe || de then Existing else NoError)

checkOldCollision :: Checker
checkOldCollision rr curr = do
                              let name      = newName curr
                              let rrSans    = S.delete curr $ S.fromList rr
                              let oldNames  = map (oldName) $ S.toList rrSans
                              let isColliding = not . null . filter (==name) $ oldNames
                              return (if isColliding then OldNameCollision else NoError)

checkNewCollision :: Checker
checkNewCollision rr curr = do
                              let name        = newName curr
                              let rrSans      = S.delete curr $ S.fromList rr
                              let newNames    = map (newName) $ S.toList rrSans
                              let isColliding = not . null . filter (==name) $ newNames
                              return (if isColliding then NewNameCollision else NoError)

-- cycles over each result and applies checker
checkResult :: [RenameResult] -> IO [RenameResult]
checkResult rr = forM rr (\r -> do
                                  ce <- forM cs (\c -> c rr r)
                                  return (r { errors = filter (/= NoError) ce })
                         )
                    where cs = [checkExist, checkOldCollision, checkNewCollision]

isCheckError :: CheckError -> RenameResult -> Bool
isCheckError ce rr = foldr step False (errors rr)
                       where step e acc = acc || (e == ce)

isAnyFatalError :: [RenameResult] -> Bool
isAnyFatalError rr = foldr step False rr
                        where step r acc = acc || (any (==NewNameCollision) $ errors r)

isAnyForceError :: [RenameResult] -> Bool
isAnyForceError rr = foldr step False rr
                        where step r acc = acc || (any (==Existing) $ errors r)

---------------- Sort Results ----------------------
  
{-
  The names will be sorted to the ok list in iterations.
  First all names, which do not have old name collisions
  can be put into ok. Then all names, for which the old
  name is already in the ok list. If we can not find such
  names, but there are names left, then there is a circular
  dependency. This dependency has to be broken up, by renaming
  one of the members into a temporary name.
-}

sortResults :: [RenameResult] -> IO [RenameResult]
sortResults rr = soRe rr []

soRe :: [RenameResult] -> [RenameResult] -> IO [RenameResult]
soRe []  ok = return ok
soRe nok ok | null toOk && (not . null $ toNok) = do
                                                    newNok <- breakCycle toNok
                                                    soRe newNok ok
            | null toOk = return ok
            | otherwise = soRe toNok (ok ++ toOk)
              where (toOk', toNok) = partition (\r -> isOk r ok) nok
                    toOk = map removeErrors toOk' -- errors can be removed if Exists && OldNameCollision both are present
                    removeErrors r = if isBothErrors r then r { errors = filter (not . isResolved) (errors r) } else r
                    isBothErrors r = any (==OldNameCollision) (errors r) && any (==Existing) (errors r)
                    isResolved   e = e == OldNameCollision || e == Existing

breakCycle :: [RenameResult] -> IO [RenameResult]
breakCycle []     = return []
breakCycle (r:rs) = do 
                       nn <- createTempName (newName r)
                       let or  = toRenameResult (oldName r) nn
                       let nr  = toRenameResult nn (newName r)
                       let nr' = nr { errors = [OldNameCollision] } -- this error needs to be set, to hold the insertion of nr
                       return (or:nr':rs)

createTempName :: FilePath -> IO FilePath
createTempName p = do
                     (tempf, temph) <- openTempFile "." p
                     hClose temph
                     D.removeFile tempf
                     return tempf

isOk :: RenameResult -> [RenameResult] -> Bool
isOk r ok = noOldCollision || inOk
           where noOldCollision = not . isCheckError OldNameCollision $ r
                 inOk = newName r `elem` (map oldName ok) -- the new name should be in the ok (oldName) list

toRenameResult :: FilePath -> FilePath -> RenameResult
toRenameResult on nn = RenameResult { oldName = on, newName = nn, errors  = [] }
  
-- FIXME throws an exception if file does not exist
isDirectory :: FilePath -> IO Bool
isDirectory name = do
                status <- PF.getFileStatus name
                return (PF.isDirectory status)

---------------- File handling ----------------------

loadFile :: FilePath -> Bool -> IO [RenameResult]
loadFile file isUndo = do
                         hIn <- openFile file ReadMode
                         let convFunc = if isUndo then (flip toRenameResult) else toRenameResult
                         result <- resultsFromLines hIn convFunc []
                         hClose hIn
                         return result
                              
resultsFromLines :: Handle -> (FilePath -> FilePath -> RenameResult) -> [RenameResult] -> IO [RenameResult]
resultsFromLines hIn convFunc r = do
                                    eof <- hIsEOF hIn
                                    if eof
                                      then return r
                                      else do l <- hGetLine hIn
                                              resultsFromLines hIn convFunc ((lineToRes l) : r)
                                              where lineToRes l = convFunc on nn
                                                                where (on, nn') = break (==':') l
                                                                      nn        = if null nn' then "" else tail nn'

writeLine :: Handle -> RenameResult -> IO ()
writeLine hOut r = hPutStrLn hOut (oldName r ++ ":" ++ newName r)