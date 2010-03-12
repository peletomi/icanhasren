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

module Tests where

import Test.QuickCheck
import Rename
import Data.Char

data TestData = TestData { fileName     :: String
                         , directory    :: String
                         , fullFileName :: String
                         , baseName     :: String
                         , extension    :: String
                         } deriving (Show)

extractFileName = map fileName

fileChars    = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-\\()[]"

genName = elements fileChars
genTestData = do
                base <- listOf genName
                ext  <- vectorOf 3 genName
                dirs <- listOf (elements ('/' : fileChars))
                let fileName = if null ext then base else base ++ ('.' : ext)
                return (TestData fileName dirs (dirs ++ "/" ++ fileName) base ext)

instance Arbitrary TestData where
   arbitrary = genTestData

toUpperFirst [] = []
toUpperFirst (n:ns) = toUpper n : map toLower ns

createNewNames pattern ns = map newName renamed
                            where names   = map (fromString . fileName) ns
                                  renamed = renameFilePath pattern names

prop_renamesize ns = length ns == length (createNewNames "[N]" ns)

prop_literal ns = replicate (length ns) "abc" == createNewNames "abc" ns

prop_fullFileName ns            = map fileName ns == createNewNames "[F]" ns

prop_fileNamePat ns             = map baseName ns == createNewNames "[N]" ns
prop_fileNameToUpperPat ns      = map (map toUpper . baseName)  ns == createNewNames "[NN]" ns
prop_fileNameToLowerPat ns      = map (map toLower . baseName)  ns == createNewNames "[nn]" ns
prop_fileNameToUpperFirstPat ns = map (toUpperFirst . baseName) ns == createNewNames "[Nn]" ns

prop_extractToEnd ns    = map (drop 2) (extractFileName ns) == createNewNames "[X:3]" ns  -- take from the third
prop_extractFromEnd ns  = map (reverse . take 3 . reverse) (extractFileName ns) == createNewNames "[X:-3]" ns -- take from the third from the end
prop_extractAll ns      = extractFileName ns == createNewNames "[X:1]" ns                 -- take from the beginning
prop_extractTwoFrom ns  = map (drop 1 . take 3) (map baseName ns) == createNewNames "[Xn:2,2]" ns           -- take from the second two characters
prop_extractTwoBack ns  = map (drop 0 . take 2) (map baseName ns) == createNewNames "[Xn:2,-2]" ns          -- take from the second two backwards
prop_extractOverLen ns  = map (drop 2) (extractFileName ns) == createNewNames "[X:3,250]" ns -- take from the third until the end (or max 50)
prop_extractName ns     = map (take 3) (map baseName ns) == createNewNames "[Xn:1,3]" ns                 -- take from the name three characters
prop_extractFromBack ns = map (reverse . drop 1 . take 3 . reverse) (map baseName ns) == createNewNames "[Xn:-3,2]"  ns -- take two from the third from the end of the name
prop_extractBackBack ns = map (reverse . drop 2 . take 4 . reverse) (map baseName ns) == createNewNames "[Xn:-3,-2]" ns -- take two backwards from the third from the end of the name
prop_extractNameName ns = createNewNames "[N]"  ns == createNewNames "[Xn:1]" ns
prop_extractNameUp ns   = createNewNames "[NN]" ns == createNewNames "[XXn:1]" ns
prop_extractNameUpFi ns = createNewNames "[Nn]" ns == createNewNames "[Xxn:1]" ns
prop_extractExtExt ns   = createNewNames "[E]"  ns == createNewNames "[Xe:1]" ns
prop_extractExtUp ns    = createNewNames "[EE]" ns == createNewNames "[XXe:1]" ns
prop_extractExtUpFi ns  = createNewNames "[Ee]" ns == createNewNames "[Xxe:1]" ns

prop_counter ns         = map show [1..(length ns)] == createNewNames "[C]" ns
prop_twoCounters ns     = map (\n -> n ++ "." ++ n) nums == createNewNames "[C].[C]" ns
                            where nums = map show [1..(length ns)]
prop_counterFromFive ns = map show [5..(length ns)+4] == createNewNames "[C:5]" ns
prop_counterOdd ns      = map show oddNums == createNewNames "[C:1,2]" ns
                            where oddNums = filter odd [1..(length ns * 2)]
prop_counterChar ns     = take len result == take len newNames
                            where chars = (map (:[]) ['a'..'z'])
                                  morechars = chars ++ zipWith (++) (repeat "a") chars ++ zipWith (++) (repeat "b") chars
                                  result = map (\n -> n ++ "." ++ n) morechars 
                                  newNames = createNewNames "[C:a].[C]" ns
                                  len = (length morechars) `min` length newNames
prop_counterTwoDiff ns  = map (\(a,b) -> a ++ "." ++ b) zipped == createNewNames "[C1].[C2:1,2]" ns
                            where nums = map show [1..(length ns)]
                                  oddNums = map show (filter odd [1..(length ns * 2)])
                                  zipped = zip nums oddNums

-- substring substringFrom
