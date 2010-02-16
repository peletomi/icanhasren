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

instance Arbitrary Char where
   arbitrary     = elements(['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-\\()[]")
   coarbitrary c = variant (ord c `rem` 4)

createFileName ns es = zipWith (\n e -> (take 200 n)++"."++(take 3 e)) ns es

toUpperFirst [] = []
toUpperFirst (n:ns) = toUpper n : map toLower ns

createNewNames pattern ns = reverse . map newName $ renamed
                            where names    = createFileName ns (reverse ns)
                                  renamed  = renameFilePath pattern names

prop_renamesize xs = length renamed == length xs
                     where renamed = renameFilePath "[N]" xs

prop_fileNamePat ns             = ns == createNewNames "[N]" ns
prop_fileNameToUpperPat ns      = (map (map toUpper) ns) == createNewNames "[NN]" ns
prop_fileNameToLowerPat ns      = (map (map toLower) ns) == createNewNames "[nn]" ns
prop_fileNameToUpperFirstPat ns = (map toUpperFirst ns) == createNewNames "[Nn]" ns

prop_extractToEnd ns    = map (drop 2) (createFileName ns (reverse ns)) == createNewNames "[X:3]" ns  -- take from the third
prop_extractFromEnd ns  = map (reverse . take 3 . reverse) (createFileName ns (reverse ns)) == createNewNames "[X:-3]" ns -- take from the third from the end
prop_extractAll ns      = createFileName ns (reverse ns) == createNewNames "[X:1]" ns                 -- take from the beginning
prop_extractTwoFrom ns  = map (drop 1 . take 3) ns == createNewNames "[Xn:2,2]" ns           -- take from the second two characters
prop_extractTwoBack ns  = map (drop 0 . take 2) ns == createNewNames "[Xn:2,-2]" ns          -- take from the second two backwards
prop_extractOverLen ns  = map (drop 2) (createFileName ns (reverse ns)) == createNewNames "[X:3,250]" ns -- take from the third until the end (or max 50)
prop_extractName ns     = map (take 3) ns == createNewNames "[Xn:1,3]" ns                 -- take from the name three characters
prop_extractFromBack ns = map (reverse . drop 1 . take 3 . reverse) ns == createNewNames "[Xn:-3,2]"  ns -- take two from the third from the end of the name
prop_extractBackBack ns = map (reverse . drop 2 . take 4 . reverse) ns == createNewNames "[Xn:-3,-2]" ns -- take two backwards from the third from the end of the name
prop_extractNameName ns = createNewNames "[N]"  ns == createNewNames "[Xn:1]" ns
prop_extractNameUp ns   = createNewNames "[NN]" ns == createNewNames "[XXn:1]" ns
prop_extractNameUpFi ns = createNewNames "[Nn]" ns == createNewNames "[Xxn:1]" ns
prop_extractExtExt ns   = createNewNames "[E]"  ns == createNewNames "[Xe:1]" ns
prop_extractExtUp ns    = createNewNames "[EE]" ns == createNewNames "[XXe:1]" ns
prop_extractExtUpFi ns  = createNewNames "[Ee]" ns == createNewNames "[Xxe:1]" ns

-- substring substringFrom
