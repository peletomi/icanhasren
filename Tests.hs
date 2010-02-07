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

createFileName ns es = zipWith (\n e -> n++"."++(take 3 e)) ns es

toUpperFirst [] = []
toUpperFirst (n:ns) = toUpper n : map toLower ns

prop_renamesize xs = length renamed == length xs
                     where renamed = renameFilePath "[N]" xs

prop_fileNamePat ns = ns == newNames
                      where names    = createFileName ns ns
                            renamed  = renameFilePath "[N]" names
                            newNames = reverse . map newName $ renamed

prop_fileNameToUpperPat ns = (map (map toUpper) ns) == newNames
                             where names    = createFileName ns ns
                                   renamed  = renameFilePath "[NN]" names
                                   newNames = reverse . map newName $ renamed

prop_fileNameToLowerPat ns = (map (map toLower) ns) == newNames
                             where names    = createFileName ns ns
                                   renamed  = renameFilePath "[nn]" names
                                   newNames = reverse . map newName $ renamed

prop_fileNameToUpperFirstPat ns = (map toUpperFirst ns) == newNames
                                  where names    = createFileName ns ns
                                        renamed  = renameFilePath "[Nn]" names
                                        newNames = reverse . map newName $ renamed
