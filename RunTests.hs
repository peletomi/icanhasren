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

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Tests

main = defaultMain tests

tests = [
        testGroup "Patterns" 
                [ testProperty "length of renamed list"             prop_renamesize
                , testProperty "literal"                            prop_literal
                , testProperty "fulle file name"                    prop_fullFileName
                , testProperty "file name"                          prop_fileNamePat
                , testProperty "file name to upper"                 prop_fileNameToUpperPat
                , testProperty "file name to lower"                 prop_fileNameToLowerPat
                , testProperty "file name first to upper"           prop_fileNameToUpperFirstPat
                , testProperty "extract to end"                     prop_extractToEnd
                , testProperty "extract from end"                   prop_extractFromEnd
                , testProperty "extract all"                        prop_extractAll
                , testProperty "extract two chars from index"       prop_extractTwoFrom 
                , testProperty "extract two chars from back"        prop_extractTwoBack 
                , testProperty "extracting over length"             prop_extractOverLen
                , testProperty "extract name"                       prop_extractName
                , testProperty "extracting backwards"               prop_extractFromBack
                , testProperty "extract from back two back"         prop_extractBackBack
                , testProperty "extract name"                       prop_extractNameName
                , testProperty "extract name to upper"              prop_extractNameUp
                , testProperty "extract name first to upper"        prop_extractNameUpFi
                , testProperty "extract extension"                  prop_extractExtExt
                , testProperty "extract extension to upper"         prop_extractExtUp
                , testProperty "extract extension first to upper"   prop_extractExtUpFi 
                , testProperty "counter"                            prop_counter
                , testProperty "two counters"                       prop_twoCounters
                , testProperty "counter from five"                  prop_counterFromFive
                , testProperty "counter with odd numbers"           prop_counterOdd
                , testProperty "counter with characters"            prop_counterChar
                , testProperty "counter with another counter"       prop_counterTwoDiff
                ]
         ]
