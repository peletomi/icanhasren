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

import Tests
import Test.QuickCheck.Batch

options = TestOptions
      { no_of_tests         = 200
      , length_of_tests     = 1
      , debug_tests         = False }

main = do
    runTests "simple" options
        [ run prop_renamesize
        , run prop_fileNamePat
        , run prop_fileNameToUpperPat
        , run prop_fileNameToLowerPat
        , run prop_fileNameToUpperFirstPat
        , run prop_extractToEnd
        , run prop_extractFromEnd
        , run prop_extractAll
        --, run prop_extractTwoFrom 
        --, run prop_extractTwoBack 
        --, run prop_extractOverLen 
        , run prop_extractName
        , run prop_extractExt
        --, run prop_extractFromBack
        --, run prop_extractBackBack
        , run prop_extractNameName
        , run prop_extractNameUp
        , run prop_extractNameUpFi
        , run prop_extractExtExt
        , run prop_extractExtUp
        , run prop_extractExtUpFi 
        ]
