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

import IO

import System
import System.Environment
import qualified System.Directory as D
import System.FilePath
import System.Console.GetOpt

import Control.Monad (when, foldM)
import Control.Exception(finally)

import Rename

data LoadMethod = Arguments | Load | Undo
                  deriving (Show, Eq, Enum)

data Options = Options  { optShowOnly   :: Bool
                        , optLoadMethod :: LoadMethod
                        , optLog        :: FilePath
                        , optLoad       :: FilePath
                        , optUndo       :: FilePath
                        }

defaultOptions :: Options
defaultOptions = Options { optShowOnly   = False
                         , optLoadMethod = Arguments
                         , optLog        = ""
                         , optLoad       = ""
                         , optUndo       = ""
                         }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['h']  ["help"]        (NoArg  showHelp)    "usage information"
    , Option ['s']  ["show-only"]   (NoArg  (\    opt -> return opt { optShowOnly = True })) "only show renames"
    , Option ['l']  ["log"]         (ReqArg (\arg opt -> return opt { optLog = arg }) "FILE")  "log rename actions to file"
    , Option ['i']  ["input"]       (ReqArg (\arg opt -> return opt { optLoadMethod = Load, optLoad = arg }) "FILE") "load renames from file"
    , Option ['u']  ["undo"]        (ReqArg (\arg opt -> return opt { optLoadMethod = Undo, optUndo = arg }) "FILE") "undo renames from file"
    ]

showHelp :: Options -> IO Options
showHelp _ = do
    	       prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitWith ExitSuccess

showShortUsage :: IO ()
showShortUsage = do
                   prg <- getProgName
                   hPutStrLn stderr ("Usage: " ++ prg ++ " [OPTIONS] PATTERN [FILE]")
                   hPutStrLn stderr ("Try '" ++ prg ++ " --help' for more information.")
                   exitFailure

main :: IO ()
main = do
         args <- getArgs
         when (null args) showShortUsage

         let (actions, nonOpts, msgs) = getOpt RequireOrder options args
         opts <- foldl (>>=) (return defaultOptions) actions

         renResult <- case optLoadMethod opts of
                           Arguments -> handleArgumments nonOpts
                           Load -> loadFile (optLoad opts) False
                           Undo -> loadFile (optUndo opts) True

         logFile <- if null $ optLog opts
                       then return Nothing
                       else fmap Just $ openFile (optLog opts) WriteMode

         -- do the RenameAction and always close the log if one is open
         finally (if optShowOnly opts 
                     then showMe logFile renResult
                     else renameFiles logFile renResult)
                 (case logFile of
                     Nothing   -> return ()
                     Just hLog -> hClose hLog)

type RenameAction = RenameResult -> IO ()

showMe :: Maybe Handle -> [RenameResult] -> IO ()
showMe Nothing     = applyToFiles (putStrLn . show)
showMe (Just hOut) = applyToFiles (\r -> do
                                           writeLine hOut r 
                                           putStrLn . show $ r)

handleArgumments :: [String] -> IO [RenameResult]
handleArgumments (p:f:fs) = rename p (f:fs)
handleArgumments _        = error "Pattern and files not supplied!"
                                   

renameFiles :: Maybe Handle -> [RenameResult] -> IO ()
renameFiles Nothing     = applyToFiles renameFile
renameFiles (Just hOut) = applyToFiles (\r -> do
                                                writeLine hOut r 
                                                renameFile r)

renameFile :: RenameAction
renameFile renResult = do
                         let (on, nn) = (oldName renResult, newName renResult)
                         isDir <- isDirectory on
                         if isDir 
                           then D.renameDirectory on nn
                           else D.renameFile on nn

applyToFiles :: RenameAction -> [RenameResult] -> IO ()
applyToFiles f renResult = do 
                             mapM f renResult
                             return ()
