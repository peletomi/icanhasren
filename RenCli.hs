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
import qualified System.Posix.Files as PF
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
                        , optForce      :: Bool
                        }

data RenameActionContext = RenameActionContext
                           { getopts  :: Options
                           , nonopts  :: [String]
                           , hLog     :: Maybe Handle
                           , exec     :: RenameAction
                           }

defaultOptions :: Options
defaultOptions = Options { optShowOnly   = False
                         , optLoadMethod = Arguments
                         , optLog        = ""
                         , optLoad       = ""
                         , optUndo       = ""
                         , optForce      = False
                         }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "h"  ["help"]        (NoArg  showHelp)    "usage information"
    , Option "s"  ["show-only"]   (NoArg  (\    opt -> return opt { optShowOnly = True })) "only show renames"
    , Option "l"  ["log"]         (ReqArg (\arg opt -> return opt { optLog = arg }) "FILE")  "log rename actions to file"
    , Option "i"  ["input"]       (ReqArg (\arg opt -> return opt { optLoadMethod = Load, optLoad = arg }) "FILE") "load renames from file"
    , Option "u"  ["undo"]        (ReqArg (\arg opt -> return opt { optLoadMethod = Undo, optUndo = arg }) "FILE") "undo renames from file"
    , Option "f"  ["force"]       (NoArg  (\    opt -> return opt { optForce = True })) "force renaming"
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

isDirectory :: FilePath -> IO Bool
isDirectory name = do
                status <- PF.getFileStatus name
                return (PF.isDirectory status)

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

         hLog <- if null $ optLog opts
                       then return Nothing
                       else fmap Just $ openFile (optLog opts) WriteMode

         -- do the RenameAction and always close the log if one is open
         finally (if optShowOnly opts
                     then applyToFiles (buildShowAction opts nonOpts hLog) renResult
                     else applyToFiles (buildRenameAction opts nonOpts hLog) renResult)
                 (case hLog of
                     Nothing   -> return ()
                     Just h    -> hClose h)

type RenameAction = Options -> RenameResult -> IO ()

buildShowAction, buildRenameAction :: Options -> [String] -> Maybe Handle -> RenameActionContext
buildShowAction o no h   = RenameActionContext o no h (const print)
buildRenameAction o no h = RenameActionContext o no h renameFile

handleArgumments :: [String] -> IO [RenameResult]
handleArgumments (p:f:fs) = rename p (f:fs)
handleArgumments _        = error "Pattern and files not supplied!"

renameFile :: RenameAction
renameFile opts renResult = do
                              let (on, nn) = (oldName renResult, newName renResult)

                              df <- D.doesFileExist nn
                              dd <- D.doesDirectoryExist nn

                              when ((df || dd) && not (optForce opts))
                                   (hPutStrLn stderr ("file [" ++ nn ++ "] already exists") >> exitFailure)

                              dof <- D.doesFileExist on
                              dod <- D.doesDirectoryExist on

                              when (not (dof || dod))
                                   (hPutStrLn stderr ("file [" ++ on ++ "] does not exists") >> exitFailure)

                              isDir <- isDirectory on
                              if isDir
                                then D.renameDirectory on nn
                                else D.renameFile on nn

applyToFiles :: RenameActionContext -> [RenameResult] -> IO ()
applyToFiles context renResult = do

                                   let doRename = not . optShowOnly $ getopts context

                                   when (doRename && (not . optForce $ getopts context) && isAnyForceError renResult)
                                        (hPutStrLn stderr "existing files detected" >> exitFailure)

                                   when (doRename && isAnyFatalError renResult)
                                        (hPutStrLn stderr "given pattern results in less files" >> exitFailure)

                                   let action' = exec context (getopts context)
                                       action  = case hLog context of
                                                      Nothing -> action'
                                                      Just h  -> (\r -> writeLine h r >> action' r)

                                   case hLog context of
                                        Nothing -> return ()
                                        Just h  -> writeInfo h (nonopts context)

                                   mapM_ action renResult
