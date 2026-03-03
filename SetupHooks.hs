{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks (setupHooks) where

import qualified Data.List.NonEmpty as NE
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import Distribution.Simple.LocalBuildInfo (mbWorkDirLBI)
import Distribution.Simple.Program (ConfiguredProgram)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Utils.Path (CWD, FileOrDir (..), Pkg, SymbolicPath, interpretSymbolicPath, makeRelativePathEx, makeSymbolicPath)
import Distribution.Verbosity (Verbosity, normal)
import System.FilePath ((</>))

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just $ rules (static ()) waylandScannerRule
          }
    }

-- | The Rule Definition
waylandScannerRule :: PreBuildComponentInputs -> RulesM ()
waylandScannerRule pbci@PreBuildComponentInputs{buildingWhat, localBuildInfo} = do
  -- In the official style, we extract standard info from the Component Inputs
  let mbWorkDir = mbWorkDirLBI localBuildInfo
      verbosity = buildingWhatVerbosity buildingWhat
      -- Assuming your project root is the base
      autoDir = autogenPackageModulesDir localBuildInfo
      srcDir = makeSymbolicPath "protocols"
      -- genDir = makeSymbolicPath "gen"

  -- Define the command using static pointers
  -- This is the 'alexCmd' equivalent
  let scannerCmd = mkCommand (static Dict) (static runWaylandScanner)

  -- Define Locations (The 'lexerInFile' equivalent)
  let xmlIn = Location srcDir (makeRelativePathEx "river-window-management-v1.xml")
      headerOut = Location autoDir (makeRelativePathEx "wayland-client-protocol.h")
      codeOut = Location autoDir (makeRelativePathEx "wayland-protocol.c")

  -- Register the rule with Cabal's dependency tracker
  registerRule_ "wayland-scanner:core" $
    staticRule
      (scannerCmd (verbosity, mbWorkDir, xmlIn, headerOut, codeOut))
      [FileDependency xmlIn] -- Inputs
      (NE.fromList [headerOut, codeOut]) -- Outputs (Must be NonEmpty)

{- | The Execution Logic (The 'runAlex' equivalent)
This must be a top-level function for 'static' to work.
-}
runWaylandScanner :: (Verbosity, Maybe (SymbolicPath CWD ('Dir Pkg)), Location, Location, Location) -> IO ()
runWaylandScanner (verbosity, mbWorkDir, xmlIn, headerOut, codeOut) = do
  pure ()
  -- Convert Location to actual FilePath strings for the OS
  -- 'Nothing' here assumes the current working directory
  let xmlPath = interpretSymbolicPath Nothing (location xmlIn)
      headerPath = interpretSymbolicPath Nothing (location headerOut)
      codePath = interpretSymbolicPath Nothing (location codeOut)

  -- 1. Generate Header
  rawSystemExit
    verbosity
    mbWorkDir
    "wayland-scanner"
    ["client-header", xmlPath, headerPath]

  -- 2. Generate Private Code
  rawSystemExit
    verbosity
    mbWorkDir
    "wayland-scanner"
    ["private-code", xmlPath, codePath]
