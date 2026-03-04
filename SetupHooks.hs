{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Distribution.Simple.Flag (flagToMaybe, fromFlag)
import Distribution.Simple.LocalBuildInfo (mbWorkDirLBI, withPrograms)
import Distribution.Simple.Program
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Utils.Path
import Distribution.Utils.ShortText (toShortText)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (replaceExtension, takeBaseName, takeDirectory, takeExtension, takeFileName)

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks = myConfigureHooks
    , buildHooks = myBuildHooks
    }

-- ============================================================================
-- CONFIGURE HOOKS: Declare that we will generate these C files
-- ============================================================================

myConfigureHooks :: ConfigureHooks
myConfigureHooks =
  noConfigureHooks
    { preConfComponentHook = Just declareGeneratedCSources
    }

-- Add generated C files to the component's build info during configuration.
-- This ensures Cabal knows these files are needed and will demand the rules.
declareGeneratedCSources :: PreConfComponentInputs -> IO PreConfComponentOutputs
declareGeneratedCSources pcci@PreConfComponentInputs{packageBuildDescr = pbd, localBuildConfig = lbc, component = comp} = do
  let cfg = LBC.configFlags pbd
      mbWorkDir = flagToMaybe $ configWorkingDir cfg
      verbosity = fromFlag $ configVerbosity cfg

      -- Directory paths
      protoDir = makeSymbolicPath "protocols"
      genDir = makeSymbolicPath "generated"
      cDir = makeSymbolicPath "c_src"

      -- Absolute paths for scanning
      protoDirAbs = interpretSymbolicPath mbWorkDir protoDir
      genDirAbs = interpretSymbolicPath mbWorkDir genDir
      cDirAbs = interpretSymbolicPath mbWorkDir cDir

  -- Ensure generated directory exists for scanning
  createDirectoryIfMissing True genDirAbs

  -- Find all XML protocol files to know what we'll generate
  xmlFiles <- filter (".xml" `isSuffixOf`) <$> listDirectory protoDirAbs
  cFiles <- filter (".c" `isSuffixOf`) <$> listDirectory cDirAbs
  let shortNames = map takeBaseName xmlFiles
      -- The C files we will generate
      generatedCFiles = map (\n -> ("generated" :: FilePath) </> n ++ ".c") shortNames
      writtenCFiles = map ((\n -> ("c_src" :: FilePath) </> n) . takeFileName) cFiles
      -- The master C file
      masterCFile = ("generated" :: FilePath) </> "all_protocols.c"
      allCFiles = (masterCFile : generatedCFiles) ++ writtenCFiles
  -- The header files (for include dirs, not compilation)
  -- generatedHFiles = map (\n -> ("generated" :: FilePath) </> n ++ ".h") shortNames

  -- Create BuildInfo with the generated C sources
  let bi =
        emptyBuildInfo
          { cSources = map makeSymbolicPath allCFiles
          -- , includeDirs = [genDir] -- For the generated headers
          }
  print bi
  return $
    (noPreConfComponentOutputs pcci)
      { componentDiff = buildInfoComponentDiff (componentName comp) bi
      }

-- ============================================================================
-- BUILD HOOKS: Rules to actually generate the files
-- ============================================================================

myBuildHooks :: BuildHooks
myBuildHooks =
  noBuildHooks
    { preBuildComponentRules = Just $ rules (static ()) waylandProtocolRules
    }

-- | Type for arguments passed to wayland-scanner invocations
type WaylandScannerArgs =
  ( Verbosity
  , Maybe (SymbolicPath CWD (Dir Pkg))
  , ConfiguredProgram -- wayland-scanner
  , SymbolicPath Pkg File -- input XML
  , SymbolicPath Pkg File -- output code
  )

waylandProtocolRules :: PreBuildComponentInputs -> RulesM ()
waylandProtocolRules pbci@PreBuildComponentInputs{buildingWhat, localBuildInfo = lbi} = do
  let verbosity = buildingWhatVerbosity buildingWhat
      mbWorkDir = mbWorkDirLBI lbi
      progDb = withPrograms lbi

      -- Directory paths
      protoDir = makeSymbolicPath "protocols"
      genDir = makeSymbolicPath "generated"

      -- Absolute paths for filesystem operations
      protoDirAbs = interpretSymbolicPath mbWorkDir protoDir
      genDirAbs = interpretSymbolicPath mbWorkDir genDir

  -- Create generated directory
  liftIO $ createDirectoryIfMissing True genDirAbs

  -- Find all XML protocol files
  xmlFiles <- liftIO $ filter (".xml" `isSuffixOf`) <$> listDirectory protoDirAbs
  let shortNames = map (takeBaseName . takeBaseName) xmlFiles -- Remove .xml
      p = simpleProgram "wayland-scanner"
  newProgDb <- liftIO $ configureProgram verbosity p progDb
  -- Look up wayland-scanner in the program database
  case lookupProgram p newProgDb of
    Nothing -> do
      -- wayland-scanner not found, but build-tool-depends should provide it
      liftIO $ putStrLn "Hello"
      return ()
    Just scannerProg -> do
      -- Register rules for each protocol file
      forM_ (zip xmlFiles shortNames) $ \(xmlFile, name) -> do
        let inputXml = protoDir </> makeRelativePathEx xmlFile
            outputH = genDir </> makeRelativePathEx (name ++ ".h")
            outputC = genDir </> makeRelativePathEx (name ++ ".c")

        -- -- Rule for generating header file
        -- registerRule_ ("wayland-header:" <> toShortText name) $
        --   staticRule
        --     (waylandHeaderCmd (verbosity, mbWorkDir, scannerProg, inputXml, outputH))
        --     [FileDependency (Location protoDir (makeRelativePathEx xmlFile))]
        --     (NE.singleton (Location genDir (makeRelativePathEx (name ++ ".h"))))
        --
        -- -- Rule for generating code file
        -- registerRule_ ("wayland-code:" <> toShortText name) $
        --   staticRule
        --     (waylandCodeCmd (verbosity, mbWorkDir, scannerProg, inputXml, outputC))
        --     [FileDependency (Location protoDir (makeRelativePathEx xmlFile))]
        --     (NE.singleton (Location genDir (makeRelativePathEx (name ++ ".c"))))
        liftIO $ runWaylandHeader (verbosity, mbWorkDir, scannerProg, inputXml, outputH)
        liftIO $ runWaylandHeader (verbosity, mbWorkDir, scannerProg, inputXml, outputC)

      -- Register rule for master C file that includes all generated .c files
      let masterC = genDir </> makeRelativePathEx "all_protocols.c"
          cFiles = map (\n -> n ++ ".c") shortNames

      -- registerRule_ "wayland-master-c" $
      --   staticRule
      --     (writeMasterCFile (verbosity, mbWorkDir, genDir, cFiles, masterC))
      --     -- Depend on all individual .c files being generated first
      --     (map (\n -> FileDependency (Location genDir (makeRelativePathEx (n ++ ".c")))) shortNames)
      --     (NE.singleton (Location genDir (makeRelativePathEx "all_protocols.c")))
      liftIO $ runWriteMasterC (verbosity, mbWorkDir, genDir, cFiles, masterC)

-- | Command to generate client header
waylandHeaderCmd :: WaylandScannerArgs -> Command WaylandScannerArgs (IO ())
waylandHeaderCmd args@(verbosity, mbWorkDir, scanner, inputXml, outputH) =
  mkCommand (static Dict) (static runWaylandHeader) args

runWaylandHeader :: WaylandScannerArgs -> IO ()
runWaylandHeader (verbosity, mbWorkDir, scanner, inputXml, outputH) = do
  let inputPath = interpretSymbolicPath mbWorkDir inputXml
      outputPath = interpretSymbolicPath mbWorkDir outputH
  createDirectoryIfMissingVerbose verbosity True (takeDirectory outputPath)
  runProgramCwd
    verbosity
    mbWorkDir
    scanner
    [ "client-header"
    , getSymbolicPath inputXml
    , getSymbolicPath outputH
    ]

-- | Command to generate private code
waylandCodeCmd :: WaylandScannerArgs -> Command WaylandScannerArgs (IO ())
waylandCodeCmd args@(verbosity, mbWorkDir, scanner, inputXml, outputC) =
  mkCommand (static Dict) (static runWaylandCode) args

runWaylandCode :: WaylandScannerArgs -> IO ()
runWaylandCode (verbosity, mbWorkDir, scanner, inputXml, outputC) = do
  let inputPath = interpretSymbolicPath mbWorkDir inputXml
      outputPath = interpretSymbolicPath mbWorkDir outputC
  createDirectoryIfMissingVerbose verbosity True (takeDirectory outputPath)
  runProgramCwd
    verbosity
    mbWorkDir
    scanner
    [ "private-code"
    , getSymbolicPath inputXml
    , getSymbolicPath outputC
    ]

-- | Type for master C file generation
type MasterCArgs =
  ( Verbosity
  , Maybe (SymbolicPath CWD (Dir Pkg))
  , SymbolicPath Pkg (Dir Pkg) -- genDir
  , [String] -- c file names
  , SymbolicPath Pkg File -- master output
  )

writeMasterCFile :: MasterCArgs -> Command MasterCArgs (IO ())
writeMasterCFile args =
  mkCommand (static Dict) (static runWriteMasterC) args

runWriteMasterC :: MasterCArgs -> IO ()
runWriteMasterC (verbosity, mbWorkDir, genDir, cFiles, masterC) = do
  let masterPath = interpretSymbolicPath mbWorkDir masterC
      includes = map (\f -> "#include \"" ++ f ++ "\"") cFiles
      content = unlines includes

  createDirectoryIfMissingVerbose verbosity True (takeDirectory masterPath)
  writeFile masterPath content
