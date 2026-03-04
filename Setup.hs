import Control.Monad (forM_)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.HookedBuildInfo
import Distribution.Types.PackageDescription

import Distribution.Utils.Path (makeSymbolicPath)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Process (callProcess)

genDir :: FilePath
genDir = "generated"
protoDir :: FilePath
protoDir = "protocols"
cDir :: FilePath
cDir = "c_src"

main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          generateWaylandProtocols
          preConf simpleUserHooks args flags
      , buildHook = \pd lbi uhs flags ->
          insertCFiles pd lbi uhs flags
      }

generateWaylandProtocols :: IO ()
generateWaylandProtocols = do
  createDirectoryIfMissing True genDir

  allFiles <- listDirectory protoDir
  let xmlFiles = filter (\f -> takeExtension f == ".xml") allFiles
      shortNames = ((\f -> take (length f - 3) f) . takeBaseName) <$> xmlFiles

  forM_ (zip xmlFiles shortNames) $ \(xmlFile, name) -> do
    let protoPath = protoDir </> xmlFile
        headerOut = genDir </> (name ++ ".h")
        codeOut = genDir </> (name ++ ".c")

    callProcess "wayland-scanner" ["client-header", protoPath, headerOut]
    callProcess "wayland-scanner" ["private-code", protoPath, codeOut]

insertCFiles :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
insertCFiles pd@PackageDescription{executables} lbi uhs flags = do
  allGenFiles <- listDirectory genDir
  allCFiles <- listDirectory cDir
  let
    genFiles = (makeSymbolicPath . (genDir </>)) <$> filter (\f -> takeExtension f == ".c") allGenFiles
    cFiles = (makeSymbolicPath . (cDir </>)) <$> filter (\f -> takeExtension f == ".c") allCFiles
    genDirSymbolic = makeSymbolicPath genDir
    masterCFile = makeSymbolicPath "generated/all_protocols.c"
    newExecutables =
      fmap
        ( \e@Executable{buildInfo = b@BuildInfo{cSources, includeDirs}} ->
            e
              { buildInfo =
                  b
                    { cSources = genFiles ++ cFiles ++ cSources
                    , includeDirs = genDirSymbolic : includeDirs
                    }
              }
        )
        executables

  buildHook simpleUserHooks pd{executables = newExecutables} lbi uhs flags
