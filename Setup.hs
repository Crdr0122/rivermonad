import Control.Monad (forM_)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Process (callProcess)

main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = \args flags -> do
          generateWaylandProtocols
          preBuild simpleUserHooks args flags
      }

generateWaylandProtocols :: IO ()
generateWaylandProtocols = do
  let protoDir = "protocols"
      genDir = "generated"
  createDirectoryIfMissing True genDir

  allFiles <- listDirectory protoDir
  let xmlFiles = filter (\f -> takeExtension f == ".xml") allFiles
      shortNames = ((\f -> take (length f - 3) f) . takeBaseName) <$> xmlFiles

  forM_ (zip xmlFiles shortNames) $ \(xmlFile, name) -> do
    let protoPath = protoDir </> xmlFile
        headerOut = genDir </> (name ++ ".h")
        codeOut = genDir </> (name ++ ".c")

    putStrLn $ "  Scanning " ++ xmlFile
    callProcess "wayland-scanner" ["client-header", protoPath, headerOut]
    callProcess "wayland-scanner" ["private-code", protoPath, codeOut]

  -- Create the Master C File
  let cFiles = map (++ ".c") shortNames
  let masterCPath = genDir </> "all_protocols.c"
  let masterCContent = unlines $ map (\f -> "#include \"" ++ f ++ "\"") cFiles

  putStrLn $ "Updating " ++ masterCPath
  writeFile masterCPath masterCContent
