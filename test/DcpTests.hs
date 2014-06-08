module Main where

import Control.Monad
import Prelude hiding (FilePath)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

buildDir :: FilePath
buildDir = "dist" </> "dcptests"

compileCmd :: Bool -> String -> String
compileCmd True  = (++) $ rmCmd ++
  "; ghc -isrc -outputdir " ++ buildDir ++ " -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d "
compileCmd False = (++) $ rmCmd ++ "; ghc -isrc -outputdir" ++ buildDir ++ " "

rmCmd :: String
rmCmd = "rm -rf " ++ buildDir

dcpTestsDir :: FilePath
dcpTestsDir = "test" </> "DcpTests"

-- Tests in which DCP rules are satisfied.
dcpTestsThatShouldCompile :: [FilePath]
dcpTestsThatShouldCompile = map (dcpTestsDir </>)
  [ "OkAffineAffine.hs"
  , "OkConcaveNondecConcave.hs"
  , "OkConvexNondecConvex.hs"
  , "OkXAffine.hs"
  , "OkConstraints.hs" ]

-- Tests in which DCP rules are not satisfied.
dcpTestsThatShouldn'tCompile :: [FilePath]
dcpTestsThatShouldn'tCompile = map (dcpTestsDir </>)
  [ "NoAffineNonmonConcaveNondec.hs"
  , "NoAffineNonmonConvexNondec.hs"
  , "NoConvexNondecConcave.hs"
  , "NoConvexGeq.hs"
  , "NoConcaveLeq.hs" ]

showExitCodeAndPath :: Bool -> ExitCode -> FilePath -> String
showExitCodeAndPath shouldPass code path = codeStr ++ " " ++ path
  where
    codeStr = if ExitSuccess == code
      then sp ++ "SUCCESS" ++ sp
      else fp ++ "FAILURE" ++ fp
    sp = if shouldPass then "  " else "!!"
    fp = if shouldPass then "!!" else "  "

main :: IO ExitCode
main = do
  createDirectoryIfMissing True buildDir
  inSandbox <- doesFileExist "cabal.sandbox.config"

  putStrLn "Exit codes for DCP tests that should successfully typecheck:"
  let compilationsThatShouldSucceed = map (compileCmd inSandbox) dcpTestsThatShouldCompile
  exitcodesSuccess <- mapM system compilationsThatShouldSucceed
  mapM_ putStrLn $ zipWith (showExitCodeAndPath True)
                        exitcodesSuccess
                        compilationsThatShouldSucceed

  putStrLn ""
  putStrLn "Exit codes for DCP tests that shouldn't successfully typecheck:"
  let compilationsThatShouldn'tSucceed = map (compileCmd inSandbox) dcpTestsThatShouldn'tCompile
  exitcodesFailure <- mapM system compilationsThatShouldn'tSucceed
  mapM_ putStrLn $ zipWith (showExitCodeAndPath False)
                        exitcodesFailure
                        compilationsThatShouldn'tSucceed

  if any (ExitSuccess /=) exitcodesSuccess || elem ExitSuccess exitcodesFailure
    then exitFailure
    else exitSuccess
