module Main where

import Control.Monad
import Prelude hiding (FilePath)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

compileCmd :: Bool -> String -> String
compileCmd True = (++)
  "ghc -isrc -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d "
compileCmd False = ("ghc -isrc " ++)

dcpTestsDir :: FilePath
dcpTestsDir = "test" </> "DcpTests"

-- Tests in which DCP rules are satisfied.
dcpTestsThatShouldCompile :: [FilePath]
dcpTestsThatShouldCompile = map (dcpTestsDir </>)
  [ "ConvexNondecConvexNondecAllowed.hs" ]

-- Tests in which DCP rules are not satisfied.
dcpTestsThatShouldn'tCompile :: [FilePath]
dcpTestsThatShouldn'tCompile = map (dcpTestsDir </>)
  [ "ConvexNondecConcaveDisallowed.hs" ]

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
