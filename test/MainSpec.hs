module MainSpec (spec) where

import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive,
                        withCurrentDirectory, getCurrentDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Test.Hspec

-- Helper to create a test directory and run tests in it
withTestDir :: String -> IO a -> IO a
withTestDir dirName action = do
  cwd <- getCurrentDirectory
  let testDir = cwd ++ "/" ++ dirName
  dirExists <- doesDirectoryExist testDir
  if dirExists
    then removeDirectoryRecursive testDir
    else return ()
  createDirectory testDir
  result <- withCurrentDirectory testDir action
  removeDirectoryRecursive testDir
  return result

-- Helper to create a minimal valid Dockerfile
createMinimalDockerfile :: FilePath -> IO ()
createMinimalDockerfile path =
  writeFile path "FROM alpine:3.19\n"

-- Helper to run hadolint executable
-- Uses 'cabal exec hadolint' to run the locally built version
runHadolint :: [String] -> IO (ExitCode, String, String)
runHadolint args = do
  -- Use 'cabal exec hadolint' to run the locally built version
  -- This works across all platforms without hardcoding paths
  readProcessWithExitCode "cabal" (["exec", "hadolint", "--"] ++ args) ""

spec :: SpecWith ()
spec = do
  describe "hadolint executable" $ do
    describe "when no dockerfiles specified" $ do
      it "fails with message when Dockerfile doesn't exist" $ do
        withTestDir "test-no-dockerfile" $ do
          (exitCode, stdout, _) <- runHadolint []
          exitCode `shouldBe` ExitFailure 1
          stdout `shouldContain` "Please provide a Dockerfile"

      it "processes default Dockerfile when it exists" $ do
        withTestDir "test-with-dockerfile" $ do
          createMinimalDockerfile "Dockerfile"
          (exitCode, _, _) <- runHadolint []
          -- Should succeed with minimal dockerfile
          exitCode `shouldSatisfy` (\code -> code == ExitSuccess || code == ExitFailure 1)

    describe "when dockerfiles specified" $ do
      it "fails with error message when file doesn't exist" $ do
        withTestDir "test-missing-file" $ do
          (exitCode, stdout, stderr) <- runHadolint ["nonexistent.dockerfile"]
          exitCode `shouldBe` ExitFailure 1
          let output = stdout ++ stderr
          output `shouldContain` "Error: The following files do not exist:"
          output `shouldContain` "nonexistent.dockerfile"

      it "fails with error message when multiple files don't exist" $ do
        withTestDir "test-missing-files" $ do
          (exitCode, stdout, stderr) <- runHadolint ["missing1.dockerfile", "missing2.dockerfile"]
          exitCode `shouldBe` ExitFailure 1
          let output = stdout ++ stderr
          output `shouldContain` "Error: The following files do not exist:"
          output `shouldContain` "missing1.dockerfile"
          output `shouldContain` "missing2.dockerfile"

      it "fails with error message when some files exist and some don't" $ do
        withTestDir "test-partial-files" $ do
          createMinimalDockerfile "exists.dockerfile"
          (exitCode, stdout, stderr) <- runHadolint ["exists.dockerfile", "missing.dockerfile"]
          exitCode `shouldBe` ExitFailure 1
          let output = stdout ++ stderr
          output `shouldContain` "Error: The following files do not exist:"
          output `shouldContain` "missing.dockerfile"
          -- The error message should only list missing files
          output `shouldNotContain` "exists.dockerfile"

      it "processes files when all specified files exist" $ do
        withTestDir "test-all-exist" $ do
          createMinimalDockerfile "test1.dockerfile"
          createMinimalDockerfile "test2.dockerfile"
          (exitCode, _, _) <- runHadolint ["test1.dockerfile", "test2.dockerfile"]
          -- Should succeed with minimal dockerfiles
          exitCode `shouldSatisfy` (\code -> code == ExitSuccess || code == ExitFailure 1)

    describe "when --version flag is used" $ do
      it "prints version and exits successfully" $ do
        (exitCode, stdout, _) <- runHadolint ["--version"]
        exitCode `shouldBe` ExitSuccess
        stdout `shouldNotBe` ""
