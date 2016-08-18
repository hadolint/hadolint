module Main where

import Hadolint.Parser
import Hadolint.Rules
import Hadolint.Syntax
import Hadolint.Format

import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)
import System.Exit hiding (die)
import Data.List (sort)
import Data.Aeson (encode)
import Text.Parsec (ParseError)
import Control.Applicative
import Options.Applicative hiding (ParseError)


type IgnoreRule = String
data LintOptions = LintOptions { useJsonFormat :: Bool
                               , codeClimate :: Bool
                               , ignoreRules :: [IgnoreRule]
                               , dockerfile :: String
                               }

ignoreFilter :: [IgnoreRule] -> Check -> Bool
ignoreFilter ignoredRules (Check (Metadata code _ _) _ _ _) = code `notElem` ignoredRules


printChecks :: Bool -> [Check] -> IO ()
printChecks useJson checks = do
    mapM_ (if useJson then jsonOutput else plainOutput) $ sort checks
    if null checks then exit else die
  where
    jsonOutput c = BS.putStrLn $ encode c
    plainOutput = print

parseOptions :: Parser LintOptions
parseOptions = LintOptions
    <$> switch (long "json" <> help "Format output as JSON")
    <*> switch (long "code-climate" <> help "Format output as CodeClimate compatible JSON")
    <*> many (strOption (long "ignore" <> help "Ignore rule" <> metavar "RULECODE"))
    <*> argument str (metavar "DOCKERFILE")

main :: IO ()
main = execParser opts >>= lint
    where
        opts = info (helper <*> parseOptions)
          ( fullDesc
         <> progDesc "Lint Dockerfile for errors and best practices"
         <> header "hadolint - Dockerfile Linter written in Haskell" )

-- | Support UNIX convention of passing "-" instead of "/dev/stdin" 
parseFilename :: String -> String
parseFilename "-" = "/dev/stdin"
parseFilename s = s

lint :: LintOptions -> IO ()
lint (LintOptions useJsonFormat _ ignoreRules dockerfile) = do
   ast <- parseFile $ parseFilename dockerfile
   case ast of
        Left err         -> print err >> exit
        Right dockerfile -> printChecks useJsonFormat $ filter (ignoreFilter ignoreRules) $ analyzeAll dockerfile

analyzeAll = analyze rules

-- Helper to analyze AST quickly in GHCI
analyzeEither (Left err) = []
analyzeEither (Right dockerfile)  = analyzeAll dockerfile

usage   = putStrLn "Usage: hadolint [-vhi] <file>"
version = putStrLn "Haskell Dockerfile Linter v1.1"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)
