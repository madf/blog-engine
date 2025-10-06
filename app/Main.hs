module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Data.Version (showVersion)
import Data.Text
import Madf.Blog
import Madf.Blog.Env
import Paths_mbe (version)

versionString :: String
versionString = showVersion version

printVersion :: IO ()
printVersion = putStrLn $ "mbe version " ++ versionString

printHelp :: IO ()
printHelp = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [OPTIONS]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  -h, --help              Show this help message"
    putStrLn "  -v, --version           Show version information"
    putStrLn "  -c, --config FILE       Path to configuration file"
    putStrLn "      --regen-key         Regenerate JWT key (invalidates all tokens)"
    putStrLn ""
    putStrLn "If no config file is specified, uses default configuration."

data RunOptions = RunOptions
    { configFile  :: !(Maybe FilePath)
    , regenKey    :: !Bool
    }

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions
    { configFile = Nothing
    , regenKey = False
    }

data Action
    = ShowHelp
    | ShowVersion
    | Run RunOptions
    | ParseError String

parseArgs :: [String] -> Action
parseArgs = parseArgs' defaultRunOptions
  where
    parseArgs' :: RunOptions -> [String] -> Action
    parseArgs' opts [] = Run opts
    parseArgs' _ ("-h":_) = ShowHelp
    parseArgs' _ ("--help":_) = ShowHelp
    parseArgs' _ ("-v":_) = ShowVersion
    parseArgs' _ ("--version":_) = ShowVersion
    parseArgs' opts ("-c":path:rest) = parseArgs' (opts { configFile = Just path }) rest
    parseArgs' opts ("--config":path:rest) = parseArgs' (opts { configFile = Just path }) rest
    parseArgs' opts ("--regen-key":rest) = parseArgs' (opts { regenKey = True }) rest
    parseArgs' _ ["-c"] = ParseError "Option -c requires an argument"
    parseArgs' _ ["--config"] = ParseError "Option --config requires an argument"
    parseArgs' _ (unknown:_) = ParseError $ "Unknown option: " ++ unknown

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        ShowHelp -> printHelp >> exitSuccess
        ShowVersion -> printVersion >> exitSuccess
        Run opts -> do
            env <- case configFile opts of
                Nothing -> defaultEnv
                Just fp -> create (pack fp) (regenKey opts)
            serve env
        ParseError err -> do
            putStrLn $ "Error: " ++ err
            putStrLn "Use --help for usage information"
            exitWith (ExitFailure 1)
