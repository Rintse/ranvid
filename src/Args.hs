module Args where

import Syntax.Grammar.Abs
import Syntax.Parse

import System.Console.GetOpt
import Control.Exception
import System.IO as IO 
import System.Exit
import System.Environment
import Data.Char
import Text.Read (readMaybe)
import Data.Maybe

-- The option list
data Options = Options
    { optVerbose    :: Bool
    , optSeedHash   :: String
    , optInput      :: IO String }

-- The default options
defaultOpts :: Options
defaultOpts = Options  
    { optVerbose    = False
    , optInput      = getContents }

type ParseMonad a = IO (Either SomeException a)

-- Reads a file if such an argument is given
readFile :: String -> Options -> IO Options
readFile arg opt = do
    file <- try (IO.readFile arg) :: ParseMonad String
    case file of
        Left ex -> do 
            putStrLn $ "Error opening file:\n" ++ show ex
            exitFailure
        Right content -> return opt { optInput = return content }

-- Sets the verbosity
readVerb :: Options -> IO Options
readVerb opt = return opt { optVerbose = True }

readSeed :: String -> Options -> IO Options
readSeed arg opt = return opt { optSeedHash = arg }

-- Outputs a help message
putHelp :: Options -> IO Options
putHelp opt = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

-- The options as functions to be threaded through
options :: [ OptDescr (Options -> IO Options) ]
options = 
    [ Option "i" ["input"] (ReqArg Args.readFile "FILE") 
        "Input file"

    , Option "s" ["seed-hash"] (ReqArg readSeed "HASH") 
        "The hash to seed the RNG with"

    , Option "v" ["verbose"] (NoArg readVerb) 
        "Enable verbose parsing"

    , Option "h" ["help"] (NoArg putHelp) 
        "Display help message" 
    ]

