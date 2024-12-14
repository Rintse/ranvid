module Args ( getOpts, Options(..)) where

import Control.Monad ( unless )
import System.Console.GetOpt
import Control.Exception
import System.IO as IO
import System.Exit
import System.Environment
import System.Random ( uniformR, mkStdGen, randomIO )
import Data.Char

printableChars :: (Int, Int)
printableChars = (48, 121)

defaultCanvasSize :: (Int, Int)
defaultCanvasSize = (300, 300)

randomSeed :: IO String
randomSeed = do
    global_seed <- (randomIO :: IO Int)
    let rListRec g = let (r, g2) = uniformR printableChars g in r : rListRec g2
    return $ map chr $ take 8 $ rListRec (mkStdGen global_seed)

-- The option list
data Options = Options
    { optVerbose    :: Bool
    , optSize       :: (Int, Int)
    , optSeedHash   :: String
    , optInputFile  :: Maybe String }

-- The default options
defaultOpts :: IO Options
defaultOpts = do
    seed <- randomSeed
    return $ Options {
        optVerbose = False,
        optSize = defaultCanvasSize,
        optSeedHash = seed,
        optInputFile = Nothing
    }

type ParseMonad a = IO (Either SomeException a)

-- Reads a file if such an argument is given
readFile :: String -> Options -> IO Options
readFile arg opt = do
    file <- try (IO.readFile arg) :: ParseMonad String
    case file of
        Left ex -> do
            putStrLn $ "Error opening file:\n" ++ show ex
            exitFailure
        Right content -> return opt { optInputFile = Just content }

-- Sets the verbosity
readVerb :: Options -> IO Options
readVerb opt = return opt { optVerbose = True }

readSeed :: String -> Options -> IO Options
readSeed arg opt = return opt { optSeedHash = arg }

-- Outputs a help message
putHelp :: Options -> IO Options
putHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

-- The options as functions to be threaded through
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg Args.readFile "FILE")
        "Input file"

    , Option "s" ["seed-hash"] (ReqArg Args.readSeed "HASH")
        "The hash to seed the RNG with"

    , Option "v" ["verbose"] (NoArg readVerb)
        "Enable verbose parsing"

    , Option "h" ["help"] (NoArg putHelp)
        "Display help message"
    ]

getOpts :: IO Options
getOpts = do
    args <- getArgs -- Get and parse options
    let (optArgs, _nonOpts, errs) = getOpt RequireOrder options args

    unless (null errs) ( do
        putStrLn "The were errors parsing the arguments:"
        mapM_ putStr errs >> exitFailure )

    foldl (>>=) defaultOpts optArgs
