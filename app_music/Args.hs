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

defaultCanvasSize :: Int
defaultCanvasSize = 16

randomSeed :: IO String
randomSeed = do
    global_seed <- (randomIO :: IO Int)
    let rListRec g = let (r, g2) = uniformR printableChars g in r : rListRec g2
    return $ map chr $ take 8 $ rListRec (mkStdGen global_seed)

-- The option list
data Options = Options
    { optSize       :: Int
    , optParallel   :: Int
    , optSeedHash   :: String
    , optInputFile  :: Maybe String
    , optOutputFile :: Maybe String }

-- The default options
defaultOpts :: IO Options
defaultOpts = do
    seed <- randomSeed
    return $ Options {
        optSize = defaultCanvasSize,
        optParallel = 1,
        optSeedHash = seed,
        optInputFile = Nothing,
        optOutputFile = Nothing
    }

type ParseMonad a = IO (Either SomeException a)

-- Reads a file if such an argument is given
readInFile :: String -> Options -> IO Options
readInFile arg opt = do
    file <- try (IO.readFile arg) :: ParseMonad String
    case file of
        Left ex -> do
            putStrLn $ "Error opening file:\n" ++ show ex
            exitFailure
        Right content -> return opt { optInputFile = Just content }

readOutFile :: String -> Options -> IO Options
readOutFile arg opt = return opt { optOutputFile = Just arg }

readSeed :: String -> Options -> IO Options
readSeed arg opt = return opt { optSeedHash = arg }

readParallel :: String -> Options -> IO Options
readParallel arg opt = return opt { optParallel = read arg }

readSize :: String -> Options -> IO Options
readSize arg opt = return opt { optSize = read arg }

-- Outputs a help message
putHelp :: Options -> IO Options
putHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

-- The options as functions to be threaded through
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"] (ReqArg Args.readInFile "FILE")
        "Input file"

    , Option "o" ["output"] (ReqArg Args.readOutFile "FILE")
        "Output file (opens generated image if not specified)"

    , Option "s" ["seed-hash"] (ReqArg Args.readSeed "HASH")
        "The hash to seed the RNG with"

    , Option "p" ["parallel"] (ReqArg Args.readParallel "NUMTHREADS")
        "The amount of threads to run in parallel when evaluating the image"

    , Option "S" ["size"] (ReqArg Args.readSize "SIZE")
        "The size of the MIDI to generate (amount of 8th notes)"

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
