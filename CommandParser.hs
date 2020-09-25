module CommandParser (CommandLineArgs(..), Options(..), Verbosity(..), parseArgs, sourceCodeHelpText, debuggerHelpText)
where

import Data.List
import Text.Read
import System.Console.GetOpt
import System.Environment

-- I don't think these two help text walls belong here but putting them in their respective modules creates cycles so... meh.
sourceCodeHelpText = "Turing machine source code file format: \n\
\  * The first line contains the input to the Turing machine, that is, the initial contents of the first tape. Letters are separated by whitespace.\n\
\  * The second line contains the list of allowed states of the machine, again separated by whitespace. The final state is the accepting state.\n\
\  * All subsequent lines denote the transition table of the machine. Each line contains, in that order:\n\
\    - Letters seen by the head of the machine on each tape\n\
\    - A transition for each state except the accepting state. The transition contains:\n\
\      # The new state of the machine;\n\
\      # For each tape: the letter to be written to the tape on the current position and either of <, > or ^, which is the direction the tape should move to (^ means the tape does not move)\n\
\ The transition table must be exhaustive: a transition should be specified for every possible state x letter^n combination, where n is the number of tapes.\n\
\ The first letter of the first line of the transition table specifies the blank character. The tapes are infinite from both sides and, unless overwritten, will be populated by copies of the blank character.\n\
\ Both letters and state symbols may consist of multiple non-whitespace characters ('letter' here is used in the sense of the element of the machine's alphabet).\n\
\ Empty lines are NOT allowed, except for the first line, which (if empty) means that the Turing machine receives no input.\n\
\ After the execution has completed, the contents of the last tape, from position 0 to the right, are interpreted as the output."

debuggerHelpText = "Debugger commands:\n\
\ * b - pause/unpause debugging.\n\
\ * p VERBOSITY - print machine state. Verbosity of 0 is not allowed. Default: 1\n\
\ * v VERBOSITY - set verbosity for automatic printing of debug messages. If ommitted, cycles among all three possible choices.\n\
\ * s STEP - only print automatic debug messages every (2^STEP)th machine steps. Default: 0\n\
\ * d DELAY - wait 2^DELAY microseconds after each machine step. Maximum: 20. -1 disables interactivity. Default: 20\n\
\ * h - display this help message\n"

data CommandLineArgs = CommandLineArgs {opts :: Options, inputFile :: FilePath} deriving Show

data Options = Options {verbosity :: Verbosity, skipStep :: Integer, delay :: Int} deriving Show
data Verbosity = NoOutput | SomeOutput | VerboseOutput deriving Show

options = [
 Option ['v'] ["verbosity"] (ReqArg parseVerbosity "VERBOSITY")
  "Set initial verbosity of debug messages. Possible values: 0 - no debug messages 1 - only machine state and tape contents the machine is currently looking at 2 - machine state as well as whole tape contents. Default: 0",
 Option ['s'] ["step"] (ReqArg parseSkipStep "STEP")
  "Only print debug messages every 2^STEP machine steps. Default: 0",
 Option ['d'] ["delay"] (ReqArg parseDelay "DELAY")
  "Pause execution for 2^DELAY microseconds every machine step. -1 disables pausing and disables interactivity. Maximum: 20. Default: -1",
 Option ['h'] ["help"] (NoArg parseDisplayHelp)
  "Display this message and exit." ]

defaultOptions = Options {verbosity = NoOutput, skipStep = 0, delay = -1}

commandLineError :: String -> String -> String
commandLineError errorMessage progName =
 errorMessage ++ "\n\n" ++ usage progName

usage :: String -> String
usage progName =
 usageInfo ("Usage: "++progName++" options inputFile\n\nOption descriptions:\n") options

parseCommand :: String -> [String] -> Either String CommandLineArgs
parseCommand progName args =
 parseOptions progName optionTransforms >>= handleGetOptOutput progName (nonOptions, unknownOptions, errs)
 where (optionTransforms, nonOptions, unknownOptions, errs) = getOpt' Permute options args

handleGetOptOutput :: String -> ([String], [String], [String]) -> Options -> Either String CommandLineArgs
handleGetOptOutput progName (_, _, errs@(_:_)) _ = Left $ commandLineError (concat errs) progName
handleGetOptOutput progName (_, spurious@(_:_), _) _ = Left $ commandLineError (intercalate " " ("Uncrecognized option(s):":spurious)) progName
handleGetOptOutput progName ([], _, _) _ = Left $ commandLineError "Expected input file name." progName
handleGetOptOutput progName ((_:_:_), _, _) _ = Left $ commandLineError "Expected only one input file." progName
handleGetOptOutput progName ([inputFile], [], []) opts = Right $ CommandLineArgs {opts = opts, inputFile = inputFile}

parseOptions :: String -> [String -> Options -> Either String Options] -> Either String Options
parseOptions progName transforms =
 foldr (=<<) (Right defaultOptions) (map ($ progName) transforms)

parseVerbosity :: String -> String -> Options -> Either String Options
parseVerbosity str progName opts = 
 case verbosity
  of Just 0 -> Right opts {verbosity=NoOutput}
     Just 1 -> Right opts {verbosity=SomeOutput}
     Just 2 -> Right opts {verbosity=VerboseOutput}
     _ -> Left $ commandLineError "Invalid verbosity specifier" progName
 where verbosity = readMaybe str

parseSkipStep :: String -> String -> Options -> Either String Options
parseSkipStep str progName opts =
 case step
  of Just n -> if n >= 0 then Right opts {skipStep = n} else Left $ commandLineError "Invalid step" progName
     _ -> Left $ commandLineError "Invalid step" progName
 where step = readMaybe str

parseDelay :: String -> String -> Options -> Either String Options
parseDelay str progName opts =
 case delay
  of Just n -> if n >= -1 && n <= 20 then Right opts {delay = n} else Left $ commandLineError "Invalid delay" progName
     _ -> Left $ commandLineError "Invalid delay" progName
 where delay = readMaybe str

parseDisplayHelp :: String -> Options -> Either String Options
parseDisplayHelp progName _ =
 Left $ usage progName ++ "\n" ++ sourceCodeHelpText ++ "\n\nUnless interactivity is disabled, during interpretation you can enter commands.\n" ++ debuggerHelpText

parseArgs :: IO (Either String CommandLineArgs)
parseArgs =
 do args <- getArgs
    progName <- getProgName
    return $ parseCommand progName args
