module Debugger (debugMachine, defaultDebuggerConfiguration, debuggerHelpText)
where

import qualified Text.Read as R
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Concurrent
import Control.Monad

import Interpreter
import CommandParser
import Utils

data DebuggerConfiguration = DebuggerConfiguration{options::Options, stepNo::Integer, paused::Bool}

defaultDebuggerConfiguration :: Options -> DebuggerConfiguration
defaultDebuggerConfiguration opts = DebuggerConfiguration{options=opts, stepNo=0, paused=False}

debugMachine :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) String
debugMachine =
 do finished <- isFinished <$> lift get
    if finished
    then showLastTapeContent <$> lift get
    else do stepDebug
            debugMachine

stepDebug :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
stepDebug =
 do delayDebugging
    printDebug
    lift $ modify stepMachine
    parseDebugCommands
    pauseDebugger

delayDebugging :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
delayDebugging =
 do DebuggerConfiguration{options=Options{delay=d}} <- get
    when (d > -1) $ liftIO $ threadDelay $ 2^d

printDebug :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
printDebug =
 do d@DebuggerConfiguration{options=Options{skipStep=ss, verbosity=v}, stepNo=s} <- get
    if s+1 >= 2^ss
    then do put d{stepNo = 0}
            liftIO . printDebugConfiguration v =<< lift get
    else put d{stepNo = s+1}
    
printDebugConfiguration :: Verbosity -> MachineConfiguration -> IO()
printDebugConfiguration NoOutput _ = return ()
printDebugConfiguration SomeOutput conf = putStrLn $ showSimpleConfiguration conf
printDebugConfiguration VerboseOutput conf = putStrLn $ showDetailedConfiguration conf

pauseDebugger :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
pauseDebugger = do
 isPaused <- gets paused
 when isPaused $
  do liftIO getLine >>= parseDebugCommand
     pauseDebugger

parseDebugCommands :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
parseDebugCommands =
 do isInteractive <- gets $ (/= -1) . delay . options
    when isInteractive $
     do cmds <- liftIO $ getImmediatelyAvailableLines
        mapM_ parseDebugCommand cmds

parseDebugCommand :: String -> StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
parseDebugCommand cmd =
 case words cmd
  of ["b"] -> togglePause
     ["p"] -> printOnDemand "1"
     ["p", arg] -> printOnDemand arg
     ["v"] -> toggleVerbosity
     ["v", arg] -> setVerbosity arg
     ["s"] -> setSkipStep "0"
     ["s", arg] -> setSkipStep arg
     ["d"] -> setDelay "20"
     ["d", arg] -> setDelay arg
     ["h"] -> liftIO $ putStrLn debuggerHelpText
     _ -> liftIO $ debugCommandError $ "Invalid command: " ++ cmd

printOnDemand :: String -> StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
printOnDemand "1" = liftIO . printDebugConfiguration SomeOutput =<< lift get
printOnDemand "2" = liftIO . printDebugConfiguration VerboseOutput =<< lift get
printOnDemand arg = liftIO $ putStrLn $ "Invalid argument to p: " ++ arg

toggleVerbosity :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
toggleVerbosity =
 do conf@DebuggerConfiguration{options=opts@Options{verbosity=v}} <- get
    case v
     of NoOutput -> setVerbosity "1"
        SomeOutput -> setVerbosity "2"
        VerboseOutput -> setVerbosity "0"

setVerbosity :: String -> StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
setVerbosity arg =
 do conf@DebuggerConfiguration{options=opts} <- get
    let n = R.readMaybe arg
     in case n >>= parseVerbosity
         of Just v -> 
             do liftIO $ putStrLn $ "Verbosity set to " ++ (show $ fromJust n)
                put conf{options=opts{verbosity=v}}
            _ -> liftIO $ debugCommandError $ "Invalid argument to v: " ++ arg

parseVerbosity :: Int -> Maybe Verbosity
parseVerbosity 0 = Just NoOutput
parseVerbosity 1 = Just SomeOutput
parseVerbosity 2 = Just VerboseOutput
parseVerbosity _ = Nothing

setSkipStep :: String -> StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
setSkipStep arg = 
 do conf@DebuggerConfiguration{options=opts} <- get
    case R.readMaybe arg
     of Just n ->
         if n >= 0
         then do liftIO $ putStrLn $ "Printing debug info every 2^" ++ (show n) ++ "th step"
                 put conf{options=opts{skipStep=n}}
         else liftIO $ debugCommandError $ "Invalid argument to s: " ++ arg
        Nothing -> liftIO $ debugCommandError $ "Invalid argument to s: " ++ arg

setDelay :: String -> StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
setDelay arg =
 do conf@DebuggerConfiguration{options=opts} <- get
    case R.readMaybe arg
     of Just n ->
         if n >= -1 && n <= 20
         then do if n == -1
                 then liftIO $ putStrLn "Interactivity disabled"
                 else liftIO $ putStrLn $ "Pausing for 2^" ++ (show n) ++ " microseconds every machine step"
                 put conf{options=opts{delay=n}}
         else liftIO $ debugCommandError $ "Invalid argument to d: " ++ arg
        Nothing -> liftIO $ debugCommandError $ "Invalid argument to d: " ++ arg

togglePause :: StateT DebuggerConfiguration (StateT MachineConfiguration IO) ()
togglePause =
 do conf@DebuggerConfiguration{paused=isPaused} <- get
    if isPaused
    then do liftIO $ putStrLn "U N P A U S E D"
            put conf{paused=False}
    else do liftIO $ putStrLn "P A U S E D"
            put conf{paused=True}

debugCommandError :: String -> IO()
debugCommandError err = putStrLn $ err ++ "\n\n" ++ debuggerHelpText
