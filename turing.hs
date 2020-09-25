import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import System.IO
import Control.Exception
import Control.DeepSeq

import CommandParser
import MachineParser
import Interpreter
import Debugger

main :: IO ()
main =
 do hSetBuffering stdout LineBuffering
    results <- runExceptT mainM
    case results
     of Right output -> putStrLn output
        Left err -> hPutStrLn stderr err

mainM :: ExceptT String IO String
mainM =
 do CommandLineArgs{opts=os, inputFile=inpf} <- ExceptT parseArgs
    machineConf <- ExceptT $ withFile inpf ReadMode (\s -> readMachine s >>= evaluate . force)
    liftIO (evalStateT (evalStateT debugMachine (defaultDebuggerConfiguration os)) machineConf)

