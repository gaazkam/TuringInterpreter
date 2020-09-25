module MachineParser (readMachine, sourceCodeHelpText)
where

import Data.List
import qualified Data.HashSet as H
import qualified Data.HashMap.Lazy as M
import Control.Monad
import System.IO

import CommandParser
import Interpreter
import Utils

data TransitionTable = TransitionTable { header :: States, tableBody :: ParseTransitionsRet }
data States = States { entryState :: StateSymbol, workingStates :: [StateSymbol], acceptingState :: StateSymbol }
data ParseTransitionsRet = ParseTransitionsRet {
 alphabet :: H.HashSet Letter, blankLetter :: Letter,
 noOfTapes :: Int, transitions :: M.HashMap MachineState TransitionTarget }

readMachine :: Handle -> IO (Either String MachineConfiguration)
readMachine h = addInstructionsToError <$> parseMachine <$> (hGetContents h)

addInstructionsToError :: Either String MachineConfiguration -> Either String MachineConfiguration
addInstructionsToError = transformLeft (++("\n\n"++sourceCodeHelpText))

parseMachine :: String -> Either String MachineConfiguration
parseMachine input =
 case lines input
 of (tapeInput:transitionTable) ->
     do TransitionTable {
         tableBody=ParseTransitionsRet{alphabet=alph, blankLetter=bl, noOfTapes=nT, transitions=ts},
         header=States{entryState=es, acceptingState=as}
        } <- parseTransitionTable transitionTable
        validateInput alph parsedTapeInput
        return $ initMachine bl parsedTapeInput nT es as ts
     where parsedTapeInput = parseInput tapeInput
    _ -> Left "No input."

parseInput :: String -> [Letter]
parseInput string = words string

validateInput :: H.HashSet Letter -> [Letter] -> Either String ()
validateInput alphabet input =
 if all ((flip H.member) alphabet) input
 then Right()
 else Left "Invalid letter found in input"
 
parseTransitionTable :: [String] -> Either String TransitionTable
parseTransitionTable (headerLine:bodyLines) =
 do states@States{entryState=es,workingStates=ws,acceptingState=as} <- parseStates headerLine
    transitions <- parseTransitions (es:ws) (H.fromList $ es:as:ws) bodyLines
    return TransitionTable {header=states, tableBody=transitions}
parseTransitionTable _ = Left "Expected transition table."

parseStates :: String -> Either String States
parseStates string =
 case symbols
 of _:_:_ -> Right $ States {entryState = head symbols, workingStates = init $ tail symbols, acceptingState = last symbols}
    _ -> Left "Expected at least two states: entry state and accepting state."
 where symbols = nub $ words string

parseTransitions :: [StateSymbol] -> H.HashSet StateSymbol -> [String] -> Either String ParseTransitionsRet
parseTransitions sourceStates allowedTargetStates string@(_:_) =
 do noOfTapes <- getNoOfTapes noOfSourceStates firstLine
    transitionLines <- mapM (parseTransitionInputLine noOfTapes noOfSourceStates allowedTargetStates) input
    (blankLetter, alphabet) <- inferAlphabet noOfTapes (map fst transitionLines)
    validateTransitionLetters alphabet ((map fst) . concat . (map snd) . concat . (map snd) $ transitionLines) -- Gimme a break!!
    return ParseTransitionsRet { alphabet = alphabet, blankLetter = blankLetter, noOfTapes = noOfTapes, transitions = makeTransitions sourceStates transitionLines }
 where noOfSourceStates = length sourceStates
       input = map words string
       firstLine = head input
parseTransitions _ _ _ = Left "Expected at least one transition."

validateTransitionLetters :: H.HashSet Letter -> [Letter] -> Either String ()
validateTransitionLetters alphabet transitionLetters =
 if all ((flip H.member) alphabet) transitionLetters
 then Right ()
 else Left "Invalid letter in a transition target"

-- d + s*(2d+1) = y
-- therefore d = (y-s)/(1+2s)
getNoOfTapes :: Int -> [String] -> Either String Int
getNoOfTapes noOfSourceStates tokens =
 do when (m /= 0) $ Left "Incorrect number of tokens in the transition table."
    when (d == 0) $ Left "Expected at least one tape."
    return d
    where y = length tokens
          s = noOfSourceStates
          (d,m) = (y-s) `divMod` (1+2*s)
   
parseTransitionInputLine :: Int -> Int -> H.HashSet StateSymbol -> [String] -> Either String ([Letter], [TransitionTarget])
parseTransitionInputLine noOfTapes noOfSourceStates allowedTargetStates tokens =
 do (letters, unsplitTargets) <- maybeToEither tooFewLettersError (splitAtMaybe noOfTapes tokens)
    targetsPerState <- maybeToEither tooFewTransitionTokensError (listToChunksMaybe transitionLength unsplitTargets)
    parsedTransitions <- mapM (parseTransitionTarget allowedTargetStates) targetsPerState
    return (letters, parsedTransitions)
 where tooFewLettersError = ("Too short transition line: expected at least " ++ show noOfTapes ++ " letters.")
       tooFewTransitionTokensError = "Expected transitions from " ++ show noOfSourceStates ++ " states, each containing " ++ show transitionLength ++ " tokens."
       transitionLength = 2*noOfTapes + 1

parseTransitionTarget :: H.HashSet StateSymbol -> [String] -> Either String TransitionTarget
parseTransitionTarget allowedTargetStates (targetState:transitions) =
 do when (not $ H.member targetState allowedTargetStates) (Left $ "Invalid target state " ++ targetState)
    parsedTapeTransitions <- mapM parseTapeTransition (listToChunks 2 transitions)
    return (targetState, parsedTapeTransitions)

parseTapeTransition :: [String] -> Either String TapeTransition
parseTapeTransition (letter:direction:[]) =
 do parsedDirection <- parseDirection direction
    return (letter, parsedDirection)

parseDirection :: String -> Either String Direction
parseDirection ">" = Right TapeRight
parseDirection "<" = Right TapeLeft
parseDirection "^" = Right TapeStill
parseDirection inv = Left $ "Invalid tape direction " ++ inv

inferAlphabet :: Int -> [[Letter]] -> Either String (Letter, H.HashSet Letter)
inferAlphabet noOfTapes tapesConfigurations = do
 when (length tapesConfigurations /= (H.size $ H.fromList tapesConfigurations)) (Left "Duplicate tape configuration.")
 when (length tapesConfigurations /= (H.size alphabet ^ noOfTapes)) (Left "Not all possible tape configurations specified.")
 return (head $ head tapesConfigurations, alphabet)
 where alphabet = H.fromList $ concat tapesConfigurations

makeTransitions :: [StateSymbol] -> [([Letter], [TransitionTarget])] -> M.HashMap MachineState TransitionTarget
makeTransitions sourceStates transitionLines =
 M.unions $ map (makeTransitionLine sourceStates) transitionLines

makeTransitionLine :: [StateSymbol] -> ([Letter], [TransitionTarget]) -> M.HashMap MachineState TransitionTarget
makeTransitionLine sourceStates (tapeConf, transitionTargets) =
 M.fromList [((sourceState, tapeConf), target) | (sourceState, target) <- zip sourceStates transitionTargets]
 
initMachine :: Letter -> [Letter] -> Int -> StateSymbol -> StateSymbol -> M.HashMap MachineState TransitionTarget -> MachineConfiguration
initMachine blankLetter input noOfTapes entryState acceptingState transitionTable =
 MachineConfiguration {
  tapes = initializeWithContent blankLetter input : replicate (noOfTapes-1) (initializeBlank blankLetter),
  current = entryState,
  accepting = acceptingState,
  transitionTable = transitionTable }

initializeBlank :: Letter -> Tape
initializeBlank blankLetter = Tape {beforeHead = [], onHead = blankLetter, afterHead = [], blank = blankLetter}

initializeWithContent :: Letter -> [Letter] -> Tape
initializeWithContent blankLetter (firstLetter:restOfContent) = Tape {beforeHead = [], onHead = firstLetter, afterHead = restOfContent, blank = blankLetter}
initializeWithContent blankLetter _ = initializeBlank blankLetter
