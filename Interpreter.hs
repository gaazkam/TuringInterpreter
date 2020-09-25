{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Interpreter (
 MachineConfiguration(..), MachineState, TransitionTarget, TapeTransition, Direction(..), Tape(..), StateSymbol, Letter,
 isFinished, stepMachine, showDetailedConfiguration, showSimpleConfiguration, showLastTapeContent)
where

import Data.List
import qualified Data.HashMap.Lazy as M
import Control.DeepSeq
import GHC.Generics

-- Assumption:
-- let S be the set of all allowed state symbols except the accepting state, A be the alphabet of the machine (the set of all possible letters on tapes) and n the number of tapes.
-- Then the set of keys of transitionTable is S x A^n
-- Which means that transitionTable is the full table of all possible transitions from all possible states (except accepting states).
data MachineConfiguration = MachineConfiguration {
 tapes :: [Tape],
 current :: StateSymbol,
 accepting :: StateSymbol,
 transitionTable :: M.HashMap MachineState TransitionTarget }
 deriving (Generic, NFData)

data Tape = Tape { beforeHead :: [Letter], onHead :: Letter, afterHead :: [Letter], blank :: Letter} deriving (Generic, NFData)

type MachineState = (StateSymbol, [Letter])

type TransitionTarget = (StateSymbol, [TapeTransition])
type TapeTransition = (Letter, Direction)
data Direction = TapeLeft | TapeRight | TapeStill deriving (Generic, NFData)

type Letter = String
type StateSymbol = String

isFinished :: MachineConfiguration -> Bool
isFinished MachineConfiguration{current=cs, accepting=as} = cs == as

-- Assumption: Machine NOT in accepting state! (current /= accepting)
stepMachine :: MachineConfiguration -> MachineConfiguration
stepMachine machine@MachineConfiguration{transitionTable=tt, tapes=ts} =
 machine{tapes = modifyTapes tapeTransitions ts, current=newState}
 where (newState, tapeTransitions) = tt M.! (getMachineState machine)

showSimpleConfiguration :: MachineConfiguration -> String
showSimpleConfiguration conf =
 "Machine state: " ++ ss ++ "\nTapes at head: " ++ intercalate " " ts ++ "\n"
 where (ss, ts) = getMachineState conf

showDetailedConfiguration :: MachineConfiguration -> String
showDetailedConfiguration MachineConfiguration{tapes=ts, current=c} = "Machine state: " ++ c ++ "\n\n" ++ showDetailedTapesContents ts

showDetailedTapesContents :: [Tape] -> String
showDetailedTapesContents tapes = concat [showDetailedTapeContent i t | (i, t) <- zip [1..] tapes]

showDetailedTapeContent :: Int -> Tape -> String
showDetailedTapeContent tapeNo Tape{beforeHead=bh, onHead=h, afterHead=ah} =
 heading ++ contentLine ++ headMarkingLine ++ "\n"
 where heading = "Tape " ++ show tapeNo ++ ":\n"
       indendation = "     "
       before = concat $ intersperse " " (reverse bh)
       after = concat $ intersperse " " ah
       beforeWhitespace = if not $ null before then " " else ""
       afterWhitespace = if not $ null after then " " else ""
       contentLine = indendation ++ before ++ beforeWhitespace ++ h ++ afterWhitespace ++ after ++ "\n"
       headPosition = (length indendation) + (length before) + (length beforeWhitespace)
       headMarkingLine = concat (replicate (headPosition-4) " ") ++ "HEAD^\n"

showLastTapeContent :: MachineConfiguration -> String
showLastTapeContent MachineConfiguration{tapes=ts} = showTapeContent $ last ts

showTapeContent :: Tape -> String
showTapeContent t = concat $ intersperse " " (tapeContent t)

tapeContent :: Tape -> [Letter]
tapeContent Tape{beforeHead=bh, onHead=h, afterHead=ah, blank=b} =
 case (bh, h, ah)
  of ([], _, []) -> [h]
     ([], b, (_:_)) -> ah
     ((_:_), b, []) -> (reverse bh)
     ((_:_), _, (_:_)) -> (reverse bh)++(h:ah)

getMachineState :: MachineConfiguration -> MachineState
getMachineState MachineConfiguration{current=c, tapes=ts} = (c, readTapes ts)

readTapes :: [Tape] -> [Letter]
readTapes ts = map onHead ts

modifyTapes :: [TapeTransition] -> [Tape] -> [Tape]
modifyTapes transitions tapes = [modifyTape transition tape | (transition, tape) <- zip transitions tapes]

modifyTape :: TapeTransition -> Tape -> Tape
modifyTape (l, d) = (moveTape d) . (writeToTape l)

writeToTape :: Letter -> Tape -> Tape
writeToTape l t = t{onHead=l}

moveTape :: Direction -> Tape -> Tape
moveTape TapeRight t@Tape{beforeHead=bh, onHead=h, afterHead=ah, blank=b} =
 t{beforeHead=newBeforeHead, onHead=newHead, afterHead=newAfterHead}
 where (newHead, newAfterHead) =
        case ah
        of (nh:nah) -> (nh, nah)
           _ -> (b, [])
       newBeforeHead = 
        if h == b && bh == []
        then []
        else h:bh
moveTape TapeLeft t@Tape{beforeHead=bh, onHead=h, afterHead=ah, blank=b} =
 t{beforeHead=newBeforeHead, onHead=newHead, afterHead=newAfterHead}
 where (newBeforeHead, newHead) =
        case bh
        of (nh:nbh) -> (nbh, nh)
           _ -> ([], b)
       newAfterHead =
        if h == b && ah == []
        then []
        else h:ah
moveTape TapeStill t = t
