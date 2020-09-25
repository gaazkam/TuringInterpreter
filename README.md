# TuringInterpreter
Turing machine interpreter & debugger

I think I made a design mistake: I require that the Turing machine source code provided by the user fully specifies all transitions from all possible letter x state combinations. This makes Turing machine source code files unreasonably large, since most such configurations are impossible anyway. I think I should have instead added an implicit erroneous state, reached whenever during interpretation the Turing machine reaches a configuration that is not present in the transition table. This, however, will be done in the second version of this interpreter (if I ever make it).

Supports machines with multiple tapes. The first tape is the input tape, the last tape is the output tape.

The debugger may print out Turing machine configurations in two verbosity settings, either on demand or automatically, every 2^n th step. Interpretation can be paused on demand or it can be slowed down, so that the user may see every step the Turing machine makes.

Example Turing machine source code, is included here. This Turing machine simply reverses the palindrome sentence 'WAS IT A CAR OR A CAT I SAW'.

Example interpreter invocation: `./turing -v 2 -d 20 REVERSE.TURING`

General usage: turing options inputFile

Option descriptions:

*  -v VERBOSITY  --verbosity=VERBOSITY  Set initial verbosity of debug messages. Possible values: 0 - no debug messages 1 - only machine state and tape contents the machine is currently looking at 2 - machine state as well as whole tape contents. Default: 0
*  -s STEP       --step=STEP            Only print debug messages every 2^STEP machine steps. Default: 0
*  -d DELAY      --delay=DELAY          Pause execution for 2^DELAY microseconds every machine step. -1 disables pausing and disables interactivity. Maximum: 20. Default: -1
*  -h            --help                 Display this message and exit.

Turing machine source code file format: 
  * The first line contains the input to the Turing machine, that is, the initial contents of the first tape. Letters are separated by whitespace.
  * The second line contains the list of allowed states of the machine, again separated by whitespace. The final state is the accepting state.
  * All subsequent lines denote the transition table of the machine. Each line contains, in that order:
    - Letters seen by the head of the machine on each tape
    - A transition for each state except the accepting state. The transition contains:
      - The new state of the machine;
      - For each tape: the letter to be written to the tape on the current position and either of <, > or ^, which is the direction the tape should move to (^ means the tape does not move)
      
 The transition table must be exhaustive: a transition should be specified for every possible state x letter^n combination, where n is the number of tapes.
 The first letter of the first line of the transition table specifies the blank character. The tapes are infinite from both sides and, unless overwritten, will be populated by copies of the blank character.
 
 Both letters and state symbols may consist of multiple non-whitespace characters ('letter' here is used in the sense of the element of the machine's alphabet).
 
 Empty lines are NOT allowed, except for the first line, which (if empty) means that the Turing machine receives no input.
 
 After the execution has completed, the contents of the last tape, from position 0 to the right, are interpreted as the output.

Unless interactivity is disabled, during interpretation you can enter commands.

Debugger commands:
 * b - pause/unpause debugging.
 * p VERBOSITY - print machine state. Verbosity of 0 is not allowed. Default: 1
 * v VERBOSITY - set verbosity for automatic printing of debug messages. If ommitted, cycles among all three possible choices.
 * s STEP - only print automatic debug messages every (2^STEP)th machine steps. Default: 0
 * d DELAY - wait 2^DELAY microseconds after each machine step. Maximum: 20. -1 disables interactivity. Default: 20
 * h - display this help message
