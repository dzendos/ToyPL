module ToyPL where

import           ToyPL.Abs
import           ToyPL.Par

import           ToyPL.Eval
import           ToyPL.Trans

import           ToyPL.ErrM

import           Data.Maybe

getCommand :: ProgramState -> [Command] -> Maybe Command
getCommand state commands = listToMaybe $ drop n commands
    where
        n = fromIntegral $ execPosition state

-- | Processes given command.
processCommands :: String -> ProgramState -> [Command] -> ProgramState
processCommands userCommand state commands = do
    case getCommand state commands of
        Nothing  -> state
        Just cmd ->
            case userCommand of
                "exit" -> state { execCode = 2, exitMessage = "Execution has terminated by the user." }
                "step" -> state'
                "run"  -> processCommands userCommand state' commands
                _      -> state

            where
                state'   = evalCommand  state cmd
                -- position = execPosition state

-- | Gets command from the User and processes it.
programRunner :: ProgramState -> [Command] -> IO ()
programRunner state commands
    | execCode state /= 0 = putStrLn $ exitMessage state
    | otherwise           = do
        putStrLn $ showCmd $ getCommand state commands
        userCommand <- getLine
        let state' = processCommands userCommand state commands
        putStrLn (show state')
        programRunner state' commands

        where
            -- position = execPosition state
            showCmd Nothing  = "Nothing"
            showCmd (Just a) = show a

translateInput :: String -> Either String [String]
translateInput input =
  case transProgram <$> pProgram (myLexer input) of
    Left err            -> Left err
    Right (_, commands) -> Right (commandsToStrings commands)

runWithInput :: String ->  IO ()
runWithInput input = do
    let commands = snd (tplProgram input)

    putStrLn $ concat $ commandsToStrings commands

    programRunner (initState { finishLabel = toInteger $ length commands }) commands

        where
            initState = ProgramState {
                finishLabel  = 0,
                execPosition = 0,
                execCode     = 0,
                exitMessage  = "",
                variables    = []
            }


-- | Runs the compiler.
run :: IO ()
run = do
    putStrLn "Enter path to source file: "
    sourcePath  <- getLine
    fileContent <- readFile sourcePath
    runWithInput fileContent

-- | Tries to translate given program s.
tplProgram :: String -> (Integer, [Command])
tplProgram s =
    let Ok e = pProgram (myLexer s)
    in transProgram e

-- | Returns a list of commands that can be printed on the screen.
commandsToStrings :: [Command] -> [String]
commandsToStrings commands = map (\c -> show c ++ "\n") commands
