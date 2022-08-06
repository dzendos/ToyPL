module ToyPL.Trans where 

import ToyPL.Abs

-- | transs given program into sequens of commands.
transProgram :: Program -> (Integer, [Command])
transProgram x = transProgramToVM x 0

-- | Recursive translation of program into sequence of commands with labels.
transProgramToVM :: Program -> Integer -> (Integer, [Command])
transProgramToVM x label =
  case x of
    PSeq program1 program2 -> (finishLabel2, commands1 ++ commands2)
      where
        (finishLabel1, commands1) = transProgramToVM program1 label
        (finishLabel2, commands2) = transProgramToVM program2 finishLabel1

    PAssign variable exp -> ((label + 1), assignCmd)
      where
        assignCmd = [VMAssignment (VMLabel label) variable exp (VMLabel (label + 1))]

    PIf condition program1 program2 -> (finishLabel, branchingCmd)
      where 
        (elseLabel,   commands1) = transProgramToVM program1 (label + 1)
        (finishLabel, commands2) = transProgramToVM program2 (elseLabel)
        branchingCmd = (VMBranching (VMLabel label) condition (VMLabel (label + 1)) (VMLabel elseLabel)) : (commands1 ++ commands2)
        
    PWhile condition program -> (finishLabel, whileCmd)
      where
        (finishLabel, commands) = transProgramToVM program (label + 1)
        whileCmd                = (VMBranching (VMLabel label) condition (VMLabel (label + 1)) (VMLabel finishLabel)) : changeLastCommand
        changeLastCommand = case (last commands) of
          VMAssignment label1 variable exp label2 -> whileCommands
            where 
              whileCommands = (take ((length commands) - 1) commands) ++ [VMAssignment label1 variable exp (VMLabel label)]

-- | transs variable by given identifier into string.
transVariable :: Variable -> String
transVariable x = case x of
  Variable ident -> transIdent ident

-- | transs identifier into string.
transIdent :: Ident -> String
transIdent x = case x of
  Ident string -> string

-- | transs a label into Integer.
transLabel :: Label -> Integer
transLabel x = case x of
  VMLabel integer -> integer