module ToyPL.Eval where

import ToyPL.Abs
import ToyPL.Trans

import Data.List (find)

-- | Evaluetes the value of the expression if possible.
evalExp :: ProgramState -> Exp -> Maybe Integer
evalExp state x = case x of
  EAdd  exp1 exp2 -> apply ( + ) (evalExp state exp1) (evalExp state exp2)
  ESub  exp1 exp2 -> apply ( - ) (evalExp state exp1) (evalExp state exp2)
  EMul  exp1 exp2 -> apply ( * ) (evalExp state exp1) (evalExp state exp2)
  EDiv  exp1 exp2 -> apply (div) (evalExp state exp1) (evalExp state exp2)
  EFact exp       -> factorial (evalExp state exp)
    where 
        factorial :: (Ord a, Num a) => Maybe a -> Maybe a
        factorial Nothing = Nothing
        factorial (Just a)
          | a <= 0    = Just 1
          | otherwise = apply (*) (factorial $ Just (a - 1)) (Just a)

  EInt integer    -> Just integer
  EVar variable   -> getValue
    where 
      getValue = fmap snd $ find (\c -> (fst c) == variable) (variables state)

-- | Evaluates the condition if possible.
evalCondition :: ProgramState -> Condition -> Maybe Bool
evalCondition state x = case x of
  COr  condition1 condition2 -> apply (||) (evalCondition state condition1)
                                           (evalCondition state condition2)
  CAnd condition1 condition2 -> apply (&&) (evalCondition state condition1)
                                           (evalCondition state condition2)
  CNot condition             -> fmap not (evalCondition state condition)
  CEquals        exp1 exp2   -> apply (==) (evalExp state exp1) (evalExp state exp2)
  CLessThan      exp1 exp2   -> apply (< ) (evalExp state exp1) (evalExp state exp2)
  CBiggerThan    exp1 exp2   -> apply (> ) (evalExp state exp1) (evalExp state exp2)
  CLessOrEqual   exp1 exp2   -> apply (<=) (evalExp state exp1) (evalExp state exp2)
  CBiggerOrEqual exp1 exp2   -> apply (>=) (evalExp state exp1) (evalExp state exp2)

-- | Executes command.
evalCommand :: ProgramState -> Command -> ProgramState
evalCommand state x = case x of
  VMProg command1 command2 -> evalCommand state' command2
    where
        state' = evalCommand state command1

  VMAssignment label1 variable exp label2 -> state'
    where 
      setValue :: Bool -> Integer -> [(Variable, Integer)]
      setValue True  val = updateVariable variable vars val
      setValue False val = vars ++ [(variable, val)]

      updateVariable _ [] _ = []
      updateVariable var ((v, int) : vs) value = (maybeUpdate (var == v) v int value) : (updateVariable var vs value)

      maybeUpdate True  v _ value = (v, value)
      maybeUpdate False v int _   = (v, int)

      vars = variables state

      result    = evalExp state exp
      vars'     = setValue (elem variable $ map fst vars) value'
      position' = (transLabel label2)

      value' = case result of
        Nothing -> 0
        Just a  -> a

      state' = (checkState result position' state) { variables = vars' }

  VMBranching label1 condition label2 label3 -> state'
    where
      result    = evalCondition state condition 
      position' = getPosition result

      getPosition (Just True) = transLabel label2
      getPosition _           = transLabel label3

      state' = (checkState result position' state)


checkState :: Eq a => Maybe a -> Integer -> ProgramState -> ProgramState
checkState result position state
  | result   == Nothing           = state { execPosition = position, execCode = -1, exitMessage = "Error: use of a non-existent variable." }
  | position == finishLabel state = state { execPosition = position, execCode = 1,  exitMessage = "Program has finished successfully." }
  | otherwise                     = state { execPosition = position }

apply :: (a -> a -> b) -> Maybe a -> Maybe a -> Maybe b
apply _ Nothing _ = Nothing
apply _ _ Nothing = Nothing
apply f (Just a1) (Just a2) = Just (f a1 a2)