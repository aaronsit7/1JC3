{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) William M. Farmer 2025
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2025
-}
module Assign_3 where

--------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
--------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLARATIONS (I.E THE LINE WITH THE :: ABOUT
--    THE FUNCTION DECLARATION). IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE
--    IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN `stack build` AND MAKE SURE THERE
--    ARE NO ERRORS) BEFORE SUBMITTING. FAILURE TO DO SO WILL RESULT IN A MARK
--    OF 0!
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS
--    "jim123" THEN `macid = "jim123"`). REMEMBER THAT YOUR MACID IS THE FIRST
--    PART OF YOUR SCHOOL EMAIL (I.E. IF YOUR EMAIL IS "jim123@mcmaster.ca",
--    THEN YOUR MACID IS "jim123"). FAILURE TO DO SO WILL RESULT IN A MARK OF 0!
--------------------------------------------------------------------------------

-- Name: Aaron Sit
-- Date: Nov 10, 2025
macid :: String
macid = "sita1"

{- -----------------------------------------------------------------
 - LambdaTerm algebraic type
 - -----------------------------------------------------------------
 -}

data LambdaTerm =
    Var Integer
  | FunApp LambdaTerm LambdaTerm
  | FunAbs Integer LambdaTerm
  deriving (Eq,Show)

{- -----------------------------------------------------------------
 - isFreeIn
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
isFreeIn :: LambdaTerm -> LambdaTerm -> Bool
isFreeIn (Var x) m = case m of
  Var y -> x == y
  FunApp m1 m2 -> isFreeIn (Var x) m1 || isFreeIn (Var x) m2
  FunAbs y m' -> x /= y && isFreeIn (Var x) m'
isFreeIn _ _ = False

{- -----------------------------------------------------------------
 - freshVarList
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
freshVarList :: [LambdaTerm] -> Integer
freshVarList ms = 1 + maximum (0 : concatMap allVars ms)
  where
    allVars :: LambdaTerm -> [Integer]
    allVars (Var i) = [i]
    allVars (FunApp m n) = allVars m ++ allVars n
    allVars (FunAbs i m) = i : allVars m

{- -----------------------------------------------------------------
 - sub
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
sub :: LambdaTerm -> LambdaTerm -> LambdaTerm -> LambdaTerm
sub v@(Var x) m n = case n of
  Var y -> if x == y then m else Var y
  FunApp n1 n2 -> FunApp (sub v m n1) (sub v m n2)
  FunAbs y n' ->
    if x == y then
      FunAbs y n'  -- x is bound, no substitution inside
    else if not (isFreeIn (Var y) m) then
      FunAbs y (sub v m n')  -- y not free in m, safe to substitute
    else
      -- Need alpha-conversion to avoid capture
      let fresh = freshVarList [m, n]
          n'' = sub (Var y) (Var fresh) n'
      in FunAbs fresh (sub v m n'')
sub _ _ n = n

{- -----------------------------------------------------------------
 - isRedex
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
isRedex :: LambdaTerm -> Bool
isRedex (FunApp (FunAbs _ _) _) = True
isRedex _ = False

{- -----------------------------------------------------------------
 - betaRed
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
betaRed :: LambdaTerm -> LambdaTerm
betaRed (FunApp (FunAbs x n) m) = sub (Var x) m n
betaRed m = m

{- -----------------------------------------------------------------
 - normOrdRed
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
normOrdRed :: LambdaTerm -> LambdaTerm
normOrdRed (Var i) = Var i
normOrdRed (FunApp m n) =
  let
    headBetaRed o =
      case o of
        (Var i) -> Var i
        (FunApp p q) ->
          case headBetaRed p of
            (FunAbs i p) -> headBetaRed (betaRed (FunApp (FunAbs i p) q))
            p'          -> FunApp p' q
        (FunAbs i p) -> FunAbs i p
    in case headBetaRed m of
      FunAbs i p -> normOrdRed (betaRed (FunApp (FunAbs i p) n))
      m'         -> FunApp (normOrdRed m') (normOrdRed n)
normOrdRed (FunAbs i m) = FunAbs i (normOrdRed m)

{- -----------------------------------------------------------------
 - prettyPrint
 - -----------------------------------------------------------------
 - Description:
 -    TODO add comments
 -}
prettyPrint :: LambdaTerm -> String
prettyPrint (Var i) = "x" ++ show i
prettyPrint (FunAbs i m) = "(λx" ++ show i ++ "." ++ prettyPrint m ++ ")"
prettyPrint (FunApp m n) = "(" ++ prettyPrint m ++ " " ++ prettyPrintArg n ++ ")"
  where
    prettyPrintArg (Var i) = "x" ++ show i
    prettyPrintArg t = prettyPrint t
