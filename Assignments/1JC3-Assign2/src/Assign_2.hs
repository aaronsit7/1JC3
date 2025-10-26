{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2025

  Modified by W. M. Farmer 9 October 2025
-}
module Assign_2 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Aaron Sit
-- Date: October 17, 2025
macid :: String
macid = "sita1"

type GaussianRat = (Rational,Rational)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussReal :: GaussianRat -> Rational
gaussReal (x, _) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussImag :: GaussianRat -> Rational
gaussImag (_, y) = y

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussConj :: GaussianRat -> GaussianRat
gaussConj (x, y) = (x, -y)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussAdd :: GaussianRat -> GaussianRat -> GaussianRat
gaussAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussMul :: GaussianRat -> GaussianRat -> GaussianRat
gaussMul (x1, y1) (x2, y2) = (x1 * x2 - y1 * y2, x1 * y2 + y1 * x2)


{- -----------------------------------------------------------------
 - gaussRecip
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussRecip :: GaussianRat -> GaussianRat
gaussRecip (x, y) = (x / d, -y / d)
  where d = x^2 + y^2

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussNorm :: GaussianRat -> Rational
gaussNorm (x, y) = x^2 + y^2
{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussAddList :: [GaussianRat] -> GaussianRat
gaussAddList [] = (0, 0)
gaussAddList ((x, y):xs) = (x + x', y + y')
  where (x', y') = gaussAddList xs

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussMulList :: [GaussianRat] -> GaussianRat
gaussMulList [] = (1, 0)
gaussMulList ((x, y):xs) = gaussMul (x, y) (gaussMulList xs)

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
gaussCircle :: [GaussianRat] -> Rational -> [GaussianRat]
gaussCircle xs r = filter (\(x, y) -> gaussNorm (x, y) == r) xs
