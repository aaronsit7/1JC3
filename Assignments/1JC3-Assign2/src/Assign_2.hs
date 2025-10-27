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
 -   Returns the real part of a Gaussian rational number.
 -}
gaussReal :: GaussianRat -> Rational
gaussReal (x, _) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   Returns the imaginary part of a Gaussian rational number.
 -}
gaussImag :: GaussianRat -> Rational
gaussImag (_, y) = y

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   Returns the conjugate of a Gaussian rational number.
 -}
gaussConj :: GaussianRat -> GaussianRat
gaussConj (x, y) = (x, -y)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   Returns the sum of two Gaussian rational numbers.
 -}
gaussAdd :: GaussianRat -> GaussianRat -> GaussianRat
gaussAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   Returns the product of two Gaussian rational numbers.
 -}
gaussMul :: GaussianRat -> GaussianRat -> GaussianRat
gaussMul (x1, y1) (x2, y2) = (x1 * x2 - y1 * y2, x1 * y2 + y1 * x2)


{- -----------------------------------------------------------------
 - gaussRecip
 - -----------------------------------------------------------------
 - Description:
 -   Returns the reciprocal of a Gaussian rational number.
 -}
gaussRecip :: GaussianRat -> GaussianRat
gaussRecip (0, 0) = error "gaussRecip: reciprocal of zero is undefined"
gaussRecip (x, y) = (x / d, (-y) / d)
  where d = x^2 + y^2

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   Returns the norm (squared magnitude) of a Gaussian rational number.
 -}
gaussNorm :: GaussianRat -> Rational
gaussNorm (x, y) = x^2 + y^2
{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 -   Returns the sum of a list of Gaussian rational numbers.
 -}
gaussAddList :: [GaussianRat] -> GaussianRat
gaussAddList [] = (0, 0)
gaussAddList ((x, y):xs) = (x + x', y + y')
  where (x', y') = gaussAddList xs

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   Returns the product of a list of Gaussian rational numbers.
 -}
gaussMulList :: [GaussianRat] -> GaussianRat
gaussMulList [] = (1, 0)
gaussMulList ((x, y):xs) = gaussMul (x, y) (gaussMulList xs)

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   Returns a list of all Gaussian rational numbers in a given list that lie on a circle of a given radius.
 -}
gaussCircle :: [GaussianRat] -> Rational -> [GaussianRat]
gaussCircle xs r = filter (\(x, y) -> gaussNorm (x, y) == r) xs
{- -------------------------------------------------------------------------
 - Test Cases
 - Function: gaussConj
 - Test Case #: 1
 - Input: (3, 4)
 - Expected Output: (3, -4)
 - Actual Output: (3 % 1, (-4) % 1)
 - Function: gaussConj
 - Test Case #: 2
 - Input: (0, 0)
 - Expected Output: (0, 0)
 - Actual Output: (0 % 1, 0 % 1)
 - Function: gaussConj
 - Test Case #: 3
 - Input: (-2, 0)
 - Expected Output: (-2, 0)
 - Actual Output: ((-2) % 1, 0 % 1)
 -
 - Function: gaussAdd
 - Test Case #: 1
 - Input: (0, 0)(0, 0)
 - Expected Output: (0, 0)
 - Actual Output: (0 % 1, 0 % 1)
 -
 - Function: gaussAdd
 - Test Case #: 2
 - Input: (0, 0)(5, -3)
 - Expected Output: (5, -3)
 - Actual Output: (5 % 1, (-3) % 1)
 -
 - Function: gaussAdd
 - Test Case #: 3
 - Input: (-1, -1)(1, 1)
 - Expected Output: (0, 0)
 - Actual Output: (0 % 1, 0 % 1)
 -
 - Function: gaussMul
 - Test Case #: 1
 - Input: (1, 2)(3, 4)
 - Expected Output: (-5, 10)
 - Actual Output: ((-5) % 1, 10 % 1)
 -
 - Function: gaussMul
 - Test Case #: 2
 - Input: (0, 0)(5, 6)
 - Expected Output: (0, 0)
 - Actual Output: (0 % 1, 0 % 1)
 -
 - Function: gaussMul
 - Test Case #: 3
 - Input: (2, 3)(4, -5)
 - Expected Output: (23, 2)
 - Actual Output: (23 % 1, 2 % 1)
 -
 - Function: gaussRecip
 - Test Case #: 1
 - Input: (1, 2)
 - Expected Output: (1/5, -2/5)
 - Actual Output: (1 % 5, (-2) % 5)
 -
 - Function: gaussRecip
 - Test Case #: 2
 - Input: (0, 0)
 - Expected Output: gaussRecip: reciprocal of zero is undefined
 - Actual Output: gaussRecip: reciprocal of zero is undefined
 -
 - Function: gaussRecip
 - Test Case #: 3
 - Input: (0, 1)
 - Expected Output: (0, -1)
 - Actual Output: (0 % 1, (-1) % 1)
 -
 - Function: gaussNorm
 - Test Case #: 1
 - Input: (3, 4)
 - Expected Output: 25
 - Actual Output: 25 % 1
 -
 - Function: gaussNorm
 - Test Case #: 2
 - Input: (1, 1)
 - Expected Output: 2
 - Actual Output: 2 % 1
 -
 - Function: gaussNorm
 - Test Case #: 3
 - Input: (0, 0)
 - Expected Output: 0
 - Actual Output: 0 % 1
 -
 - Function: gaussAddList
 - Test Case #: 1
 - Input: [(1, 2), (3, 4), (5, 6)]
 - Expected Output: (9, 12)
 - Actual Output: (9 % 1, 12 % 1)
 -
 - Function: gaussAddList
 - Test Case #: 2
 - Input: []
 - Expected Output: (0, 0)
 - Actual Output: (0 % 1, 0 % 1)
 -
 - Function: gaussAddList
 - Test Case #: 3
 - Input: [(-1, -1), (1, 2)]
 - Expected Output: (2, 1)
 - Actual Output: (0 % 1, 1 % 1)
 -
 - Function: gaussMulList
 - Test Case #: 1
 - Input: [(1, 2), (3, 4), (5, 6)]
 - Expected Output: (-85, 20)
 - Actual Output: ((-85) % 1, 20 % 1)
 -
 - Function: gaussMulList
 - Test Case #: 2
 - Input: [(0, 1), (0, 1)]
 - Expected Output: (-1, 0)
 - Actual Output: ((-1) % 1, 0 % 1)
 -
 - Function: gaussMulList
 - Test Case #: 3
 - Input: []
 - Expected Output: (1, 0)
 - Actual Output: (1 % 1, 0 % 1)
 -
 - Function: gaussCircle
 - Test Case #: 1
 - Input: [(1, 2), (3, 4), (0, 5)] 25
 - Expected Output: [(3, 4), (0, 5)]
 - Actual Output: [(3 % 1, 4 % 1), (0 % 1, 5 % 1)]
 -
 - Function: gaussCircle
 - Test Case #: 2
 - Input: [(0, 0), (1, 0), (0, 1)] 0
 - Expected Output: [(0, 0)]
 - Actual Output: [(0 % 1, 0 % 1)]
 -
 - Function: gaussCircle
 - Test Case #: 3
 - Input: [] 5
 - Expected Output: []
 - Actual Output: []
 ---------------------------------------------------------------------------
 --}
