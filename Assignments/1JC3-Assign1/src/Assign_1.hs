{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2022
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 -- McMaster CS 1JC3 2025.

  Modified by W. M. Farmer 13 September 2025.
-}
module Assign_1 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS AND DO NOT ADD ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Aaron Sit
-- Date: October 9, 2025
macid :: String
macid = "sita1"

(***) :: Double -> Double -> Double
x *** y = if x >= 0 then x ** y else -((-x) ** y)

(===) :: Double -> Double -> Bool
x === y =
  let tol = 1e-3
  in abs (x-y) <= tol

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description:
 -   Computes the Q parameter to solve cubic equations.
 -   For a cubic ax^3 + bx^2 + cx + d = 0, Q = (3ac - b^2) / (9a^2)
 -}
cubicQ :: Rational -> Rational -> Rational -> Rational
cubicQ a b c = (3 * a * c - b * b) / (9 * a * a)

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description:
 -   Computes the R parameter to solve cubic equations.
 -   For a cubic ax^3 + bx^2 + cx + d = 0, R = (9abc - 27a^2d - 2b^3) / (54a^3)
 -}
cubicR :: Rational -> Rational -> Rational -> Rational -> Rational
cubicR a b c d = (9 * a * b * c - 27 * a * a * d - 2 * b * b * b) / (54 * a * a * a)

{- -----------------------------------------------------------------
 - cubicDiscSign
 - -----------------------------------------------------------------
 - Description:
 -   Computes the sign of the discriminant Q^3 + R^2 for a cubic equation.
 -   Returns -1 if negative (3 real roots), 0 if zero (repeated roots), 
 -   1 if positive (1 real root)
 -}
cubicDiscSign :: Rational -> Rational -> Int
cubicDiscSign q r =
  let disc = q*q*q + r*r
  in if disc < 0 then -1
     else if disc > 0 then 1
     else 0

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description:
 -   Computes the S parameter: S = cuberoot(R + sqrt(Q^3 + R^2))
 -   Uses the custom (***) operator for signed cube roots
 -}
cubicS :: Rational -> Rational -> Double
cubicS q r = 
  let qDouble = fromRational q
      rDouble = fromRational r
      disc = qDouble*qDouble*qDouble + rDouble*rDouble
  in (rDouble + sqrt disc) *** (1/3)

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description:
 -   Computes the T parameter: T = cuberoot(R - sqrt(Q^3 + R^2))
 -   Uses the custom (***) operator for signed cube roots
 -}
cubicT :: Rational -> Rational -> Double
cubicT q r = 
  let qDouble = fromRational q
      rDouble = fromRational r
      disc = qDouble*qDouble*qDouble + rDouble*rDouble
  in (rDouble - sqrt disc) *** (1/3)

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description:
 -   Finds all real solutions to the cubic equation ax^3 + bx^2 + cx + d = 0.
 -   Returns a list of real solutions.
 -}
cubicRealSolutions :: Rational -> Rational -> Rational -> Rational -> [Double]
cubicRealSolutions a b c d
  | a == 0      = []
  | sign == -1  = -- Three real roots (use trigonometric method)
      let qDouble = fromRational q
          rDouble = fromRational r
          rho = sqrt (-qDouble*qDouble*qDouble)
          theta = acos (rDouble / rho)
          bTerm = fromRational b / (3 * fromRational a)
          x1 = 2 * (rho *** (1/3)) * cos (theta / 3) - bTerm
          x2 = 2 * (rho *** (1/3)) * cos ((theta + 2*pi) / 3) - bTerm
          x3 = 2 * (rho *** (1/3)) * cos ((theta + 4*pi) / 3) - bTerm
      in [x1, x2, x3]
  | sign ==  0  = -- Repeated roots
      let bTerm = fromRational b / (3 * fromRational a)
          rDouble = fromRational r
          x1 = 2 * (rDouble *** (1/3)) - bTerm
          x2 = -(rDouble *** (1/3)) - bTerm
      in if abs rDouble < 1e-10 then [x2, x2, x2] else [x1, x2, x2]
  | sign ==  1  = -- One real root
      let bTerm = fromRational b / (3 * fromRational a)
          x1 = s + t - bTerm
      in [x1]
  | otherwise   = []
  where
    sign = cubicDiscSign q r
    s    = cubicS q r
    t    = cubicT q r
    q    = cubicQ a b c
    r    = cubicR a b c d

{- -----------------------------------------------------------------
 - Test Cases
 - Function name: cubicQ
  - Test cases:
  -   cubicQ 1 6 11
  -     Expected output: 1
  -   cubicQ 2 3 4
  -     Expected output: -0.020833333333333332
 - Function name: cubicR
  - Test cases:
  -   cubicR 1 6 11 6
  -     Expected output: -1
  -   cubicR 2 3 4 5
  -     Expected output: -0.16276041666666666
  - Function name: cubicDiscSign
  - Test cases:
  -   cubicDiscSign 1 (-1)
  -     Expected output: 1
  -   cubicDiscSign 0 0
  -     Expected output: 0
  -   cubicDiscSign (-1) 0
  -     Expected output: -1
  - Function name: cubicS
  - Test cases:
  -   cubicS 1 (-1)
  -     Expected output: 1.0
  -   cubicS (-1) 0
  -     Expected output: 0.0
  - Function name: cubicT
  - Test cases:   
  -   cubicT 1 (-1)
  -     Expected output: 1.0
  -   cubicT (-1) 0
  -     Expected output: 0.0    
  - Function name: cubicRealSolutions
  - Test cases:
  -   cubicRealSolutions 1 6 11 6
  -     Expected output: [-1.0,-2.0,-3.0]
  -   cubicRealSolutions 1 (-3) 3 (-1)
  -     Expected output: [1.0,1.0,1.0]
  -   cubicRealSolutions 1 0 0 1
  -     Expected output: [-1.0]
 - -----------------------------------------------------------------
 -}