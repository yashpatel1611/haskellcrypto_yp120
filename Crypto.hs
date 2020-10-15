module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

-- Returns the greatest common divisor (or higest common factor) of two numbers
-- Uses recursion with base cases where either number is a zero
gcd :: Int -> Int -> Int
gcd n m
  | n == 0    = m
  | m == n    = n
  | m == 0    = n
  | otherwise = gcd (mod m n) n

-- Implementation of Euler's totient function
-- Returns the number of co-prime numbers that a given number has
-- Uses list comprehension and the above gcd function
phi :: Int -> Int
phi m
  = length [x | x <- [1..m], gcd m x == 1]


-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
-- Uses recursion to find coefficients with base case of either 
-- number being a zero
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a b
  | b == 0    = (1, 0)
  | a == 0    = (0, 1)
  | otherwise = (v', (u' - (q * v')))
  where
      q  = fst (quotRem a b)
      r  = snd (quotRem a b)
      v' = snd (computeCoeffs b r)
      u' = fst (computeCoeffs b r)

-- Inverse of a modulo m
-- Uses the computeCoeffs function and mod function
inverse :: Int -> Int -> Int
inverse a m
  = (fst (computeCoeffs a m)) `mod` m

-- Calculates (a^k mod m)
-- Uses recursion with the base case being either k == 0 or k == 1
modPow :: Int -> Int -> Int -> Int
modPow a k m
  | k == 0 = 1 `mod` m
  | k == 1 = a `mod` m
  | even k = (modPow ((a^2) `mod` m) j m) `mod` m
  | odd k  = (a * (modPow ((a^2) `mod` m) j m)) `mod` m
  where
    j = k `div` 2

-- Returns the smallest integer that is coprime with phi
-- Uses an auxillary (or helper) function to loop through natural numbers
-- Uses the gcd function created above
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf a
  = smallestCoPrimeOf' a 2
  where
    smallestCoPrimeOf' a x
      | gcd a x == 1 = x
      | otherwise    = smallestCoPrimeOf' a (x + 1)


-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
-- Uses the smallestCoPrimeOf function above
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q
  = ((e, n), (d, n))
  where
    n = p * q
    e = smallestCoPrimeOf ((p - 1) * (q - 1))
    d = inverse e ((p - 1) * (q - 1))

-- RSA encryption/decryption
-- Uses modPow function, otherwise generates an overflow,
-- producing incorrect results
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e, n)
  = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n)
  = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt c
  = (ord c) - (ord 'a')

-- Returns the n^th letter
toChar :: Int -> Char
toChar x
  = chr (x + ord 'a')

-- "adds" two letters
add :: Char -> Char -> Char
add c1 c2
  = toChar (mod ((toInt c1) + (toInt c2)) n)
  where
    n = (ord 'z') - (ord 'a') + 1 

-- "substracts" two letters (subtract?)
substract :: Char -> Char -> Char
substract c1 c2
  = toChar (mod ((toInt c1) - (toInt c2)) n)
  where
    n = (ord 'z') - (ord 'a') + 1

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt k m
  = undefined

ecbDecrypt :: Char -> String -> String
ecbDecrypt
  = undefined

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt
  = undefined

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt
  = undefined
