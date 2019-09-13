-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Fermat's algorithm to test primality
-- This is a probabilistic algorithm
-- False means it is NOT a prime
-- True means it probably is prime
-- First parameter is a integer to be tested
-- The second is how many times it must be tested

-- 🇧🇷
-- Algoritmo de Fermat para testar primalidade
-- É um algoritmo probabilístico
-- False significa que o número NÃO é primo
-- True significa que o número provavelmente é primo
-- O primeiro parâmetro é um inteiro a ser testado
-- O segundo é quantas vezes ele deve ser testado

-- fermat_test
-- @param prime (integer)
-- @param repeat (integer)
-- @param seed (seed)
-- @return is_prime (Bool)

module FermatTest where

import System.Random
import ModularExp
import GenerateRandom


fermat_test :: (Integral t, Random t, RandomGen g) => t -> t -> g -> Bool
fermat_test prime n seed = is_prime
  where
    arr = gen_random_arr 2 (prime -1) n seed
    booleans = map (\a -> unitary_test prime a) arr
    is_prime = foldl (&&) True booleans

unitary_test :: (Integral a, Random a) => a -> a -> Bool
unitary_test prime a
  | mod_exp a (prime-1) prime == 1 = True
  | otherwise = False
