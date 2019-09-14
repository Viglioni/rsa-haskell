-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- 🇬🇧
-- Get public key (e,n) and a corresponding private key (d) using two primes p and q
-- One must install System.Random
-- $ cabal install random
-- Extendend GCD and GCD algorithm is also needed

-- 🇧🇷
-- Cria a chave pública (n,e) e a chave privada (d) correspondente, a partir de dois primos p e q
-- É necessário instalar System.Random
-- $ cabal install random
-- Os algoritmos de MDC e MDC expandidos são necessários

-- generate_keys
-- @param  p (prime number)
-- @param  q (prime number)
-- @return (public_key, private_key) (non-netative integer)

module RSAkeys (gen_keys_by_bits) where

import System.Random
import EGcd
import PrimeGenerator



gen_keys_by_bits :: (RandomGen g) => Int -> g -> g -> ((Integer,Integer),Integer)
gen_keys_by_bits bits seed1 seed2 = gen_keys p q seed1
  where
    p = PrimeGenerator.prime_gen_by_bits bits seed1
    q = PrimeGenerator.prime_gen_by_bits bits seed2
    
  
    

gen_keys :: (RandomGen g) => Integer -> Integer -> g -> ((Integer,Integer),Integer)
gen_keys p q seed = (public, private)
  where
    n = p*q
    phi = (p-1)*(q-1)
    e = get_public_exp phi seed 
    public = (e,n)
    private = get_private phi e


get_public_exp :: (RandomGen g) => Integer -> g -> Integer
get_public_exp phi seed 
  | gcd (e+2) phi == 1 = (e+2)
  | otherwise = get_public_exp phi new_seed 
  where
    (e, new_seed) = randomR (1, ((phi)-3) ) seed

           
get_private :: Integer -> Integer -> Integer
get_private phi e = mod coef_from_egcd phi
  where coef_from_egcd = trd $ eGCD phi e


trd :: (a,b,c) -> c
trd (_,_,x) = x
