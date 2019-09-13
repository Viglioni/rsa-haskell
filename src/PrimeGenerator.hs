-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0 

-- :gb:
-- Algorithm to generate randomly a prime number
-- prime_generator a b -> generates a prime p such that a < p < b.
-- prime_gen_by_bits a -> generates a prime p such that 2^a < p < 2^(a+1), i.e. p has a bits.
-- BE CAREFUL! This algorithm may never stop! If there is no prime in range (a,b) it will run forever.
-- BE CAREFUL![2] The prime generated is tested with two probabilistic algorithms (fermat 50 times and  miller-rabin, also 50 times) but there is a slightly chance that the number is not a prime. 

-- 🇧🇷
-- Algoritmo para gerar aleatoriamente um número primo
-- prime_generator a b -> gera um primo p tal que a<p<b
-- prime_gen_by_bits a -> gera um primo p tal que 2^a < p < 2^(a+1), i.e. p tem a bits.
-- ATENÇÃO! Este algoritmo pode não parar! Se não houver nenhum primo no intervalo (a,b) o algoritmo irá rodar para sempre.
-- ATENÇÃO![2] O primo gerado é testado usando dois algoritmos probabilísticos (fermat 50 vezes e miller-rabin outras 50), mas existe uma pequena chance de o número gerado não ser primo.

-- prime_gen
-- @param a (integer)
-- @param b (integer)
-- @return p (integer)

-- prime_gen_by_bits
-- @param a (integer)
-- @return p (integer)


module PrimeGenerator where

import System.Random
import FermatTest
import MillerRabinTest


prime_gen_by_bits :: (RandomGen g) => Int -> g ->  Integer
prime_gen_by_bits bits seed = prime_gen (2^(bits-1)) (2^bits) seed

prime_gen :: (RandomGen g) => Integer -> Integer -> g -> Integer
prime_gen min_val max_val seed 
  | is_prime = (number)
  | otherwise = prime_gen min_val max_val new_seed
  where
    (number, new_seed) = (randomR (min_val, max_val) seed) 
    fermat = fermat_test number 1 new_seed
    miller_rabin = miller_rabin_test number 1 (snd $ next new_seed)
    is_prime =  fermat  && miller_rabin

