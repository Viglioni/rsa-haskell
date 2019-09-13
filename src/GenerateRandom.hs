-- @author Laura Viglioni
-- 2019
-- GNU General Public License v3.0

module GenerateRandom where

import System.Random


-- gen_random_arr
-- generate array of random ints such that each int is bounded min_val < a < max_val
-- @param min_val (integer)
-- @param max_val (integer)
-- @param n (integer)
-- @param seed (seed)
-- @return [a1,a2,...,an] (arr of integers)

gen_random_arr :: (Integral a, Random a, RandomGen g) => a -> a ->  a -> g ->  [a]
gen_random_arr min_val max_val n seed = 
   map (fst)
   (foldl
    (\ (a:cc) _ -> (randomR (min_val,max_val) $ snd a):(a:cc))
    [ randomR (min_val,max_val) seed ]
    [1..(n-1)])
