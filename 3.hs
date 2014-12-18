
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors x (p:ps)
  | p > x          = []
  | x `mod` p == 0 = p : primeFactors (x `div` p) (p:ps)
  | otherwise      = primeFactors x ps

main = print(last(primeFactors 600851475143 primes))
