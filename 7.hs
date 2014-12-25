
-- What is the 10 001st prime number?

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

main = print(primes !! 10000)
