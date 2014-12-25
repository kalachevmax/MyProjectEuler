

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

main = print(take 10 primes)


