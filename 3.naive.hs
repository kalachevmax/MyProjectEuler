

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral


primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]


primeFactors :: Integer -> [Integer]
primeFactors x
  | null ps = [x]
  | otherwise = ps
  where
    ps = [p | p <- takeWhile (<= isqrt x) primes, x `mod` p == 0]


main = print(primeFactors 210)
