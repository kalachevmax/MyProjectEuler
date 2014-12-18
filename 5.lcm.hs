

gcd1 :: Int -> Int -> Int
gcd1 a 0 = a
gcd1 a b = gcd b (a `mod` b)

lcm1 :: Int -> Int -> Int
lcm1 a b = abs(a * b) `div` gcd1 a b

main = print(lcm1 10 25)
