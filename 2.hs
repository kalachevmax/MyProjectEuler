
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million,
-- find the sum of the even-valued terms.

s = sum [x | x <- takeWhile (<= 4000000) fibs, even x]
  where fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))

main = print(s)
