
digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

isPalindrome :: Integer -> Bool
isPalindrome x = reverse digitsList == digitsList && length digitsList > 1
  where digitsList = digits x

palindromes = [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

main = print(maximum palindromes)