
main = print(squareOfSum - sumOfSquares)
  where
    squareOfSum = (sum [1..100])^2
    sumOfSquares = foldl1 (\acc x -> acc + x^2) [1..100]
