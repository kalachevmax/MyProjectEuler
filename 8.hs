
-- Find the thirteen adjacent digits in the 1000-digit number that have the greatest product.
-- What is the value of this product?

import Data.Char
import Data.List

main = do
  str <- readFile("number")
  print . maximum . map product
        . foldr (zipWith (:)) (repeat [])
        . take 13 . tails . map (fromIntegral . digitToInt)
        . concat . lines $ str
