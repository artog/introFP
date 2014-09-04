power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Assignment 1
--Computing power n k takes k+1 steps

--Assignment 2

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product [n*x `div` x | x <- [1..k]]

--Assignment 3

power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) $ k `div` 2
power2 n k | otherwise = n * power2 (n*n) ((k-1) `div` 2)

--Assignment 4
--Test cases:
--0^k (k = 0, 1, 5) - ^0 is a special case, ^1 and 
--                    ^5 should give 0
--4^k (k = 0) - Postive n^0 should be 1
--(-4)^k (k = 0, 1, 2) 
--     With a negative base ^0 should be 1, 
--     ^odd should be a negative and 
--     ^even should be positive
--2^k (k = 5, 6) To test odd and even exponents

-- Compare power to power1, with input n k
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = (power n k) == (power1 n k)

-- Compare power to power2, with input n k
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = (power n k) == (power2 n k)

-- Test our test cases for power1, if they are all true
testComparePower1 :: Bool
testComparePower1 = and [ comparePower1 x y | x <- [0, 2, 4, (-4)], y <- [0, 1, 2, 5, 6]]

-- Test our test cases for power2, if they are all true
testComparePower2 :: Bool
testComparePower2 = and [ comparePower2 x y | x <- [0, 2, 4, (-4)], y <- [0, 1, 2, 5, 6]]

-- Checks if both power1 and power2 for all test cases
test :: Bool
test = and [testComparePower1, testComparePower2]


-- Assignment 5

-- Calculate length of string
getStringLength :: Integer -> Integer
getStringLength x = max 6 $ toInteger $ length $ show $ x

-- Calculate how many spaces to pad a string with to achieve length l
getPadLength :: String -> Integer -> Integer
getPadLength str l = (l - k) 
	where k = toInteger $ length $ str

-- Pad a string to length l from the right. Like: "text    "
padRight :: String -> Integer -> String
padRight str l = concat $ [str] ++ pad
	where 
		pad = take num $ repeat " "
		num = fromInteger $ getPadLength str l

-- Format a row of strings into with padding of each segment to maxLength
formatRow :: [String] -> Integer -> String
formatRow row maxLength = unwords [padRight i maxLength | i <- row]

buildNumberList :: Integer -> Integer -> [String]
buildNumberList x k = [
		show k,
		show $ power  x k,
		show $ power1 x k, 
		show $ power2 x k
	]

-- Builds the table, returns a list with each row as Strings
buildTable :: Integer -> Integer -> [[String]]
buildTable x n = 
	   [["n","power","power1","power2"]] ++ [buildNumberList x k | k <- [0..n]]
		
-- Format all the rows in a table
formatTable :: [[String]] -> Integer ->  [String]
formatTable list l = [formatRow r l | r <- list]

-- Outputs a table 
printTable :: Integer -> Integer -> IO()
printTable x n = putStr $ unlines $ formatTable (buildTable x n) $ getStringLength $ power x n

--main = printTable 2 15
tab :: Html
tab = simpleTable [] []
    [ [toHtml e | e <- r] | r <- (buildTable 3 5) ]

main = writeFile "table.html" (renderHtml tab)














