module RsaLib
( encodeTextInt
, decodeIntText
, powerMod ) where 

{- Import Modules -}
import Data.Char 
{- We use base 128 in order to be able to encode
   all ASCII characters.
   Key length is 1024 bits ~ 2^1024 and each encoded Integer should be 
   less than that. 
   64 Characters/block creates Integers <= 350 bits which is fine
-}

{- Useful functions -}

{- Takes some text and returns a list of integers <= 350 bits each which represent it -}
encodeTextInt :: String -> [Integer]
encodeTextInt msg = map (blockToInt 128 0) $ splitBlock 64 $ textToBlock msg 

{- Takes a list of integers representing some text and returns the text -}
decodeIntText :: [Integer] -> String 
decodeIntText msg = unwords splitText 
		  where splitText = map (blockToText . intToBlock) msg

{- **** Returns (b^e) mod n **** -}
powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod b e m 
	| e == 0 = 1
	| (odd e) = ( b * (powerMod (mbsq) (e `div` 2) m)) `mod` m 
	| otherwise = (powerMod (mbsq) (e `div` 2) m)
	where mbsq = (b*b) `mod` m


{- Helper functions -}

{- Replace each char with its ASCII value -}
textToBlock :: [Char] -> [Int]
textToBlock msg = map (ord) msg

{- Replace each ASCII value with the appropriate char -}
blockToText :: [Int] -> [Char]
blockToText msg = map (chr) msg 

{- Split the ASCII values in blocks of size len -} 
splitBlock :: Int -> [Int] -> [[Int]]
splitBlock _ [] = []
splitBlock len msgBlock = frontBlock : splitBlock len restOfTheBlock 
			 where frontBlock = take len msgBlock
			       restOfTheBlock = drop len msgBlock 

{- Create an intger base "base" from a list of Ints -} 
blockToInt :: Integer -> Integer -> [Int] -> Integer
blockToInt _ _ [] = 0
blockToInt base exp (x:xs) = (fromIntegral (x) * base^exp) + (blockToInt base (exp+1) xs)

{- Create a list containing the Integer's digits in base numberBase -}
intToBlock :: Integer -> [Int]
intToBlock 0 = []
intToBlock msgi = fromIntegral (msgi `mod` 128) : intToBlock (msgi `div` 128)
