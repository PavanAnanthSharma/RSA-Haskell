import System.IO
import RsaLib

main = do
		prvKey <- openFile "rsa-prv.key" ReadMode
		key <- hGetLine prvKey
		contents <- getContents
		let (n,d) = read (key) :: (Integer,Integer)
		let decContens = decryptText contents (n, d)
		putStrLn decContens
		hClose prvKey
		
{- Recovers the decrypted text from a string containing an encrypted [Integer] list -}
decryptText :: String -> (Integer,Integer) -> String
decryptText msg (n,d) = decodeIntText $ map (\x -> powerMod x d n ) $ encryptedList
	where encryptedList = read msg :: [Integer] 
