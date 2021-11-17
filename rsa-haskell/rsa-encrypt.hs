import System.IO
import RsaLib

main = do
		pubKey <- openFile "rsa-pub.key" ReadMode
		key <- hGetLine pubKey
		contents <- getContents
		let (n,e) = read (key) :: (Integer,Integer)
		let encContents = encryptText contents (n,e)
		putStr encContents
		hClose pubKey

{- Returns a string of [Integer] representing the encrypted text -}
encryptText :: String -> (Integer,Integer) -> String
encryptText msg (n,e) = show encodedIntegers 
	where encodedIntegers = map (\x -> powerMod x e n ) $ encodeTextInt msg
