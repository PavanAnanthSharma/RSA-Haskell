import System.IO
import System.Random
import RsaLib

main = do
	pubKey <- openFile "rsa-pub.key" WriteMode
	prvKey <- openFile "rsa-prv.key" WriteMode
	randGen <- getStdGen
	putStr "Generating key, please wait...\n"
	let (p,q) = getFactors $ primeList randGen
	hPutStrLn pubKey $ getPubKey (p,q)
	hPutStrLn prvKey $ getPrvKey (p,q)
	putStr "Success!\n"
	putStr "Public Key: rsa-pub.key\n"
	putStr "Private Key: rsa-prv.key\n"
	hClose pubKey
	hClose prvKey

{- Returns the string (n,e) -}
getPubKey :: (Integer, Integer) -> String
getPubKey (p, q) =  show (n,e)
	where n = getN (p,q)
	      e = 3

{- Returns the string (n,d) -}
getPrvKey :: (Integer, Integer) -> String
getPrvKey (p,q) = show (n,d) 
	where n = getN (p,q)
	      d = getD $ getPhi (p,q)

{- Returns p * q -}
getN :: (Integer, Integer) -> Integer
getN (p, q) = p*q

{- Extented Eucledian Algoritm, returns (x,y) from Bezut's identity -}
extendedGCD :: (Integer,Integer) -> (Integer, Integer)
extendedGCD (a,b)
	| (b == 0) = (1,0)
	| otherwise = (t, z)
	where (s,t) = extendedGCD(b, a `mod` b)
	      z = s - ( (a `div` b) * t)

{- Returns phi(N) -}
getPhi :: (Integer, Integer) -> Integer
getPhi (p, q) = (p-1) * (q-1)

{- Returns d so that (d*e) `mod` phi(N) == 1 -}
getD :: Integer -> Integer
getD phi = if y < 0 then (y + phi) else y
	where (_,y) = extendedGCD (phi, e)
	      e = 3

{- Returns a list of prime numbers between 2^211 and 2^513 -}
primeList :: StdGen -> [Integer]
primeList gen = filter (isRabinMillerPrime gen) randomList
	where randomList = randomRs ( (2^511), (2^513) ) gen
		  
		  
{- Find a factor p so that p is prime and p `mod` e /= 1 -}	
findFactor :: [Integer] -> Int -> (Integer, Int)
findFactor primes x = if ( current `mod` e /= 1 ) 
		      then (current,x)
		      else findFactor primes (x + 1)
	where current = primes !! x
	      e = 3

{- Returns (p,q) -}
getFactors :: [Integer] -> (Integer, Integer)
getFactors primes = (p, fst $ findFactor primes (pi+1))
					where (p,pi) = findFactor primes 0

{- Checks whether an Integer is a Rabin-Miller prime -}
isRabinMillerPrime :: StdGen -> Integer -> Bool
isRabinMillerPrime gen n = if n `mod` 2 == 0 
			   then False
			   else result 
	where result = foldl (\acc x -> (acc == True && x == True)) True rlist
	      rlist = map (millerRabinTest s n ) (millerRabinList n 30 gen)
	      (_,s) = millerRabinForm n

{- Returns (d,s) where (2^s)*d = n -}
millerRabinForm :: (Integral a) => a -> (a, a)
millerRabinForm n = (d, fromIntegral s)
	where pn = pred n
	      factorList = iterate (`div` 2) pn
	      s = length $ takeWhile (\x -> x `mod` 2 == 0) $ factorList
	      d = factorList !! s

{- Creates the list of ((a^d) mod n)s where a is random. 1s and (n-1)s are discarded -}
millerRabinList :: Integer -> Int -> StdGen -> [Integer]
millerRabinList _ 0 _ = []
millerRabinList n k gen  = 
	let (d,s) = millerRabinForm n
	    (a,newGen) = randomR (2, (n-2)) gen {- We can use the sequence A014233 in OEIS instead -}
	    rabinTest = powerMod a d n
	    result = if ( rabinTest == 1 || rabinTest == (n-1) ) 
			then millerRabinList n (k-1) newGen
			else rabinTest : millerRabinList n (k-1) newGen  
	in result
	
{- Inner loop of the Rabin-Miller algorithm -}
millerRabinTest :: (Integral a, Integral b) => b-> a -> a -> Bool
millerRabinTest s n x
	| xmsq == 1 = False
	| s == 1 = False 
	| xmsq == (n-1) = True
	| otherwise  = millerRabinTest (s-1) n xmsq
	where xmsq = powerMod x 2 n

