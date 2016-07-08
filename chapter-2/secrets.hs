largestChar :: Int
largestChar = fromEnum (maxBound :: Char)

rotN :: Int -> Char -> Char
rotN n c = toEnum rotation
   where halfN = n `div` 2
         offset = (fromEnum c) + halfN
         rotation = n `mod` offset

rot :: Char -> Char
rot val = toEnum rotation
 where n = fromEnum (maxBound :: Char)
       halfN = n `div` 2
       offset = (fromEnum val) + halfN
       rotation = n `mod` offset

rotEncode :: String -> String
rotEncode text = map rot text

xor' :: Bool -> Bool -> Bool
xor' value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xor' v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

inputList = [True,True,False,False,True,False,False,True]
encodingList = [False,True,False,True,True,True,False,True]
encrypted = inputList `xor` encodingList
decoded = encrypted `xor` encodingList

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : (intToBits' nextVal)
               else True : (intToBits' nextVal)
 where remainder = n `mod` 2
       nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
   where reversedBits = reverse  (intToBits' n)
         missingBits = maxBits - (length reversedBits)
         leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
 where size = (length bits)
       indices = [size-1,size-2 .. 0]
       trueLocations = filter (\x ->
                                ((fst x) == True))
                       (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =  map (\pair ->
                                 (fst pair) `xor` (snd pair))
                           (zip padBits plaintextBits)
 where padBits =  map charToBits pad
       plaintextBits =  map charToBits plaintext


applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
 where bitList = applyOTP' pad plaintext


class Cipher a where
   encode :: a -> String -> String
   decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
   encode Rot text = rotEncode text
   decode Rot text = rotEncode text

data OneTimePad = OTP String

instance Cipher OneTimePad where
   encode (OTP pad) text = applyOTP pad text
   decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
