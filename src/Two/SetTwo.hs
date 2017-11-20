{-# LANGUAGE PackageImports #-}
import Data.Char
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import Data.ByteString.Base64
import Data.ByteString (ByteString)
import Data.String
import Data.Word
import "cipher-aes" Crypto.Cipher.AES
import Data.Bits (xor)
import Data.ByteString.Internal (unpackBytes)
import GHC.Word (Word8)

{- Challenge 9: Implement PKCS#7 padding -}
pad :: ByteString -> Int -> ByteString
pad bs size
  | (Char8.length bs) >= size  = bs
  | otherwise =  pad (bs `Char8.append` (Char8.pack "\x04")) size

challenge9 :: IO (String)
challenge9 = do
  let s = "YELLOW SUBMARINE"
  let s' = Char8.pack s
  let size = Char8.length s'
  let padded = pad s' 20
  let padded_and_unpacked = Char8.unpack padded
  return padded_and_unpacked


{- Challenge 10: ECB Block Encryption -}
-- Sourced from: https://github.com/thedufer/Cryptopals/blob/master/Two/Ten.hs

-- | Helper functions
longXor :: [Word8] -> [Word8] -> [Word8]
longXor = zipWith xor

groupsOfSize :: Int -> [a] -> [[a]]
groupsOfSize n bs
  | (length bs) >= n = (take n bs):(groupsOfSize n $ drop n bs)
  | otherwise        = []

strToWord8s :: String -> [Word8]
strToWord8s = unpackBytes . Char8.pack

stringToBytes :: String -> [Word8]
stringToBytes = map charToByte

bytesToString :: [Word8] -> String
bytesToString = map byteToChar

charToByte :: Char -> Word8
charToByte = toEnum . fromEnum

byteToChar :: Word8 -> Char
byteToChar = toEnum . fromEnum

base64StringToBytes :: [Char] -> [Word8]
base64StringToBytes xs = B.unpack $ B64.decodeLenient $ Char8.pack xs

dropNewlines :: String -> String
dropNewlines = filter (/= '\n')

-- | ECB Encryption
aesInit :: [Word8] -> AES
aesInit = initAES . B.pack

ecbDecrypt :: AES -> [Word8] -> [Word8]
ecbDecrypt aes = B.unpack . decryptECB aes . B.pack

ecbEncrypt :: AES -> [Word8] -> [Word8]
ecbEncrypt aes = B.unpack . encryptECB aes . B.pack

cbcBlockDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockDecrypt aes iv cipher = longXor (ecbDecrypt aes cipher) iv

cbcDecrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcDecrypt aes iv cipher =
  let blockedCipher = (groupsOfSize 16 cipher)
  in concat $ map (uncurry $ cbcBlockDecrypt aes) $ zip (iv:blockedCipher) blockedCipher

cbcBlockEncrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcBlockEncrypt aes iv plain = ecbEncrypt aes $ longXor plain iv

cbcEncrypt :: AES -> [Word8] -> [Word8] -> [Word8]
cbcEncrypt aes iv plain = concat $ drop 1 $ reverse $ foldl (\ciphers -> \plain -> (cbcBlockEncrypt aes (head ciphers) plain):ciphers) [iv] (groupsOfSize 16 plain)

key = stringToBytes "YELLOW SUBMARINE"
iv :: [Word8]
iv = take 16 $ repeat $ charToByte '\x00'

ch10 :: IO ()
ch10 = (readFile "10.txt") >>= putStrLn . bytesToString . (cbcDecrypt (aesInit key) iv) . base64StringToBytes . dropNewlines
