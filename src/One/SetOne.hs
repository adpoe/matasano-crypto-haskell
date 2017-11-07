{-# LANGUAGE OverloadedStrings #-}
module SetOne
    (xorStrings) where

import Test.Hspec

import Data.ByteString as B
import Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base16 as Base16
import Data.Bits (xor, popCount)
import Data.Map as M
import Data.Char
import Data.Ord
import Data.List

{- Challenge 1 -}
input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

hex2binary :: String -> ByteString
hex2binary s = (fst . Base16.decode) $ Char8.pack s
-- yields: "I'm killing your brain like a poisonous mushroom"

binary2base64 :: ByteString -> ByteString
binary2base64 s = Base64.encode s

challenge1 :: IO ()
challenge1 = hspec $ do
  describe "Confirm Base64 decode" $ do
    it "Base64 yields: I'm killing your brain like a poisonous mushroom" $ do
      (binary2base64 (hex2binary input)) `shouldBe` output

{- Challenge 2 -}
xor1 = fst $ Base16.decode "1c0111001f010100061a024b53535009181c"
xor2 = fst $ Base16.decode "686974207468652062756c6c277320657965"
-- *FixedXor> xor2
-- "hit the bull's eye"

xorOutput = fst $ Base16.decode "746865206b696420646f6e277420706c6179"
-- *FixedXor> xorOutput
-- "the kid don't play"

xorStrings :: ByteString -> ByteString -> ByteString
xorStrings b1 b2 = B.pack $ B.zipWith xor b1 b2
-- *FixedXor> xorStrings xor1 xor2
-- "the kid don't play"

challenge2 :: IO ()
challenge2 = hspec $ do
  describe "Xor Strings" $ do
    it "Confirm FixedXor is functional" $ do
      (xorStrings xor1 xor2) `shouldBe` xorOutput

{- Challenge 3 -}
singleByte = fst $ Base16.decode "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

repeatStr :: ByteString -> Char -> ByteString
repeatStr b c = B.replicate (B.length b) (I.c2w c)

xor' :: ByteString -> Char -> ByteString
xor' b c = xorStrings b (repeatStr b c)

xorAll :: ByteString -> [ByteString]
xorAll b = Prelude.map (xor' b) (['0' .. 'z'])

scores :: M.Map Char Double
scores = M.fromList $ Prelude.zip (Prelude.reverse "etaoin shrdlu") [1,2..]

scoreLookup :: Char -> Double
scoreLookup c = case M.lookup (toLower c) scores of
  Just score -> score
  Nothing    -> 0

getScore :: [Char] -> Double
getScore b = sum $ Prelude.map scoreLookup b

getAllScores :: [ByteString] -> [Double]
getAllScores bs = Prelude.map (getScore . Char8.unpack) bs

findMaxLikelihood :: ByteString -> (ByteString, Double)
findMaxLikelihood bs = maximumBy (comparing snd) (Prelude.zip xs (getAllScores xs))
  where xs = xorAll bs

challenge3 :: IO ()
challenge3 = hspec $ do
  describe "Challenge #3" $ do
    it "Confirm we find: Cooking MC's like a pound of bacon" $ do
      fst (findMaxLikelihood singleByte) `shouldBe` "Cooking MC's like a pound of bacon"


{- Challenge 4: Detect Single Character XOR -}
challenge4 :: IO (ByteString, Double)
challenge4 = Prelude.readFile "data/4.txt"
  >>= return
  . maximumBy (comparing snd)
  . Data.List.map findMaxLikelihood
  . Data.List.map hex2binary
  . Data.List.lines
-- SetOne Data.Ord Data.List Control.Monad> challenge4
-- ("Now that the party is jumping\n",183.0)

{- Challenge 5: Implement Repeating Key XOR -}
poetry :: String
poetry = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

repeatingKeyXor :: String -> String -> String
repeatingKeyXor _ []  = []
repeatingKeyXor key str = Char8.unpack (xorStrings keyPacked this) ++ repeatingKeyXor key rest
  where
    keyPacked = Char8.pack key
    keyLen = Data.List.length key
    this = (Char8.pack . Prelude.take keyLen) str
    rest = Prelude.drop keyLen str

repeatingKeyXor' :: String -> String -> ByteString
repeatingKeyXor' key str = (Base16.encode . Char8.pack) (repeatingKeyXor key str)

ch5output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

challenge5 :: IO ()
challenge5 = hspec $ do
  describe "Challenge #5" $ do
    it "Confirm repeatingKeyXor' is functional" $ do
      (repeatingKeyXor' "ICE" poetry) `shouldBe` ch5output

{- Challenge 6: Break repeating-key XOR -}
hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = sum $ Data.List.map popCount xored
  where xored  = B.zipWith xor (Char8.pack s1) (Char8.pack s2)
