import Data.Char

import Data.ByteString.Char8 as Char8


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
