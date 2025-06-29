module Set14a where

-- Remember to browse the docs of the Data.Text and Data.ByteString
-- libraries while working on the exercises!

import Mooc.Todo

import Data.Bits
import Data.Char
import Data.Text.Encoding
import Data.Word
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

------------------------------------------------------------------------------
-- Ex 1: Greet a person. Given the name of a person as a Text, return
-- the Text "Hello, <name>!". However, if the name is longer than 15
-- characters, output "Hello, <first 15 characters of the name>...!"
--
-- PS. the test outputs and examples print Text values as if they were
-- Strings, just like GHCi prints Texts as Strings.
--
-- Examples:
--  greetText (T.pack "Martin Freeman") ==> "Hello, Martin Freeman!"
--  greetText (T.pack "Benedict Cumberbatch") ==> "Hello, Benedict Cumber...!"

greetText :: T.Text -> T.Text
greetText name = case T.length name <= 15 of
    True  -> T.pack ("Hello, " ++ T.unpack name ++ "!")
    False -> T.pack ("Hello, " ++ take 15 (T.unpack name) ++ "...!")

------------------------------------------------------------------------------
-- Ex 2: Capitalize every second word of a Text.
--
-- Examples:
--   shout (T.pack "hello how are you")
--     ==> "HELLO how ARE you"
--   shout (T.pack "word")
--     ==> "WORD"


shout :: T.Text -> T.Text
shout text = T.unwords $ alternateCase (cycle [capitalizeWord, id]) (T.words text)
    where
        alternateCase :: [T.Text -> T.Text] -> [T.Text] -> [T.Text]
        alternateCase _ [] = []
        alternateCase (f:fs) (x:xs) = f x : alternateCase fs xs
        alternateCase [] _ = []

        capitalizeWord :: T.Text -> T.Text
        capitalizeWord = T.toUpper

------------------------------------------------------------------------------
-- Ex 3: Find the longest sequence of a single character repeating in
-- a Text, and return its length.
--
-- Examples:
--   longestRepeat (T.pack "") ==> 0
--   longestRepeat (T.pack "aabbbbccc") ==> 4

longestRepeat :: T.Text -> Int
longestRepeat text
  | T.null text = 0    
  | otherwise = maximum $ scanText text ' ' 0 0
  where
    scanText :: T.Text -> Char -> Int -> Int -> [Int]
    scanText t prevChar currentLength maxLen
      | T.null t = [max maxLen currentLength] 
      | otherwise =
          let currentChar = T.head t
              rest = T.tail t
          in if currentChar == prevChar
               then scanText rest currentChar (currentLength + 1) maxLen
               else max maxLen currentLength : scanText rest currentChar 1 (max maxLen currentLength)

------------------------------------------------------------------------------
-- Ex 4: Given a lazy (potentially infinite) Text, extract the first n
-- characters from it and return them as a strict Text.
--
-- The type of the n parameter is Int64, a 64-bit Int. Can you figure
-- out why this is convenient?
--
-- Example:
--   takeStrict 15 (TL.pack (cycle "asdf"))  ==>  "asdfasdfasdfasd"

takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n = T.concat . map (T.pack . (:[])) . take (fromIntegral n) . TL.unpack

------------------------------------------------------------------------------
-- Ex 5: Find the difference between the largest and smallest byte
-- value in a ByteString. Return 0 for an empty ByteString
--
-- Examples:
--   byteRange (B.pack [1,11,8,3]) ==> 10
--   byteRange (B.pack []) ==> 0
--   byteRange (B.pack [3]) ==> 0

byteRange :: B.ByteString -> Word8
byteRange bs
  | B.null bs = 0    
  | otherwise = maxByte - minByte
  where
    (minByte, maxByte) = B.foldl' updateMinMax (255, 0) bs

    updateMinMax :: (Word8, Word8) -> Word8 -> (Word8, Word8)
    updateMinMax (currentMin, currentMax) byte =
      (min currentMin byte, max currentMax byte)

------------------------------------------------------------------------------
-- Ex 6: Compute the XOR checksum of a ByteString. The XOR checksum of
-- a string of bytes is computed by using the bitwise XOR operation to
-- "sum" together all the bytes.
--
-- The XOR operation is available in Haskell as Data.Bits.xor
-- (imported into this module).
--
-- Examples:
--   xorChecksum (B.pack [137]) ==> 137
--   xor 1 2 ==> 3
--   xorChecksum (B.pack [1,2]) ==> 3
--   xor 1 (xor 2 4) ==> 7
--   xorChecksum (B.pack [1,2,4]) ==> 7
--   xorChecksum (B.pack [13,197,20]) ==> 220
--   xorChecksum (B.pack [13,197,20,197,13,20]) ==> 0
--   xorChecksum (B.pack []) ==> 0

xorChecksum :: B.ByteString -> Word8
xorChecksum bs
  | B.null bs = 0
  | otherwise = foldr1 xorByte [b | b <- B.unpack bs]
    where
        xorByte :: Word8 -> Word8 -> Word8
        xorByte a b = fromIntegral $ (fromIntegral a :: Int) `xor` (fromIntegral b :: Int)

------------------------------------------------------------------------------
-- Ex 7: Given a ByteString, compute how many UTF-8 characters it
-- consists of. If the ByteString is not valid UTF-8, return Nothing.
--
-- Look at the docs of Data.Text.Encoding to find the right functions
-- for this.
--
-- Examples:
--   countUtf8Chars (encodeUtf8 (T.pack "åäö")) ==> Just 3
--   countUtf8Chars (encodeUtf8 (T.pack "wxyz")) ==> Just 4
--   countUtf8Chars (B.pack [195]) ==> Nothing
--   countUtf8Chars (B.pack [195,184]) ==> Just 1
--   countUtf8Chars (B.drop 1 (encodeUtf8 (T.pack "åäö"))) ==> Nothing


countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars bs = case decodeUtf8' bs of
  Left _ -> Nothing
  Right t -> Just $ T.length t

------------------------------------------------------------------------------
-- Ex 8: Given a (nonempty) strict ByteString b, generate an infinite
-- lazy ByteString that consists of b, reversed b, b, reversed b, and
-- so on.
--
-- Example:
--   BL.unpack (BL.take 20 (pingpong (B.pack [0,1,2])))
--     ==> [0,1,2,2,1,0,0,1,2,2,1,0,0,1,2,2,1,0,0,1]

pingpong :: B.ByteString -> BL.ByteString
pingpong b = BL.cycle $ BL.fromChunks [b, B.reverse b]

