module HollowKnightSaveED where

import           Crypto.Cipher.AES      (AES, decryptECB, encryptECB, initAES)
import           Crypto.Data.PKCS7      (padBytesN, unpadBytesN)
import qualified Data.ByteString        as B
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.ByteString.Char8  (pack)
import           Data.Char              (digitToInt, intToDigit)
import           Data.List.Split        (chunksOf)
import           Numeric                (showIntAtBase)

hkAES :: AES
hkAES = initAES (pack "UKu52ePUBwetZ9wNX88o54dnfKRu0T1l")

header :: B.ByteString
header = pack "\NUL\SOH\NUL\NUL\NUL\255\255\255\255\SOH\NUL\
               \\NUL\NUL\NUL\NUL\NUL\NUL\ACK\SOH\NUL\NUL\NUL"

decryptSave :: B.ByteString -> Maybe B.ByteString
decryptSave  = unpadBytesN 16 . decryptECB hkAES
             . decodeLenient . B.drop 22 . B.init

encryptSave :: B.ByteString -> B.ByteString
encryptSave = finalizeSave . encode . encryptECB hkAES . padBytesN 16

finalizeSave :: B.ByteString -> B.ByteString
finalizeSave save = header +-+ fl +-+ filtered +-+ B.singleton 11
  where
    filtered = B.filter ((&&) <$> (0x0A /=) <*> (0x0D /=)) save
    fl = lengthInBytes filtered
    (+-+) = B.append

intToBinaryStr :: Int -> String
intToBinaryStr = flip (showIntAtBase 2 intToDigit) []

revBinaryToInt :: String -> Int
revBinaryToInt = foldr (\x y -> digitToInt x + (y * 2)) 0

lengthInBytes :: B.ByteString -> B.ByteString
lengthInBytes b = B.pack $ (fromIntegral . revBinaryToInt . add1) <$> revBinary
  where
    revBinary = (chunksOf 7 . reverse . intToBinaryStr) (B.length b)
    add1 x = if length x == 7 then x ++ "1" else x
