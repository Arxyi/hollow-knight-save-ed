import           Data.ByteString    (pack)
import           HollowKnightSaveED
import           Test.QuickCheck

propED str  = decryptSave (encryptSave (pack str)) == Just (pack str)
propBin x = ((revBinaryToInt.reverse.intToBinaryStr) (abs x)) == (abs x)

main :: IO ()
main = quickCheck propED >> quickCheck propBin
