{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad (unless)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word (Word8)
import           Language.Haskell.TH (listE, runIO, stringE, tupE)
import           Language.Haskell.TH.Syntax (qAddDependentFile)
import           Numeric.Natural (Natural)

wordlist :: [(Text, Text)]
wordlist = $(do
    let file = "words.txt"
    qAddDependentFile file
    raw <- runIO $ readFile file
    let wordPairs = lines raw
    unless (length wordPairs == 256) $ fail "bad words file length"
    listE $ tupE . map stringE . words <$> wordPairs
    )

main :: IO ()
main = Text.putStrLn $ natToWords 257

natToWords :: Natural -> Text
natToWords = Text.unwords . alternate . map byteToWord . natToBytes
  where
    alternate = zipWith ($) $ cycle [fst, snd]

byteToWord :: Word8 -> (Text, Text)
byteToWord b = wordlist !! fromIntegral b

natToBytes :: Natural -> [Word8]
natToBytes n = fromIntegral m : case d of
    0 -> []
    _ -> natToBytes d
  where
    (d, m) = n `divMod` 256
