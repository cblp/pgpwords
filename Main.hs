{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative (optional, (<**>))
import           Control.Monad (unless)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word (Word8)
import           Language.Haskell.TH (listE, runIO, stringE, tupE)
import           Language.Haskell.TH.Syntax (qAddDependentFile)
import           Numeric.Natural (Natural)
import           Options.Applicative (execParser, flag, fullDesc, helper, info,
                                      long, metavar, progDesc, short,
                                      strArgument, switch)
import           Text.Read (readMaybe)

wordlist :: [(Text, Text)]
wordlist = $(do
    let file = "words.txt"
    qAddDependentFile file
    raw <- runIO $ readFile file
    let wordPairs = lines raw
    unless (length wordPairs == 256) $ fail "bad words file length"
    listE $ tupE . map stringE . words <$> wordPairs
    )

data Direction = Encode | Decode

data Options = Options
    { direction :: Direction
    , numeric   :: Bool
    , minput    :: Maybe ByteString
    }

main :: IO ()
main = do
    Options{..} <- execParser $
        info (parser <**> helper) $ fullDesc <> progDesc theProgDesc
    input <- case minput of
        Just input -> pure input
        Nothing    -> BSL.getContents
    if numeric then
        case readMaybe $ BSL.unpack input of
            Just n  -> Text.putStrLn $ natToWords n
            Nothing -> fail "input is not a natural number"
    else
        fail "binary coding is not implemented yet"
  where
    parser = Options
        <$> flag Encode Decode (short 'd' <> long "decode")
        <*> switch (short 'n' <> long "numeric")
        <*> optional (strArgument $ metavar "TEXT")
    theProgDesc =
        "A tool to encode/decode data and numbers using the PGP word list"

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
