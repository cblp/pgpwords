{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative (optional, (<**>))
import           Control.Monad (unless)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Data.Word (Word8)
import           Language.Haskell.TH (listE, runIO, stringE, tupE)
import           Language.Haskell.TH.Syntax (qAddDependentFile)
import           Numeric.Natural (Natural)
import           Options.Applicative (execParser, flag, fullDesc, helper, info,
                                      long, metavar, progDesc, short,
                                      strArgument, switch)
import           Text.Read (readMaybe)

wordlist :: [(ByteString, ByteString)]
wordlist = $(do
    let file = "words.txt"
    qAddDependentFile file
    raw <- runIO $ readFile file
    let wordPairs = lines raw
    unless (length wordPairs == 256) $ fail "bad words file length"
    listE $ tupE . map stringE . words <$> wordPairs
    )

rwordlist :: Map ByteString (Bool, Word8)
rwordlist = Map.fromList $ do
    (b, (evenWord, oddWord)) <- zip [0..] wordlist
    [(evenWord, (False, b)), (oddWord, (True, b))]

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
        case direction of
            Encode -> case readMaybe $ BSL.unpack input of
                Just n  -> BSL.putStrLn $ natToWords n
                Nothing -> fail "input is not a natural number"
            Decode -> print $ wordsToNat input
    else
        fail "binary coding is not implemented yet"
  where
    parser = Options
        <$> flag Encode Decode (short 'd' <> long "decode")
        <*> switch (short 'n' <> long "numeric")
        <*> optional (strArgument $ metavar "TEXT")
    theProgDesc =
        "A tool to encode/decode data and numbers using the PGP word list"

natToWords :: Natural -> ByteString
natToWords = BSL.unwords . alternate . map byteToWord . natToBytes
  where
    alternate = zipWith ($) $ cycle [fst, snd]

wordsToNat :: ByteString -> Natural
wordsToNat = bytesToNat . map wordToByte . BSL.words

byteToWord :: Word8 -> (ByteString, ByteString)
byteToWord b = wordlist !! fromIntegral b

wordToByte :: ByteString -> Word8
wordToByte w =
    snd . fromMaybe (error $ "unknown word " ++ show w) $ Map.lookup w rwordlist

natToBytes :: Natural -> [Word8]
natToBytes n = fromIntegral m : case d of
    0 -> []
    _ -> natToBytes d
  where
    (d, m) = n `divMod` 256

bytesToNat :: [Word8] -> Natural
bytesToNat = \case
    []     -> 0
    b : bs -> fromIntegral b + 256 * bytesToNat bs
