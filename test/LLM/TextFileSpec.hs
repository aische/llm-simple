module LLM.TextFileSpec (spec) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import LLM.Tools.Internal.TextFile (CappedRead (..), readBoundedBytesWith, readBoundedBytesWithSize, readCapped, readCappedWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hFileSize, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  readCappedSpec
  readBoundedBytesSpec

readCappedSpec :: Spec
readCappedSpec = describe "TextFile.readCapped" $ do
  it "reads a file of exactly the cap" $
    withFile (BS.replicate cap 97) $ \path ->
      withBinaryFile path ReadMode (readCapped (fromIntegral cap))
        `shouldReturn` CappedBytes (fromIntegral cap) (BS.replicate cap 97)

  it "refuses a file over the cap at the size check" $
    withFile (BS.replicate (cap + 1) 97) $ \path ->
      withBinaryFile path ReadMode (readCapped (fromIntegral cap))
        `shouldReturn` CappedTooLarge (fromIntegral cap + 1)

  -- C23: a file within the cap at the size check that grows past it
  -- before the read must be an error, not a page that ends at the cap.
  it "reports a file that grows past the cap after the size check" $
    withFile (BS.replicate (cap + 2) 97) $ \path ->
      withBinaryFile path ReadMode (readCappedWith (sizeBeforeGrowth 2) (fromIntegral cap))
        `shouldReturn` CappedGrew

  it "reads a file that grows but stays within the cap" $
    withFile (BS.replicate cap 97) $ \path ->
      withBinaryFile path ReadMode (readCappedWith (sizeBeforeGrowth 2) (fromIntegral cap))
        `shouldReturn` CappedBytes (fromIntegral cap - 2) (BS.replicate cap 97)
  where
    cap = 1000

readBoundedBytesSpec :: Spec
readBoundedBytesSpec = describe "TextFile.readBoundedBytesWith" $ do
  it "reads a file of exactly the cap" $
    withFile (BS.replicate cap 97) $ \path ->
      readBoundedBytesWith (withBinaryFile path ReadMode) name cap
        `shouldReturn` Right (BS.replicate cap 97)

  it "refuses a file over the cap at the size check" $
    withFile (BS.replicate (cap + 1) 97) $ \path ->
      readBoundedBytesWith (withBinaryFile path ReadMode) name cap
        `shouldReturn` Left "f.txt is 1001 bytes, which exceeds the 1000-byte cap."

  it "reports a file that grows past the cap after the size check" $
    withFile (BS.replicate (cap + 2) 97) $ \path ->
      readBoundedBytesWithSize (sizeBeforeGrowth 2) (withBinaryFile path ReadMode) name cap
        `shouldReturn` Left "f.txt grew past the 1000-byte cap during the read."

  it "reads a file that grows but stays within the cap" $
    withFile (BS.replicate cap 97) $ \path ->
      readBoundedBytesWithSize (sizeBeforeGrowth 2) (withBinaryFile path ReadMode) name cap
        `shouldReturn` Right (BS.replicate cap 97)

  it "names the size check when it fails" $
    withFile "text" $ \path ->
      readBoundedBytesWithSize (\_ -> throwIO (userError "no size")) (withBinaryFile path ReadMode) name cap
        `shouldReturn` Left "could not stat f.txt: user error (no size)"
  where
    cap = 1000
    name :: Text
    name = "f.txt"

-- | A size check that sees the file as it was @n@ bytes earlier: the file
-- grew by @n@ bytes between the check and the read. (Appending for real
-- from the check does not work: GHC's file locking refuses a write handle
-- while the read handle is open.)
sizeBeforeGrowth :: Integer -> Handle -> IO Integer
sizeBeforeGrowth n h = subtract n <$> hFileSize h

withFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withFile content act =
  withSystemTempDirectory "llm-simple-textfile" $ \dir -> do
    let path = dir </> "f.txt"
    BS.writeFile path content
    act path
