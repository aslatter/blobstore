
module Main(main) where

import System.IO.Temp (withSystemTempDirectory)
import Database.BlobStorage

import Test.HUnit

import qualified Data.ByteString.Lazy.Char8 as L

import Control.Monad (when)
import System.Exit (exitFailure)

-- | Run an action in a fresh blob-storage, then tear it
-- down.
withTempStore :: (BlobStorage -> IO a) -> IO a
withTempStore k = withSystemTempDirectory "testStorage" $ \path ->
                  open path >>= k

addAndCheck :: L.ByteString -> BlobStorage -> IO ()
addAndCheck bytes store
    = do
  ident <- add store bytes
  bytes' <- fetch store ident
  assert (bytes == bytes')

insertRetrieveEmpty :: IO ()
insertRetrieveEmpty =
    withTempStore $ 
    addAndCheck (L.pack "The quick brown fox jumped over the lazy dog.")

insertRetrieveNonEmpty :: IO ()
insertRetrieveNonEmpty =
    withTempStore $ \store -> do
      addAndCheck (L.pack "The quick brown fox jumped over the lazy dog.") store
      addAndCheck (L.pack "Haskell fun with HUnit!") store

main :: IO ()
main = do
  -- run tests
  count <- runTestTT $
           test [ insertRetrieveEmpty
                , insertRetrieveNonEmpty
                ]
  -- print results
  putStrLn (showCounts count)

  -- exit with failure if anything went poorly
  when (errors count /= 0)
       exitFailure
  when (failures count /= 0)
       exitFailure
