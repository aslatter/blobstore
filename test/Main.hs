
module Main(main) where

import System.IO.Temp (withSystemTempDirectory)
import Database.BlobStorage

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

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

insertRetrieveEmpty :: Assertion
insertRetrieveEmpty =
    withTempStore $ 
    addAndCheck (L.pack "The quick brown fox jumped over the lazy dog.")

insertRetrieveNonEmpty :: Assertion
insertRetrieveNonEmpty =
    withTempStore $ \store -> do
      addAndCheck (L.pack "The quick brown fox jumped over the lazy dog.") store
      addAndCheck (L.pack "Haskell fun with HUnit!") store

main :: IO ()
main = do
  -- run tests
  defaultMain $
       [ testCase "Insert/retrieve empty" insertRetrieveEmpty
       , testCase "Insert/retrieve non-empty" insertRetrieveNonEmpty
       ]
