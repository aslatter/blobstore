{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Database.BlobStorage
-- Copyright   :  Duncan Coutts <duncan@haskell.org>
--
-- Maintainer  :  Antoine Latter <aslatter@gmail.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Persistent storage for blobs of data.
--
module Database.BlobStorage (
    -- * General usage
    BlobStorage,
    BlobId,
    open,
    add,
    fetch,
    -- * Advanced Usage
    addWith,
    addFileWith,
    filepath,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import System.FilePath ((</>), takeDirectory)
import Control.Exception (handle, throwIO, evaluate)
import System.Directory
import System.IO

-- | An id for a blob. The content of the blob is stable.
--
newtype BlobId = BlobId MD5Digest
  deriving (Eq, Ord, Binary, Typeable)

instance Show BlobId where show (BlobId digest) = show digest

-- | A persistent blob storage area. Blobs can be added and retrieved but
-- not removed or modified.
--
newtype BlobStorage = BlobStorage FilePath -- ^ location of the store

filepath :: BlobStorage -> BlobId -> FilePath
filepath (BlobStorage storeDir) (BlobId hash) =
    let hashStr = show hash
    in  storeDir </> take 2 hashStr </> drop 2 hashStr

incomingDir :: BlobStorage -> FilePath
incomingDir (BlobStorage storeDir) = storeDir </> "incoming"

-- | Add a blob into the store. The result is a 'BlobId' that can be used
-- later with 'fetch' to retrieve the blob content.
--
-- * This operation is idempotent. That is, adding the same content again
--   gives the same 'BlobId'.
--
add :: BlobStorage -> ByteString -> IO BlobId
add store content =
  withIncoming store content $ \_ blobId -> return (blobId, True)

-- | Like 'add' but we get another chance to make another pass over the input
-- 'ByteString'.
--
-- What happens is that we stream the input into a temp file in an incoming
-- area. Then we can make a second pass over it to do some validation or
-- processing. If the validator decides to reject then we rollback and the
-- blob is not entered into the store. If it accepts then the blob is added
-- and the 'BlobId' is returned.
--
addWith :: BlobStorage -> ByteString
        -> (ByteString -> IO (Either error result))
        -> IO (Either error (result, BlobId))
addWith store content check =
  withIncoming store content $ \file blobId -> do
    content' <- BS.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

addFileWith :: BlobStorage -> FilePath
        -> (ByteString -> IO (Either error result))
        -> IO (Either error (result, BlobId))
addFileWith store filePath check =
  withIncomingFile store filePath $ \file blobId -> do
    content' <- BS.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

hBlobId :: Handle -> IO BlobId
hBlobId hnd = evaluate . BlobId . md5 =<< BS.hGetContents hnd

fpBlobId :: FilePath -> IO BlobId
fpBlobId file =
    do hnd <- openBinaryFile file ReadMode 
       blobId <- hBlobId hnd
       hClose hnd
       return blobId 

withIncoming :: BlobStorage -> ByteString
              -> (FilePath -> BlobId -> IO (a, Bool))
              -> IO a
withIncoming store content action = do
    (file, hnd) <- openBinaryTempFile (incomingDir store) "new"
    handleExceptions file hnd $ do
        -- TODO: calculate the md5 and write to the temp file in one pass:
        BS.hPut hnd content
        hSeek hnd AbsoluteSeek 0
        blobId <- hBlobId hnd
        hClose hnd
        withIncoming' store file blobId action
  where
    handleExceptions tmpFile tmpHandle =
      handle $ \err -> do
        hClose tmpHandle
        removeFile tmpFile
        throwIO (err :: IOError)

withIncomingFile :: BlobStorage 
                     -> FilePath
                     -> (FilePath -> BlobId -> IO (a, Bool))
                     -> IO a
withIncomingFile store file action =
    do blobId <- fpBlobId file
       withIncoming' store file blobId action

withIncoming' :: BlobStorage -> FilePath -> BlobId -> (FilePath -> BlobId -> IO (a, Bool)) -> IO a
withIncoming' store file blobId action = do
        -- open a new Handle since the old one is closed by hGetContents
        (res, commit) <- action file blobId
        if commit
            then do
                let targetPath = filepath store blobId
                exists <- doesFileExist targetPath

                if exists then removeFile file else do
                createDirectoryIfMissing False (takeDirectory targetPath)
                renameFile file targetPath 
            else removeFile file
        return res


-- | Retrieve a blob from the store given its 'BlobId'.
--
-- * The content corresponding to a given 'BlobId' never changes.
--
-- * The blob must exist in the store or it is an error.
--
fetch :: BlobStorage -> BlobId -> IO ByteString
fetch store blobid = BS.readFile (filepath store blobid)

-- | Opens an existing or new blob storage area.
--
open :: FilePath -> IO BlobStorage
open storeDir = do
  createDirectoryIfMissing False storeDir
  let store = BlobStorage storeDir
  createDirectoryIfMissing False (incomingDir store)
  return store
