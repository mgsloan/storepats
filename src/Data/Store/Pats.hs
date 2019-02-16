{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Store.Pats where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (sizeOf, poke)
import qualified Data.ByteString.Internal as BS
import Data.Store.Pats.Internal

pattern StoredNothing :: Stored (Maybe a)
pattern StoredNothing <- (readTag -> Tagged 0 _)

pattern StoredJust :: Stored a -> Stored (Maybe a)
pattern StoredJust a <- (readTag -> Tagged 1 (Stored -> a))

{-# COMPLETE StoredNothing, StoredJust #-}

testMaybeInt :: Maybe Int -> IO ()
testMaybeInt mx = do
  let BS.PS fptr _ _ = serializeMaybeInt mx
  withForeignPtr fptr $ \ptr -> do
    let decoded = readMaybeInt (Stored ptr)
    when (decoded /= mx) $ error "Mismatch?!"

readMaybeInt :: Stored (Maybe Int) -> Maybe Int
readMaybeInt = \case
  StoredNothing -> Nothing
  StoredJust (readStorable -> x) -> Just x

serializeMaybeInt :: Maybe Int -> ByteString
serializeMaybeInt = \case
  Nothing -> "\0"
  Just x -> BS.unsafeCreate 5 $ \ptr -> do
    poke ptr 1
    poke (coerce (ptr `plusPtr` sizeOf (undefined :: Word8))) x
