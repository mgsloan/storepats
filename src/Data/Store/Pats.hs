{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Store.Pats where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, sizeOf, peek, poke)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Internal as BS

type role Stored nominal
newtype Stored a = Stored (Ptr Word8)

type role Tagged nominal
data Tagged a = Tagged
  { taggedTag :: {-# UNPACK #-} !Word8
  , taggedPtr :: {-# UNPACK #-} !(Ptr Word8)
  }

readTag :: Stored a -> Tagged a
readTag (Stored ptr) = Tagged
  { taggedTag = unsafePeek ptr
  , taggedPtr = ptr `plusPtr` sizeOf (undefined :: Word8)
  }

readStorable :: Storable a => Stored a -> a
readStorable (Stored ptr) = unsafePeek ptr

pattern StoredNothing :: Tagged (Maybe a)
pattern StoredNothing <- (Tagged 0 _)

pattern StoredJust :: Stored a -> Tagged (Maybe a)
pattern StoredJust a <- (Tagged 1 (Stored -> a))

{-# COMPLETE StoredNothing, StoredJust #-}

testMaybeInt :: Maybe Int -> IO ()
testMaybeInt mx = do
  let BS.PS fptr _ _ = serializeMaybeInt mx
  withForeignPtr fptr $ \ptr -> do
    let decoded = readMaybeInt (Stored ptr)
    when (decoded /= mx) $ error "Mismatch?!"

readMaybeInt :: Stored (Maybe Int) -> Maybe Int
readMaybeInt s =
  case readTag s of
    StoredNothing -> Nothing
    StoredJust (readStorable -> x) -> Just x

serializeMaybeInt :: Maybe Int -> ByteString
serializeMaybeInt = \case
  Nothing -> "\0"
  Just x -> BS.unsafeCreate 5 $ \ptr -> do
    poke ptr 1
    poke (coerce (ptr `plusPtr` sizeOf (undefined :: Word8))) x

unsafePeek :: Storable a => Ptr Word8 -> a
unsafePeek = unsafePerformIO . peek . coerce
