{-# LANGUAGE RoleAnnotations #-}

module Data.Store.Pats.Internal where

import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable, sizeOf, peek)
import System.IO.Unsafe (unsafePerformIO)

type role Stored nominal
newtype Stored a = Stored { storedPtr :: Ptr Word8 }

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
readStorable = unsafePeek . storedPtr

unsafePeek :: Storable a => Ptr Word8 -> a
unsafePeek = unsafePerformIO . peek . coerce
