{-# LANGUAGE RoleAnnotations #-}

module Data.Store.Pats.Internal where

import Data.Coerce (coerce)
import Data.Word (Word8)
import Foreign.Storable (Storable, peek)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

type role Stored nominal
newtype Stored a = Stored { storedBytes :: ByteString }

type role TagAndBytes nominal
data TagAndBytes a = TagAndBytes
  { tabTag :: {-# UNPACK #-} !Word8
  , tabBytes :: {-# UNPACK #-} !ByteString
  }

readTag :: Stored a -> TagAndBytes a
readTag (Stored bs) = TagAndBytes
  { tabTag = BS.index bs 0
  , tabBytes = BS.drop 1 bs
  }

readStorable :: Storable a => Stored a -> a
readStorable (Stored bs) = unsafePerformIO $ BS.unsafeUseAsCString bs (peek . coerce)
