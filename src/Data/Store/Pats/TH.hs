{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Store.Pats.TH where

import TH.Utilities
import TH.ReifySimple
import Data.Store.Pats.Internal
import Language.Haskell.TH
import Data.Word

-- Format notes:
--
-- Word16 tag.  Top of range is reserved for special tags (utf8,
-- utf16, ascii, bytes, etc)
--
-- Word16 number-of-var-size fields.  Motivation of this is to make it
-- possible to decode tree structure without knowing other info.
--
-- (var_size_count - 1) fields have Int32 offsets stored.  Sizes can
-- be inferred by subtraction.
--
-- Size of fixed-layout portion of data can be inferred by the first
-- offset.

makePats :: Name -> Q [Dec]
makePats name = do
  dt <- reifyDataType name
  case dtCons dt of
    [] -> return []
    [dc] -> makeConPat dt dc Nothing
    cons -> concat <$> sequence (zipWith (makeConPat dt) cons tags)
      where
        tags = map Just universeBounded ++ [tooManyCons]
        tooManyCons = error "can't use makePats on datatypes > 256 constructors."

makeConPat :: DataType -> DataCon -> Maybe Word8 -> Q [Dec]
makeConPat dt dc mtag =
  sequence
    [ patSynSigD name $ fmap functionType $ sequence $
        map (\ty -> [t| Stored $(pure ty) |]) $
        map snd (dcFields dc) ++ [dataTypeToType dt]
    , patSynD name (prefixPatSyn fieldVars) unidir $
        case mtag of
          Just tag ->
            [p| (readTag -> TagAndBytes $(tagPat) $(matchContents)) |]
            where
              tagPat = litP (IntegerL (fromIntegral tag))
          Nothing ->
            matchContents
    ]
  where
    name = dequalify $ dcName dc
    fieldVars = zipWith (\_ n -> n) (dcFields dc) varNames
    matchContents = undefined

--------------------------------------------------------------------------------
-- Utilities

universeBounded :: (Enum a, Bounded a) => [a]
universeBounded = [minBound .. maxBound]

functionType :: [Type] -> Type
functionType = foldr1 (\l r -> appsT ArrowT [l, r])

dataTypeToType :: DataType -> Type
dataTypeToType dt = appsT (ConT (dtName dt)) (map VarT (dtTvs dt))

varNames :: [Name]
varNames = map mkName names
  where
    names =
      concat $
      zipWith (\chars suffix -> map (\c -> [c] ++ suffix) chars)
              (repeat ['a'..'z'])
              ("" : map show ([1..] :: [Integer]))
