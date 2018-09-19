{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams' ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams' s fp =
    (((<$>) filter)
    (flip S.member <$>  ((S.fromList . hlist . lines) <$> readFile fp)))
    <*>
    (pure $ permutations s)
  -- error "todo: Course.FastAnagrams#fastAnagrams"
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams  s fp =
  (flip (filter . flip S.member))
  (permutations s) <$>
  ((S.fromList . hlist . lines) <$> readFile fp)


newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
