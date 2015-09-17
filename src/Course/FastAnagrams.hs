{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- >>> fastAnagrams "Julian" "/usr/share/dict/words" 

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
--  -> IO()
fastAnagrams name f =
--  error "todo: Course.FastAnagrams#fastAnagrams"
--	putStrLn ("file: " ++ f)
  (flip (filter . flip S.member) (permutations name) . S.fromList . hlist . lines) <$> readFile f
--  putStrLn readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
