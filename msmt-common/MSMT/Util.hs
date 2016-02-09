module MSMT.Util
  -- re-export stuff which is required often
  ( liftIO
  , lift
  , when
  , forM
  , forM_
  , fromMaybe
  , maybe
  , split
  , either ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Either
import           Data.Maybe


split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'
