module Import.SCC where

import           Control.Monad
import           Control.Monad.Trans.Either
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as S8
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Servant.Common.BaseUrl
import           Servant.Common.Req

import           MSMT.Configuration
import           MSMT.Util
import           MSMT.Util.ErrorT

--import           Import.SCC.Api
import           Types


data ImportError = ImportFailed ByteString



syncSCC :: Runtime -> IO ()
syncSCC (pool, conf, opts) = do
  validateConfiguration conf
    [ "import" `has` "host"
    , "import" `has` "port"
    , "import" `has` "login"
    , "import" `has` "credentials"]








  where
    transport = if cfgDefault "scc" "use-https" True conf
                  then Https
                  else Http
