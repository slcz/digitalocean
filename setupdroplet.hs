------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly hiding (find)
import qualified Data.Text as T
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import Data.Monoid
import System.Environment (getArgs)
default (T.Text)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 2) $ ioError (userError
        ("setupvm <ip_address> <user>"))
    address <- return $ T.pack $ head args
    user <- return $ T.pack $ head $ tail args
    home <- return $ "/home/" `T.append` user `T.append` "/"
    shelly $ silently $ do
        sshPairs_ ("root@" `T.append` address) [("useradd", ["-m", user]),
             ("mkdir", ["-p",  home `T.append` ".ssh"])]
        return ("%" `T.append` user `T.append` " ALL=(ALL) NOPASSWD: ALL\n")
            -|- sshPairs_ ("root@" `T.append` address) [("cat",
                [">>", "/etc/sudoers"])]
        run_ "scp" [home `T.append` ".ssh/id_rsa.pub", "root@"
             `T.append` address `T.append` ":" `T.append`
             home `T.append` ".ssh/authorized_keys"]
        sshPairs_ ("root@" `T.append` address)
            [("chown", ["-R", user `T.append` ":" `T.append` user,
              home `T.append` ".ssh"])]
        return ()
