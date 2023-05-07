{-# LANGUAGE OverloadedStrings #-}

module AutoScalingGroup.Ping where

import Data.Text (Text)
import Data.Text as T
import Data.Word (Word8)
import Shelly (errExit, lastExitCode, run, shelly, silently)

ping :: Text -> Word8 -> Word8 -> IO Bool
ping host resTimeout resCount = shelly $ silently $ do
    let cmd = run "ping" ["-W", T.pack $ show resTimeout, "-c", T.pack $ show resCount, host]
    errExit False cmd
    code <- lastExitCode
    return $ code == 0
