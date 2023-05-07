{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Yaml (decodeFileEither)
import Options.Applicative (
    Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
 )

import AutoScalingGroup.AWS (runInstance)
import AutoScalingGroup.Env (mkEnv, runASGAction, Env(pingEnv), PingOpts(..))
import AutoScalingGroup.Ping (ping)

newtype CmdOpts = CmdOpts
    { configPath :: String
    }
    deriving (Show)

main :: IO ()
main = do
    cmdOpts <- execParser parserInfo
    eitherOpts <- decodeFileEither (configPath cmdOpts)
    case eitherOpts of
        Right opts -> do
            env <- mkEnv opts
            -- instanceInfo <- runASGAction env runInstance
            -- print instanceInfo
            isAlive <- ping "100.26.163.50" (responseTimeoutSecs $ pingEnv env) (responseCount $ pingEnv env)
            print isAlive
        Left e -> error $ show e

parserInfo :: ParserInfo CmdOpts
parserInfo =
    info
        (helper <*> optionParser)
        (fullDesc <> header "autoscaling-service" <> progDesc "Service for autoscaling groups")

optionParser :: Parser CmdOpts
optionParser =
    CmdOpts
        <$> strOption (long "config-path" <> short 'c' <> metavar "CONFIGPATH" <> help "Absolute path to config-file")
