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

import AutoScalingGroup.App (runApp)
import AutoScalingGroup.Env (mkEnv)

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
            runApp env
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
