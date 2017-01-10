#! /usr/bin/env stack
-- stack --resolver lts-7.14 --nix --nix-shell-file shell.nix runghc --package turtle --package ansi-wl-pprint
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, RecordWildCards #-}

import Control.Concurrent.Async (cancel)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_)
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Turtle

data Options = Options
  { reinitDb :: Bool
  } deriving (Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> Opts.switch (Opts.long "reinit-db" <> Opts.help "Reinitialize the database by deleting any existing one.")

devEnvCtx, postgresCtx, testCtx :: PP.Doc
devEnvCtx   = PP.red   "dev-env"
postgresCtx = PP.green "postgres"
testCtx     = PP.blue  "nixtest"


main :: IO ()
main = do
  Options {..} <- options "Development environment" parseOptions

  outMutex <- newQSem 1
  let logCtx :: MonadIO io => PP.Doc -> Text -> io ()
      logCtx label message =
        liftIO . bracket_ (waitQSem outMutex) (signalQSem outMutex) $
          PP.putDoc
            $  PP.fill 10 (PP.bold label)
            <> PP.hang 10 (PP.vsep . map (PP.text . T.unpack) . T.lines $ message)
            <> PP.linebreak

  devdbPresent <- testdir "devdb"
  devdbInitialized <- if devdbPresent && not reinitDb then pure True else do
    when devdbPresent $ do
      logCtx devEnvCtx "Removing devdb..."
      rmtree "devdb"

    logCtx devEnvCtx "Initializing devdb..."
    sh $
      logCtx (PP.cyan "initdb") =<< either id id <$> inprocWithErr "initdb" ["devdb"] empty

    pure True

  -- migration here

  sh $ do
    postgresRunning <- liftIO $ newQSem 0
    postgres <- fork $ do
      logCtx devEnvCtx "Starting postgres..."
      sh $
        inprocWithErr "postgres" ["-D", "devdb"] empty >>= \ case
          Right line -> logCtx postgresCtx line
          Left line -> do
            when ("database system is ready to accept connections" `T.isInfixOf` line) $
              liftIO $ signalQSem postgresRunning
            logCtx postgresCtx line

    liftIO $ waitQSem postgresRunning
    logCtx devEnvCtx "Postgres is ready."

    when devdbInitialized $ do
      logCtx devEnvCtx "Creating database and user..."
      let initScript
            =   "create database nixtest;"
            <|> "create user nixtest;"
            <|> "grant all privileges on database nixtest to nixtest;"
      sh $ logCtx (PP.cyan "psql") =<< either id id <$> inprocWithErr "psql" ["postgres"] initScript

    sh $ logCtx testCtx =<< either id id <$> inprocWithErr "stack" ["exec", "nixtest"] empty

    liftIO $ cancel postgres


