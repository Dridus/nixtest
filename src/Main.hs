module Main where

import ClassyPrelude
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (withPostgresqlConn)
import Database.Persist.Sql (runMigration)
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistLowerCase)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  foo Text
  deriving Show
|]

main :: IO ()
main =
  runStdoutLoggingT $
    withPostgresqlConn "host=localhost port=5432 user=nixtest password=nixtest dbname=nixtest" $
      runReaderT $
        runMigration migrateAll
