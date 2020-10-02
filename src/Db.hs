-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Db where

import Database.Selda
import Database.Selda.PostgreSQL

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)

instance SqlType Pet

data Person = Person
  { name :: Text,
    age :: Int,
    pet :: Maybe Pet
  }
  deriving (Generic)

instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

connectInfo = PGConnectInfo {pgHost = "localhost", pgPort = 5432, pgDatabase = "", pgSchema = Just "", pgUsername = Just "", pgPassword = Just ""}

insertTest :: IO ()
insertTest =
  let connectInfo =
        PGConnectInfo
          { pgHost = "localhost",
            pgPort = 5432,
            pgDatabase = "",
            pgSchema = Just "",
            pgUsername = Just "",
            pgPassword = Just ""
          }
   in withPostgreSQL connectInfo $ do
        createTable people
        insert_
          people
          [ Person "Velvet" 19 (Just Dog),
            Person "Kobayashi" 23 (Just Dragon),
            Person "Miyu" 10 Nothing
          ]

        adultsAndTheirPets <- query $ do
          person <- select people
          restrict (person ! #age .>= 18)
          return (person ! #name :*: person ! #pet)
        liftIO $ print adultsAndTheirPets

-- import Data.ByteString (ByteString)
-- import Data.Default.Class (def)
-- import Data.Int (Int32)
-- import Data.Tuple.Homotuple.Only ()
-- import Data.Tuple.List.Only ()
-- import Data.Tuple.Only (Only (Only))
-- import Database.PostgreSQL.Pure

-- findPerson = do
--   conn <- connect def
--   let preparedStatementProcedure = parse "" "SELECT id, name FROM person WHERE id = $1" Nothing
--   portalProcedure <- bind @_ @2 @_ @_ "" BinaryFormat BinaryFormat (parameters conn) (const $ fail "") (Only (1 :: Int32)) preparedStatementProcedure
--   let executedProcedure = execute @_ @_ @(Int32, ByteString) 0 (const $ fail "") portalProcedure
--   ((_, _, e, _), _) <- sync conn executedProcedure
--   let a = records e
--   return ()
