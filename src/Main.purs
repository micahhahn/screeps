module Main where

import Data.Array
import Data.Array
import Data.Maybe
import Prelude
import Screeps.Spawn

import Data.Either
import Data.Newtype
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException, throw)

import Data.Argonaut

newtype Memory = Memory { testValue :: String }

derive instance newtypeMemory :: Newtype Memory _

derive newtype instance encodeMemory :: EncodeJson Memory
derive newtype instance decodeMemory :: DecodeJson Memory

gameInit :: Effect Memory
gameInit = do
  spawns <- getSpawns
  case head spawns of
    Just s -> do 
      result <- spawnCreep s [BodyWork, BodyCarry, BodyMove] "TestCreep"
      case result of 
        Left _ -> pure unit
        Right e -> throw ("Unexpected error code: " <> show e)
    _ -> throw "Expected exactly one spawn at game origin."
  pure (Memory { testValue: "asdf" })

gameLoop :: Effect Unit
gameLoop = do
  mem <- getRawMemory
  m <- case parseJson mem of 
        Left _ -> throw "Failed to parse memory"
        Right json -> case (decodeJson json :: Either JsonDecodeError Memory) of
          Left _ -> gameInit
          Right m -> pure m

  setRawMemory <<< stringify <<< encodeJson $ m

loop :: Effect Unit
loop = catchException logError gameLoop
  where logError e = do
          let message = "Encountered unhandled exception: " <> (show e)
          log message
          gameNotify message 0.0