module Screeps.Types where

import Data.Maybe

import Data.Distributive (class Distributive)
import Data.Either (Either)
import Data.Lazy (Lazy)
import Data.Newtype
import Foreign (Foreign)
import Screeps.Spawn (setRawMemory)
import Unsafe.Coerce (unsafeCoerce)

type RoomPosition = { roomName :: String, x :: Number, y :: Number }

type ObjectEffect = { effect :: Number, level :: Maybe Number, ticksRemaining :: Number }

type RoomObject = ( effects :: Array ObjectEffect, pos :: RoomPosition, room :: Lazy Room, handle :: Foreign )

type RoomVisual = { asdf :: Int }

newtype LazyStructureController = LazyStructureController (Maybe StructureController)

type Room = { controller :: LazyStructureController
            , energyAvailable :: Number
            , energyCapacityAvailable :: Number
            , name :: String
            , storage :: Maybe StructureStorage
            , terminal :: Maybe StructureTermainal
            , visual :: RoomVisual 
            }

type Source = { energy :: Number
              , energyCapacity :: Number
              , id :: String
              , ticksToRegeneration :: Number 
              | RoomObject }

data BodyType = BodyWork
              | BodyMove
              | BodyCarry
              | BodyAttack
              | BodyRangedAttack
              | BodyHeal
              | BodyTough
              | BodyClaim

data DepositType = Mist
                 | Biomass
                 | Metal
                 | Silicon

data Color = Red

newtype ConstructionSite = ConstructionSite { id :: String, my :: Boolean, owner :: Owner, progress :: Number, progressTotal :: Number, structureType :: String | RoomObject }
newtype Creep = Creep { body :: Array BodyType, fatigue :: Number, hits :: Number, hitsMax :: Number, id :: String, my :: Boolean, name :: String, owner :: Owner, saying :: String, spawning :: Boolean, store :: Store, ticksToLive :: Number | RoomObject }

newtype Deposit = Deposit { cooldown :: Number, depositType :: DepositType, id :: String, lastCooldown :: Number, ticksToDecay :: Number | RoomObject }

newtype Flag = Flag { color :: Color, name :: String, secondaryColor :: Color | RoomObject }

type Structure = ( hits :: Number
                 , hitsMax :: Number
                 , id :: String
                 , structureType :: String -- Change to enum?
                 | RoomObject 
                 )

type Owner = { username :: String }

type OwnedStructure = ( my :: Boolean
                      , owner :: Owner
                      | Structure
                      )

type Store = { asdf :: Int }
type Date = { asdf :: Int }

type StructureContainer = { store :: Store
                          , ticksToDecay :: Number
                          | Structure
                          }

type StructureControllerReservation = { username :: String, ticksToEnd :: Number }

type Sign = { username :: String, text :: String, time :: Number, datetime :: Date }

type StructureController = { isPowerEnabled :: Boolean
                           , level :: Number
                           , progress :: Number
                           , progressTotal :: Number
                           , reservation :: Maybe StructureControllerReservation
                           , safeMode :: Maybe Number
                           , safeModeAvailable :: Number 
                           , safeModeCooldown :: Number
                           , sign :: Maybe Sign
                           , ticksToDowngrade :: Number
                           , upgradeBlocked :: Number
                           | OwnedStructure
                           }
 
newtype StructureExtension = StructureExtension { store :: Store | OwnedStructure }
newtype StructureExtractor = StructureExtractor { cooldown :: Number | OwnedStructure }
newtype StructureFactory = StructureFactory { cooldown :: Number, level :: Number, store :: Store | OwnedStructure }
newtype StructureInvaderCore = StructureInvaderCore { level :: Number, ticksToDeploy :: Number, spawning :: Maybe (Lazy StructureSpawnSpawning) | OwnedStructure }
newtype StructureKeeperLair = StructureKeeperLair { ticksToSpawn :: Number | OwnedStructure }
newtype StructureLab = StructureLab { cooldown :: Number, mineralType :: BaseMineralType, store :: Store | OwnedStructure }
newtype StructureLink = StructureLink { cooldown :: Number, store :: Store | OwnedStructure }
newtype StructureNuker = StructureNuker { cooldown :: Number, store :: Store | OwnedStructure }
newtype StructureObserver = StructureObserver { | OwnedStructure }
newtype StructurePowerBank = StructurePowerBank { power :: Number, ticksToDecay :: Number | Structure }
newtype StructurePowerSpawn = StructurePowerSpawn { store :: Store | OwnedStructure }
newtype StructurePortal = StructurePortal { destination :: Either RoomPosition { shard :: String, room :: String}, ticksToDecay :: Number | Structure }
newtype StructureRampart = StructureRampart { isPublic :: Boolean, ticksToDecay :: Number | OwnedStructure }
newtype StructureRoad = StructureRoad { ticksToDecay :: Number | Structure }
newtype StructureSpawn = StructureSpawn { name :: String, spawning :: Maybe (Lazy StructureSpawnSpawning), store :: Store | OwnedStructure }
newtype StructureSpawnSpawning = StructureSpawnSpawning { directions :: Array Direction, name :: String, needTime :: Number, remainingTime :: Number, spawn :: Lazy StructureSpawn }
newtype StructureStorage = StructureStorage { store :: Store | OwnedStructure }
newtype StructureTermainal = StructureTermainal { cooldown :: Store, store :: Store | OwnedStructure }
newtype StructureTower = StructureTower { store :: Store | OwnedStructure }
newtype StructureWall = StructureWall { | Structure }

derive instance newtypeStructureWall :: Newtype StructureWall _

type PowerCreep = {}

newtype Mineral = Mineral { density :: Number, mineralAmount :: Number, minearlType :: BaseMineralType, id :: String, ticksToRegeneration :: Number | RoomObject }



type Tombstone = { creep :: Either Creep PowerCreep, deathTime :: Number, id :: String, store :: Store, ticksToDecay :: Number | RoomObject }

data BaseMineralType = Hydrogen
                     | Oxygen
                     | Utrium
                     | Keanium
                     | Lemergium
                     | Zynthium
                     | Catalyst

data Direction = Top
               | TopRight
               | Right
               | BottomRight
               | Bottom
               | BottomLeft
               | Left
               | TopLeft

