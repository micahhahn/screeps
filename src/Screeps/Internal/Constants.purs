module Screeps.Internal.Constants where

-- See https://docs.screeps.com/api/#Constants

foreign import kOK :: Int
foreign import kERR_NOT_OWNER :: Int
foreign import kERR_NO_PATH :: Int
foreign import kERR_NAME_EXISTS :: Int
foreign import kERR_BUSY :: Int
foreign import kERR_NOT_FOUND :: Int
foreign import kERR_NOT_ENOUGH_ENERGY :: Int
foreign import kERR_NOT_ENOUGH_RESOURCES :: Int
foreign import kERR_INVALID_TARGET :: Int
foreign import kERR_FULL :: Int
foreign import kERR_NOT_IN_RANGE :: Int
foreign import kERR_INVALID_ARGS :: Int
foreign import kERR_TIRED :: Int
foreign import kERR_NO_BODYPART :: Int
foreign import kERR_NOT_ENOUGH_EXTENSIONS :: Int
foreign import kERR_RCL_NOT_ENOUGH :: Int
foreign import kERR_GCL_NOT_ENOUGH :: Int

foreign import kFIND_EXIT_TOP :: Int
foreign import kFIND_EXIT_RIGHT :: Int
foreign import kFIND_EXIT_BOTTOM :: Int
foreign import kFIND_EXIT_LEFT :: Int
foreign import kFIND_EXIT :: Int
foreign import kFIND_CREEPS :: Int
foreign import kFIND_MY_CREEPS :: Int
foreign import kFIND_HOSTILE_CREEPS :: Int
foreign import kFIND_SOURCES_ACTIVE :: Int
foreign import kFIND_SOURCES :: Int
foreign import kFIND_DROPPED_RESOURCES :: Int
foreign import kFIND_STRUCTURES :: Int
foreign import kFIND_MY_STRUCTURES :: Int
foreign import kFIND_HOSTILE_STRUCTURES :: Int
foreign import kFIND_FLAGS :: Int
foreign import kFIND_CONSTRUCTION_SITES :: Int
foreign import kFIND_MY_SPAWNS :: Int
foreign import kFIND_HOSTILE_SPAWNS :: Int
foreign import kFIND_MY_CONSTRUCTION_SITES :: Int
foreign import kFIND_HOSTILE_CONSTRUCTION_SITES :: Int
foreign import kFIND_MINERALS :: Int
foreign import kFIND_NUKES :: Int
foreign import kFIND_TOMBSTONES :: Int
foreign import kFIND_POWER_CREEPS :: Int
foreign import kFIND_MY_POWER_CREEPS :: Int
foreign import kFIND_HOSTILE_POWER_CREEPS :: Int
foreign import kFIND_DEPOSITS :: Int
foreign import kFIND_RUINS :: Int

foreign import kTOP :: Int
foreign import kTOP_RIGHT :: Int
foreign import kRIGHT :: Int
foreign import kBOTTOM_RIGHT :: Int
foreign import kBOTTOM :: Int
foreign import kBOTTOM_LEFT :: Int
foreign import kLEFT :: Int
foreign import kTOP_LEFT :: Int

foreign import kCOLOR_RED :: Int
foreign import kCOLOR_PURPLE :: Int
foreign import kCOLOR_BLUE :: Int
foreign import kCOLOR_CYAN :: Int
foreign import kCOLOR_GREEN :: Int
foreign import kCOLOR_YELLOW :: Int
foreign import kCOLOR_ORANGE :: Int
foreign import kCOLOR_BROWN :: Int
foreign import kCOLOR_GREY :: Int
foreign import kCOLOR_WHITE :: Int

foreign import kLOOK_CREEPS :: String
foreign import kLOOK_ENERGY :: String
foreign import kLOOK_RESOURCES :: String
foreign import kLOOK_SOURCES :: String
foreign import kLOOK_MINERALS :: String
foreign import kLOOK_DEPOSITS :: String
foreign import kLOOK_STRUCTURES :: String
foreign import kLOOK_FLAGS :: String
foreign import kLOOK_CONSTRUCTION_SITES :: String
foreign import kLOOK_NUKES :: String
foreign import kLOOK_TERRAIN :: String
foreign import kLOOK_TOMBSTONES :: String
foreign import kLOOK_POWER_CREEPS :: String
foreign import kLOOK_RUINS :: String

-- OBSTACLE_OBJECT_TYPES

foreign import kMOVE :: String
foreign import kWORK :: String
foreign import kCARRY :: String
foreign import kATTACK :: String
foreign import kRANGED_ATTACK :: String
foreign import kTOUGH :: String
foreign import kHEAL :: String
foreign import kCLAIM :: String

-- BODYPART_COST

-- Deprecated
-- WORLD_WIDTH
-- WORLD_HEIGHT