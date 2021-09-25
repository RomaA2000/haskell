{-# LANGUAGE InstanceSigs #-}
{-
Module : Block1_1
Data type for DayOfWeek
-}
module Block1_1
  ( -- * The 'DayOfWeek' type
    DayOfWeek(..)
    -- * Functions for 'DayOfWeek'
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)


instance Enum DayOfWeek where
  -- | Function 'toEnum' converts 'Int' to 'DayOfWeek'
  toEnum :: Int -> DayOfWeek
  toEnum num = case mod num 7 of
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    _ -> Sunday

  -- | Function 'fromEnum' converts 'DayOfWeek' to 'Int'
  fromEnum :: DayOfWeek -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

instance Eq DayOfWeek where
  -- | Function '==' checks equality of 'DayOfWeek's
  (==) :: DayOfWeek -> DayOfWeek -> Bool
  d1 == d2 = fromEnum d1 == fromEnum d2

-- | Function 'nextDay' returns next 'DayOfWeek'
nextDay :: DayOfWeek -> DayOfWeek
nextDay day = succ day

-- | Function 'afterDays' returns 'DayOfWeek' after given number of days after a given one
afterDays :: DayOfWeek -> Int -> DayOfWeek
afterDays day num = toEnum $ (num + fromEnum day)

-- | Function 'isWeekend' returns if 'DayOfWeek' is weekend
isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Function 'daysToParty' returns number of days to nearest friday
daysToParty :: DayOfWeek -> Int
daysToParty day = mod (4 - fromEnum day) 7
