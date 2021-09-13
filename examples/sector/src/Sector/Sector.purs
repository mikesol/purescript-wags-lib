module App.Sector where

-- |   _____ ______ _____ _______ ____  _____  
-- |  / ____|  ____/ ____|__   __/ __ \|  __ \ 
-- | | (___ | |__ | |       | | | |  | | |__) |
-- |  \___ \|  __|| |       | | | |  | |  _  / 
-- |  ____) | |___| |____   | | | |__| | | \ \ 
-- | |_____/|______\_____|  |_|  \____/|_|  \_\

-- | This file contains a user-(developer-) facing API for the sector app.
-- | It tries to expose just enough functionality to be interesting without being
-- | overwhelming.  Each part is commented.

-- | Part 1. Obligatory imports (long... sorry...)

import Prelude

import Data.Typelevel.Num as N
import Data.NonEmpty ((:|))
import Data.List (List(..), (:))
import App.Lib (RateInfo, Order, ix, usingNSectors)

-- | Part 2. API

-- | The url of the sample to use.
-- | While the sample can be anything, the best results tend to be a file from 1-8 seconds. The default is a samba rhythm from freesound.org.
sample :: String
sample = "https://freesound.org/data/previews/320/320873_527080-hq.mp3"

-- | The number of sectors as a _type_. The number must be positive.
-- | Examples are D3, D5, D8 and D96.

type NumberOfSectors = N.D8

-- | A function to set the playback rate of a sector.
-- | Takes the following argument:
-- | ```purescript
-- | type RateInfo =
-- |   { starting :: Boolean -- is the sector starting?
-- |   , clockTime :: Number -- how much time has passed
-- |   , sectorStartClockTime :: Number -- when did the sector start in clock time
-- |   , bufferTime :: Number -- how much of the buffer has played before now
-- |   , sectorStartBufferTime :: Number -- when did the sector start in buffer time
-- |   , lastRate :: Maybe Number -- the previous rate if there was one
-- |   , duration :: Number -- the duration of the whole buffer. divide this by nsectors to get the duration of a sector.
-- |   , sector :: Int -- the current sector
-- |   }
-- | ```
-- |
-- | Returns a buffer playback rate, which is a floating point number. Negative rates go backwards in time. 1.0 is the default used below, which means a constant playback rate at the native speed of the buffer.

rate :: RateInfo -> Number
rate = const 1.0

-- | An order or sectors to use when not using randomly generated markov chains.
-- | To turn off Markov generation, set `useMarkov` to false below.
-- | Note that the indices _must be_ less than NumberOfSectors, otherwise there
-- | will be a compile-time error.
-- |
-- | In the example below, the sectors played are [0,2,1,5,4,5,3,5,6,5,7] in a loop.
order :: forall a. Order NumberOfSectors a
order = usingNSectors
  ( ix N.d0
      :| ix N.d2
        : ix N.d1
        : ix N.d5
        : ix N.d4
        : ix N.d5
        : ix N.d3
        : ix N.d5
        : ix N.d6
        : ix N.d5
        : ix N.d7
        : Nil
  )

-- | Whether or not to use a markov model via tfjs (experimental) or to use
-- | the order defined above.
useMarkov :: Boolean
useMarkov = true

-- | When changing sectors, do we use clock time or sector time?
-- | For example, if a sample lasts four seconds, contains eight sectors,
-- | and plays back at a rate of half-speed (0.5x), `useClockTime` instructs
-- | sector to make each section 0.5 seconds long (4 / 8 = 0.5).
-- | Setting this to false uses
-- | the time scale of the playback rate, which means that each section would be
-- | 1 second long (4 / (8 / 0.5) = 1.0).
-- | The actual Sector iPad app has `useClockTime` set to `true`.
useClockTime :: Boolean
useClockTime = true

