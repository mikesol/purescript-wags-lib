module WAGS.Lib.FofT where

import Prelude
import Data.Newtype (class Newtype)

newtype FofTimeRate a
  = FofTimeRate ({ time :: Number, rate :: Number } -> a)

derive instance newtypeFofTimeRate :: Newtype (FofTimeRate a) _

derive instance functorFofTimeRate :: Functor FofTimeRate

newtype FofTimeHeadroomRate a
  = FofTimeHeadroomRate ({ time :: Number, headroom :: Number, rate :: Number } -> a)

derive instance newtypeFofTimeHeadroomRate :: Newtype (FofTimeHeadroomRate a) _

derive instance functorFofTimeHeadroomRate :: Functor FofTimeHeadroomRate

newtype FofBoolean a
  = FofBoolean (Boolean -> a)

derive instance newtypeFofBoolean :: Newtype (FofBoolean a) _

derive instance functorFofBoolean :: Functor FofBoolean

newtype FofTimeHeadroomOffsets rest a
  = FofTimeHeadroomOffsets ({ time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } } -> a)

derive instance newtypeFofTimeHeadroomOffsets :: Newtype (FofTimeHeadroomOffsets rest a) _

derive instance functorFofTimeHeadroomOffsets :: Functor (FofTimeHeadroomOffsets rest)
