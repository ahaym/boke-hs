{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.BokeHS.Prim where

import Data.Aeson
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C
import Data.Scientific
import Data.Text
import GHC.Generics

--encodes a BokehJS Ref ID
newtype BID = BID Text deriving (Eq, Show, Generic)
instance ToJSON BID

--encodes a BokehJS Type Declaration
newtype BType = BType Text deriving (Eq, Show, Generic)
instance ToJSON BType

data Direction = BLeft | BRight | BAbove | BBelow | BCenter deriving (Eq, Show)

newtype Field = Field Text deriving (Show, Generic)
instance ToJSON Field

--Degrees
newtype Angle = Angle Scientific deriving (Eq, Show, Generic, Num)

type BNum = Scientific

type Color = C.Colour Double
c2j :: Color -> Value
c2j = toJSON . C.sRGB24show

data DashPattern = Solid | Dashed | Dotted | DotDash | DashDot deriving (Eq, Enum, Show)

instance ToJSON DashPattern where
    toJSON Solid = toJSON ([] :: [Int])
    toJSON Dashed = toJSON ([6] :: [Int])
    toJSON Dotted = toJSON ([2, 4] :: [Int])
    toJSON DotDash = toJSON ([2, 4, 6, 4] :: [Int])
    toJSON DashDot = toJSON ([6, 4, 2, 4] :: [Int])

data LineJoin = Miter | Round | Bevel deriving (Eq, Enum, Show)
instance ToJSON LineJoin where
    toJSON Miter = "miter"
    toJSON Round = "round"
    toJSON Bevel = "bevel"

data LineCap = Butt | Rounded | Square deriving (Eq, Enum, Show)
instance ToJSON LineCap where
    toJSON Butt = "butt"
    toJSON Rounded = "round"
    toJSON Square = "square"

