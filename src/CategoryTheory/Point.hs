module CategoryTheory.Point where

data Point = Point (Double, Double)
           deriving (Show, Read)

data Triangle = Triangle Point Point Point
              deriving (Show, Read)

data RegularPolygon = RP Int Point Point
                    deriving (Show, Read)

data Car = Car
    { company :: String
    , style :: String
    , production :: Integer
    } deriving (Show, Read)
