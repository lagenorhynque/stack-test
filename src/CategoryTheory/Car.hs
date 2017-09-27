module CategoryTheory.Car where

data Car = Car
    { company :: String
    , style :: String
    , production :: Integer
    } deriving (Show, Read)
