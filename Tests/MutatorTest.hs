{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Test.HUnit
import Data.Mutators


data Point = Point {
    ptX :: Integer
  , ptY :: Integer
  } deriving (Eq, Show)

data Rectangle = Rectangle {
    rcTopLeft :: Point  
  , rcBotRight :: Point
  } deriving (Eq, Show)


$(genMutators ''Point)
$(genMutators ''Rectangle)


samplePoint = Point { ptX = 0
                    , ptY = 0
                    } 

sampleRectangle = Rectangle { rcTopLeft = Point 0 20
                            , rcBotRight = Point 30 0
                            }

setterTest = TestCase $ assertEqual "x is set" x x'
  where x  = ptX $ Point { ptX = 10, ptY = 0 }
        x' = ptX $ samplePoint `setPtX` 10

modTest = TestCase $ assertEqual "y is modified" y y'
  where y  = ptY $ Point { ptX = 0, ptY = 1 }
        y' = ptY $ modPtY samplePoint succ

complexTest = TestCase $ assertEqual "Nested field is updated" r r'
  where r  = Rectangle { rcTopLeft = Point 0 20
                       , rcBotRight = Point 30 10
                       }
        r' = modRcBotRight sampleRectangle $ \p -> setPtY p 10

tests = TestList [ TestLabel "Setter test" setterTest
                 , TestLabel "Modifier test" modTest
                 , TestLabel "Complex update field" complexTest
                 ]

main = runTestTT tests

