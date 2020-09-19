module Test.Main where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Picture (Picture, Point(..), Shape(..), area, getString, origin, scaleByTwoAndCenterToOrigin)
import Effect (Effect)
import Math (pi)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

circle ∷ Shape
circle = Circle p r
  where
    p = Point { x: 1.0, y: 1.0 }
    r = 10.0

rectangle ∷ Shape
rectangle = Rectangle p w h
  where
    p = Point { x: 1.0, y: 1.0 }
    w = 5.0
    h = 3.0

line ∷ Shape
line = Line p1 p2
  where
    p1 = Point { x: 1.0, y: -4.0 }
    p2 = Point { x: -25.0, y: 23.0 }

text ∷ Shape
text = Text p s
  where
    p = Point { x: 0.1, y: 0.1 }
    s = "Hello World"

picture :: Picture
picture = [circle, rectangle]

main ∷ Effect Unit
main = run [consoleReporter] do
  describe "Shape" do
    describe "scaleByTwoAndCenterToOrigin" do
      it "can process circle" do
        let input = show $ scaleByTwoAndCenterToOrigin circle
        let output = show $ Circle origin 20.0
        input `shouldEqual` output

      it "can process rectangle" do
        let input = show $ scaleByTwoAndCenterToOrigin rectangle
        let output = show $ Rectangle origin 10.0 6.0
        input `shouldEqual` output

      it "can process line" do
        let input = show $ scaleByTwoAndCenterToOrigin line
        let p1 = Point { x: 26.0, y: -27.0 }
        let p2 = Point { x: -26.0, y: 27.0 }
        let output = show $ Line p1 p2
        input `shouldEqual` output

      it "can process text" do
        let input = show $ scaleByTwoAndCenterToOrigin text
        let output = show $ Text origin "Hello World"
        input `shouldEqual` output

    describe "getString" do
      it "should get string if input is Text" do
        let string = fromMaybe "" $ getString text
        string `shouldEqual` "Hello World"

      it "should get nothing if input is not Text" do
        let string = fromMaybe "" $ getString circle
        string `shouldEqual` ""

  describe "Picture" do
    describe "area" do
      it "compute area of circle" do
        let input = area circle
        input `shouldEqual` (10.0 * 10.0 * pi)

      it "compute area of rectangle" do
        let input = area rectangle
        input `shouldEqual` 15.0

      it "otherwise should be zero" do
        let input = [line, text]
        let allTrue = foldl (\acc shape -> acc && (area shape) == 0.0) true
        (allTrue input) `shouldEqual` true
