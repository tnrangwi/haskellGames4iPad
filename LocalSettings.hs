module LocalSettings

  (
    screenSize
  , screenWidth
  , screenDetail
  )

where

data DeviceType = Console | Ipad2 | Iphone4 | Iphone6Std

device :: DeviceType

screenSize :: Int
screenSize = case device of
                 Console -> screenSizeConsole
                 Ipad2 -> screenSizeiPad2Landscape
                 Iphone4 -> screenSizeiPhone4Portrait
                 Iphone6Std -> screenSizeiPhone6StdPortrait

screenSizeConsole :: Int
screenSizeConsole = 25

screenSizeiPad2Landscape :: Int
screenSizeiPad2Landscape = 18

screenSizeiPhone4Portrait :: Int
screenSizeiPhone4Portrait = 9

screenSizeiPhone6StdPortrait :: Int
screenSizeiPhone6StdPortrait = 13

screenWidth :: Int
screenWidth = case device of
                  Console -> screenWidthConsole
                  Ipad2 -> screenWidthiPad2Landscape
                  Iphone4 -> screenWidthiPhone4Portrait
                  Iphone6Std -> screenWidthiPhone6StdPortrait

screenWidthConsole :: Int
screenWidthConsole = 80

screenWidthiPad2Landscape :: Int
screenWidthiPad2Landscape = 58

screenWidthiPhone4Portrait :: Int
screenWidthiPhone4Portrait = 20

screenWidthiPhone6StdPortrait :: Int
screenWidthiPhone6StdPortrait = 35

screenDetail :: Bool
screenDetail = case device of
                Console -> True
                Ipad2 -> True
                Iphone4 -> False
                Iphone6Std -> True


-- Configuration goes here
device = Iphone6Std
