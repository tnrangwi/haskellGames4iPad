module LocalSettings

  (
    screenSize
  , screenWidth
  , screenDetail
  )

where

data DeviceType = Console | Ipad2 | Iphone4

device :: DeviceType

screenSize :: Int
screenSize = case device of
                 Console -> screenSizeConsole
                 Ipad2 -> screenSizeiPad2Landscape
                 Iphone4 -> screenSizeiPhone4Portrait

screenSizeConsole :: Int
screenSizeConsole = 25

screenSizeiPad2Landscape :: Int
screenSizeiPad2Landscape = 18

screenSizeiPhone4Portrait :: Int
screenSizeiPhone4Portrait = 9

screenWidth :: Int
screenWidth = case device of
                  Console -> screenWidthConsole
                  Ipad2 -> screenWidthiPad2Landscape
                  Iphone4 -> screenWidthiPhone4Portrait

screenWidthConsole :: Int
screenWidthConsole = 80

screenWidthiPad2Landscape :: Int
screenWidthiPad2Landscape = 58

screenWidthiPhone4Portrait :: Int
screenWidthiPhone4Portrait = 20

screenDetail :: Bool
screenDetail = case device of
                   Console -> True
                   Ipad2 -> True
                   Iphone4 -> False


-- Configuration goes here
device = Ipad2
