-- | Clock appearances using IANA zones, including daylight-saving transitions.
module StreamDeck.Clock
    ( ClockZone, loadClockZone, loadLocalClockZone, formatClock, clockImage, labeledClockImage
    ) where

import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import qualified Data.Time.Zones as TZ
import StreamDeck.Font (Font, IconStyle, renderText, renderLabeledText)
import StreamDeck.Image (KeyImage)

newtype ClockZone = ClockZone TZ.TZ

-- | Load once from the timezone database bundled by the tz dependency.
loadClockZone :: String -> IO ClockZone
loadClockZone name = ClockZone <$> TZ.loadTZFromDB name

-- | Load the local zone using TZ or /etc/localtime on Linux.
loadLocalClockZone :: IO ClockZone
loadLocalClockZone = ClockZone <$> TZ.loadLocalTZ

formatClock :: ClockZone -> String -> UTCTime -> String
formatClock (ClockZone zone) patternText = formatTime defaultTimeLocale patternText . TZ.utcToLocalTimeTZ zone

-- | Suitable as the renderer in refreshEvery. The format uses formatTime syntax.
clockImage :: ClockZone -> Font -> IconStyle -> String -> IO KeyImage
clockImage zone font style patternText = do
    now <- getCurrentTime
    either (ioError . userError) pure (renderText font style (formatClock zone patternText now))

-- | A city/title above a formatted time or date, suitable for refreshEvery.
labeledClockImage :: ClockZone -> Font -> IconStyle -> String -> String -> IO KeyImage
labeledClockImage zone font style title patternText = do
    now <- getCurrentTime
    either (ioError . userError) pure $
        renderLabeledText font style title (formatClock zone patternText now)
