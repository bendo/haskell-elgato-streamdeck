-- | Font Awesome Free 6.7.2, bundled so examples need no system font install.
module StreamDeck.FontAwesome
    ( FontAwesome, loadFontAwesome, solidFont, regularFont, brandsFont
    , AwesomeStyle (..), AwesomeIcon (..), renderAwesomeIcon
    , spotify, volumeHigh, volumeLow, volumeXmark
    , sun, cloud, cloudSun, cloudRain, cloudShowersHeavy, snowflake, wind, bolt, temperatureHalf
    , radiation, play, pause, forwardStep, backwardStep, headphones, terminal, heart
    ) where

import Paths_haskell_elgato_streamdeck (getDataFileName)
import StreamDeck.Font (Font, IconStyle, loadFont, renderGlyph)
import StreamDeck.Image (KeyImage)

data FontAwesome = FontAwesome
    { solidFont :: Font
    , regularFont :: Font
    , brandsFont :: Font
    }

data AwesomeStyle = Solid | Regular | Brands deriving (Eq, Show)

-- | Use any glyph from the bundled Free fonts, beyond the named helpers below.
data AwesomeIcon = AwesomeIcon AwesomeStyle Char deriving (Eq, Show)

loadFontAwesome :: IO FontAwesome
loadFontAwesome = FontAwesome
    <$> bundled "fa-solid-900.ttf"
    <*> bundled "fa-regular-400.ttf"
    <*> bundled "fa-brands-400.ttf"
  where
    bundled name = getDataFileName ("fonts/fontawesome/" ++ name) >>= loadFont

renderAwesomeIcon :: FontAwesome -> IconStyle -> AwesomeIcon -> Either String KeyImage
renderAwesomeIcon fonts style (AwesomeIcon family character) =
    renderGlyph (select family) style character
  where
    select Solid = solidFont fonts
    select Regular = regularFont fonts
    select Brands = brandsFont fonts

spotify, volumeHigh, volumeLow, volumeXmark :: AwesomeIcon
spotify = AwesomeIcon Brands '\xf1bc'
volumeHigh = AwesomeIcon Solid '\xf028'
volumeLow = AwesomeIcon Solid '\xf027'
volumeXmark = AwesomeIcon Solid '\xf6a9'

sun, cloud, cloudSun, cloudRain, cloudShowersHeavy, snowflake, wind, bolt, temperatureHalf :: AwesomeIcon
sun = AwesomeIcon Solid '\xf185'
cloud = AwesomeIcon Solid '\xf0c2'
cloudSun = AwesomeIcon Solid '\xf6c4'
cloudRain = AwesomeIcon Solid '\xf73d'
cloudShowersHeavy = AwesomeIcon Solid '\xf740'
snowflake = AwesomeIcon Solid '\xf2dc'
wind = AwesomeIcon Solid '\xf72e'
bolt = AwesomeIcon Solid '\xf0e7'
temperatureHalf = AwesomeIcon Solid '\xf2c9'

radiation, play, pause, forwardStep, backwardStep, headphones, terminal, heart :: AwesomeIcon
radiation = AwesomeIcon Solid '\xf7b9'
play = AwesomeIcon Solid '\xf04b'
pause = AwesomeIcon Solid '\xf04c'
forwardStep = AwesomeIcon Solid '\xf051'
backwardStep = AwesomeIcon Solid '\xf048'
headphones = AwesomeIcon Solid '\xf025'
terminal = AwesomeIcon Solid '\xf120'
heart = AwesomeIcon Solid '\xf004'
