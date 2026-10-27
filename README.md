# haskell-elgato-streamdeck

A Haskell library for small, single-file **Stream Deck MK2** applications.
Configure key images and ordinary Haskell `IO` actions. The library handles device
discovery, image conversion, button events, and cleanup.

## Build and run

The project uses **Stack LTS 24.65 / GHC 9.10.3**. Stack selects the project compiler
independently of your default GHC.

On Linux, the `hidapi` Haskell package builds its bundled HIDAPI C implementation
and requires a C compiler and libudev development headers. For Debian/Ubuntu:

```sh
sudo apt install build-essential libudev-dev
```

Build and run the discovery example:

```sh
stack build
stack run haskell-elgato-streamdeck-exe
```

The example lists the number of MK2 devices, prints the first device's serial
number, opens it, and closes it. It does not change the display or brightness.

Your user needs read/write access to the device's `/dev/hidraw*` node. If the
example reports an opening error, check the device's udev rules and session ACLs.

You can also build with Cabal and a compatible GHC (9.10 or newer):

```sh
cabal build all
cabal run haskell-elgato-streamdeck-exe
```

## Library usage

Here is a complete application. It puts a blue button in the top-left corner and
prints a message when pressed:

```haskell
import StreamDeck

main :: IO ()
main = do
    hello <- either fail pure (keyAt 0 0)
    runDeck
        [ Button hello (solidColor 30 120 220)
            (putStrLn "Hello from your Stream Deck!")
        ]
```

The included `examples/MyDeck.hs` configures two buttons (blue and orange):

```sh
stack build
stack exec -- runghc -package=haskell-elgato-streamdeck examples/MyDeck.hs
```

Press Ctrl-C to exit. You can compile the same source with `ghc -threaded` instead
of `runghc` when ready to deploy it. With Cabal, run the example using:

```sh
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/MyDeck.hs
```

`examples/Radio.hs` loads `~/radio.jpeg` onto the top-left key and prints
`radioactive` whenever you press it:

```sh
cabal build all
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/Radio.hs
```

JPEG files work directly; no PNG conversion is necessary.

## Font Awesome icons and text

Font Awesome Free **6.7.2** is bundled with the library. No system font installation
or runtime download is needed. It has three complementary fonts:

| Font | Examples |
| --- | --- |
| Solid | Volume, media controls, sun, cloud, rain, wind, temperature |
| Regular | Outline variants such as sun, snowflake, heart |
| Brands | Spotify and other brand logos |

Run a single-file example with ten icons on the first two rows:

```sh
cabal build all
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/FontAwesome.hs
```

The buttons print their names when pressed. You can replace those actions with
your own functions for Spotify, volume control, or weather integrations.

```haskell
import StreamDeck
import qualified StreamDeck.FontAwesome as FA

main :: IO ()
main = do
    fonts <- FA.loadFontAwesome
    first <- either fail pure (keyAt 0 0)
    image <- either fail pure $ FA.renderAwesomeIcon fonts
        defaultIconStyle { foreground = RGB 30 215 96, padding = 10 }
        FA.spotify
    runDeck [Button first image (putStrLn "Spotify")]
```

Named helpers include `spotify`, `volumeHigh`, `volumeLow`, `volumeXmark`, `sun`,
`cloud`, `cloudSun`, `cloudRain`, `cloudShowersHeavy`, `snowflake`, `wind`, `bolt`,
`temperatureHalf`, `radiation`, `play`, `pause`, `forwardStep`, `backwardStep`,
`headphones`, `terminal`, and `heart`. These are shortcuts, not a restriction on
the available glyphs. Any other Free icon can be selected by style and Unicode
from the [Font Awesome 6 catalogue](https://fontawesome.com/v6/search?ic=free):

```haskell
-- Regular/outline sun instead of the solid variant:
let outlineSun = FA.AwesomeIcon FA.Regular '\xf185'
```

`IconStyle` controls `foreground`, `background`, and `padding` (0..35 pixels).
The renderer preserves aspect ratio, centers the glyph outlines, antialiases
edges, and produces a ready-to-use `KeyImage`. Missing glyphs are reported rather
than silently drawing a replacement box. Font Awesome Pro glyphs are not included.

For ordinary text or another icon font, use `loadFont` with an explicit `.ttf`
path, then `renderText font style "Mute"` or `renderGlyph font style character`.
Text is a single line fitted to the key. Complex-script shaping, multiline layout,
and automatic font fallback are not implemented. Font Awesome itself is an icon
font; use a regular text font for arbitrary labels.

You can inspect icons without a connected deck by saving upright PNG previews:

```sh
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/FontAwesome.hs --preview /tmp/deck
```

This writes `/tmp/deck-spotify.png`, `/tmp/deck-volume.png`, and the other icons.
The parent directory of the output prefix must exist. In your own code,
`saveKeyImagePreview "preview.png" image` saves the exact encoded image in its
upright orientation.

Font files retain their **SIL OFL 1.1** license and attribution in
`fonts/fontawesome/LICENSE.txt`; see that directory's README for their source.

- Positions are **zero-based**: rows `0..2`, columns `0..4`, left-to-right and
  top-to-bottom. `keyAt` validates the position.
- `loadKeyImage "icons/terminal.png"` loads PNG/JPEG and other JuicyPixels formats.
  Images are stretched to 72×72 using nearest-neighbour sampling, rotated for the
  device, and encoded as JPEG. Supply an upright image, ideally already square.
- `runDeck` opens the first matching MK2. Duplicate bindings are rejected before
  touching the device. Only configured keys are updated.
- Actions run in press order on a separate worker; slow actions do not block
  input reading. Further presses are queued. Synchronous action failures are
  printed to stderr and later actions continue.
- On exit, the worker is cancelled and the device is closed. Images remain on the
  device until another application changes them; no reset is sent.

## Device diagnostics

Watch press/release transitions (key indexes `0..14`):

```sh
stack run haskell-elgato-streamdeck-exe -- watch
```

Set one key image and exit:

```sh
stack run haskell-elgato-streamdeck-exe -- image 0 0 icons/terminal.png
```

The lower-level API is in `DeviceManager`:

```haskell
import Control.Monad (forever)
import DeviceManager

main :: IO ()
main = withHIDAPI $ do
    devices <- enumerate
    case devices of
        [] -> putStrLn "No Stream Deck MK2 found."
        device : _ -> withDevice device $ \deck ->
            forever $ readKeyEvents deck 100 >>= mapM_ print
```

Keep discovery and all device operations inside `withHIDAPI`. Use `withDevice`
to close the device on both normal return and exceptions; the device handle must
not escape its callback. `firmwareVersion` currently returns the raw report ID
and bytes, not a decoded version string.

## Development

Edit `package.yaml`; the `.cabal` file is generated by Hpack when Stack builds.
Commit both files when changing package settings. `stack.yaml.lock` records the
snapshot's content hash.

```sh
stack test
```

The tests run without hardware and cover key positions and transitions, malformed
reports, JPEG sizing/orientation, image packet boundaries, duplicate bindings,
action ordering, error recovery, worker cancellation, bundled font glyphs, font
colors/centering/padding, missing glyphs, labels, and PNG previews. Use the
diagnostics and examples above for live hardware checks.

Native I/O uses a small local wrapper around the C implementation bundled by
`hidapi`. This avoids the binding's incorrect freeing of HIDAPI-owned error
strings, which can crash cleanup after an interrupted read. Native operations on
each device are serialized to protect HIDAPI's shared error storage. A Linux
regression test exercises error handling and closing without a physical deck.

The MK2 packet format is cross-checked against the
[python-elgato-streamdeck Original V2 implementation](https://github.com/abcminiuser/python-elgato-streamdeck/blob/master/src/StreamDeck/Devices/StreamDeckOriginalV2.py),
which also handles the MK2. Other deck models, automatic reconnection, and pages
are not implemented yet.
