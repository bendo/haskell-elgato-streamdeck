# haskell-elgato-streamdeck

A Haskell library for small, single-file **Stream Deck MK2** applications.
Configure key images, typed actions, pages, and stateful buttons with scheduled
appearances. The library handles discovery, image conversion, button events,
rendering, and cleanup.

## Build and run

The project uses **Stack LTS 24.65 / GHC 9.10.3**. Stack selects the project compiler
independently of your default GHC.

On Linux, the `hidapi` Haskell package builds its bundled HIDAPI C implementation.
The native keyboard backend links to X11 and XTest. Install a C compiler, libudev,
X11/XTest development headers, and pkg-config. For Debian/Ubuntu:

```sh
sudo apt install build-essential pkg-config libudev-dev libx11-dev libxtst-dev
```

On Arch Linux:

```sh
sudo pacman -S --needed base-devel pkgconf systemd libx11 libxtst
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

## Pages, actions, and remembered button states

Run the complete page example:

```sh
cabal build all
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/Pages.hs
```

On the main page, `(0,0)` opens the weather page and `(0,4)` opens a commands page.
The clock at `(0,1)` cycles through Munich (green), London (yellow), and Sydney
(blue). Each face shows the city name above a larger **HH:MM**, using bundled
DejaVu Sans, and refreshes every second. Munich uses `Europe/Berlin`; its selected
city is remembered when you leave and return. City
buttons print their names; the commands page has a Super+d shortcut and a bell
button that runs `zenity` to show a "Hello from your Stream Deck!" popup with an
OK button. This example requires `zenity` for the popup. Other command actions
wait until you close the dialog; page navigation and clock updates stay responsive.
Press Ctrl-C to exit.

### Define a page

`runPages` takes an initial page ID and your page definitions. For example, after
preparing your key positions and images:

```haskell
runPages "main"
    [ Page "main"
        [ button "weather" weatherKey weatherImage (SwitchPage "weather") ]
    , Page "weather"
        [ button "london" londonKey londonImage
            (RunIO (putStrLn "Weather city: London"))
        , button "back" backKey backImage (SwitchPage "main")
        ]
    ]
```

The back button is entirely yours: choose its position, an image or rendered font
icon/text, and its destination. No keys, icons, or navigation behavior are reserved.

`runPages` owns the full 15-key layout and draws unused keys black. It validates
page IDs, button IDs, positions, state IDs, destinations, and refresh intervals
before opening a device. Button IDs must be unique within a page; state IDs must
be unique within a button. The original `runDeck [Button ...]` API is also available
for simple single-page `IO` applications.

Page transitions prepare the full desired frame in memory before writing any
keys: JPEG encoding and USB packet preparation are complete before transfer.
Prepared packet streams are cached per image/key. Only changed keys are sent,
as one uninterrupted burst under the device I/O lock, while input polling is
paused. Input resumes after draining transition reports. The MK2 protocol still
updates individual keys; this is a fast burst, not a hardware-atomic page swap.

### Choose actions

| Action | Behavior |
| --- | --- |
| `SwitchPage "weather"` | Display a configured page |
| `Execute "xset" ["s", "off", "-dpms"]` | Execute a program with literal arguments |
| `Shell "command1 && command2"` | Execute an explicit shell command |
| `SendKeys (super 'd')` | Send a native X11/XTest key chord |
| `RunIO operation` | Run your own Haskell `IO ()` |
| `NextState` | Advance this button's state, wrapping to the first |
| `SetState "london"` | Select a named state of this button |
| `Sequence [action1, action2]` | Apply UI actions and enqueue external effects in list order |

`Execute`, `Shell`, `SendKeys`, and `RunIO` run sequentially on a separate worker.
Navigation, state changes, and display refreshes remain responsive during a slow
command. An action already accepted stays queued/running even if you change pages.
Synchronous effect failures are logged and the worker continues with later actions.

`Sequence` does **not** wait for external effects before applying its later UI
actions. For example, `Sequence [Execute "program" [], SwitchPage "main"]` queues
the program and switches pages immediately. Use one `RunIO` operation when you
need several external operations to wait for each other. `NextState` and `SetState`
always refer to the button that initiated the sequence, including after a
`SwitchPage` in that sequence.

Keyboard chords use X11 keysym names:

```haskell
SendKeys (super 'd')
SendKeys (KeyChord [Control, Alt] "Delete")
SendKeys (KeyChord [] "XF86AudioRaiseVolume")
```

`SendKeys` calls X11's XTest extension directly; it needs no `xdotool` executable.
Set `DISPLAY` for the X11 session. The backend validates all keys before sending
input, presses modifiers before the target, and releases injected keys in reverse
order. It temporarily clears held/latched/locked modifiers and restores their
state and the active keyboard group afterward, including during cancellation.
Complete chords are serialized across callers.

Key names are resolved at base/Shift levels in the active keyboard group. An
explicit shifted symbol such as `exclam` adds Shift automatically; an explicit
modifier such as `KeyChord [Super, Shift] "d"` also works. Unknown or unmapped
keys report an error before generating events. An already-held target key is
rejected so its physical key state is not altered.

This is an X11 backend, suitable for XMonad and other X11 desktops. Under Wayland,
XTest can target XWayland applications but does not provide compositor-wide
shortcuts. A native Wayland backend can be added separately.

### Give a button multiple states

Every state has its own appearance and action. This three-state button cycles
between images in declaration order:

```haskell
statefulButton "mode" modeKey "one"
    [ state "one"   (Static firstImage)  NextState
    , state "two"   (Static secondImage) NextState
    , state "three" (Static thirdImage)  NextState
    ]
```

Selections are remembered by **(page ID, button ID)** throughout the running
session. Leaving a page pauses its renderers; returning restores each button's
selected state. Restarting the application restores the configured initial states.

### Refresh a clock or other dynamic appearance

`refreshEvery seconds renderer` runs the renderer immediately on activation, then
waits the given number of seconds after each completed attempt. Only visible,
selected states run. Renderers run separately from actions and device dispatch;
they should use cancellation-friendly IO so they can stop on navigation or exit.

```haskell
textFont <- loadTextFont
munich <- loadClockZone "Europe/Berlin"
london <- loadClockZone "Europe/London"
sydney <- loadClockZone "Australia/Sydney"

let face city zone = refreshEvery 1 $
        labeledClockImage zone textFont defaultIconStyle city "%H:%M"

-- Use this as a button in a Page:
let clockButton = statefulButton "clock" clockKey "munich"
        [ state "munich" (face "Munich" munich) NextState
        , state "london" (face "London" london) NextState
        , state "sydney" (face "Sydney" sydney) NextState
        ]
```

`loadTextFont` loads the bundled DejaVu Sans text font, including lowercase names
and punctuation. `labeledClockImage` puts the title above the formatted value;
`clockImage` renders only the formatted value on one line. You can also supply a
custom font loaded with `loadFont`.
Clock formats use `formatTime` syntax, so `%Y-%m-%d` displays a date instead of a
time. Named zones use the IANA database bundled by the `tz` dependency, including
daylight-saving rules. `loadLocalClockZone` loads the local Linux zone from `TZ`
or `/etc/localtime` once at startup.

A renderer is any `IO KeyImage`, so it can also read weather data, audio state,
or another application. On a page/state transition, all visible renderers start
concurrently and share one **50 ms initial-render budget**, rather than waiting
50 ms per button. Fast results, including clocks, join the first frame. Slower
renderers use their last cached image, or a black placeholder on the first visit,
and update when ready. The renderer keeps running after the deadline; a result
arriving during the frame transfer is retained for a later update.

Identical JPEGs are not resent. After a rendering error, the last good image
remains and the renderer retries at its next interval. Dynamic caches retain the
latest image per configured state, not every historical clock/weather image.
Input from a layout transition and results from obsolete renderers are discarded;
held keys must be released before a fresh press can activate the new layout.

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

For ordinary text, use the bundled `loadTextFont`, or `loadFont` with an explicit
`.ttf` path. `renderText font style "Mute"` renders one line;
`renderLabeledText font style "Munich" "23:20"` renders a smaller title above a
larger value. The two lines are independently fitted and centered, with numeric
advances and a shared digit baseline keeping clock values stable as they update.
`renderGlyph font style character` renders an individual symbol. Complex-script
shaping, arbitrary multiline layout, and automatic font fallback are not
implemented. Font Awesome itself remains an icon font.

You can inspect icons without a connected deck by saving upright PNG previews:

```sh
cabal exec -- runghc -package=haskell-elgato-streamdeck examples/FontAwesome.hs --preview /tmp/deck
```

This writes `/tmp/deck-spotify.png`, `/tmp/deck-volume.png`, and the other icons.
The parent directory of the output prefix must exist. In your own code,
`saveKeyImagePreview "preview.png" image` saves the exact encoded image in its
upright orientation.

Font Awesome files retain their **SIL OFL 1.1** license and attribution in
`fonts/fontawesome/LICENSE.txt`. DejaVu Sans retains its font license and
attribution in `fonts/dejavu/LICENSE.txt`. See each directory's README for sources.

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
colors/centering/padding, missing glyphs, labels, PNG previews, page validation,
remembered states, redraw/blanking, transition input, background effects,
renderer lifecycle/deduplication, and clock daylight-saving rules. Page tests use
an injected device backend; no physical deck or keyboard injection is required.
Use the diagnostics and examples above for live hardware checks.

### Page-transfer benchmark

```sh
cabal run streamdeck-page-benchmark
```

This fills all 15 keys with test icons, then switches between two test pages using
scripted presses. Avoid physical key presses while it runs. It reports cold
render/encode/packet preparation, warm cached preparation, batch transfer with
and without competing input reads, and end-to-end runtime transition timing.
The runtime timings separate input/preparation/handoff from the uninterrupted
device burst and the final flush/activation. Individual transfer timings include
any wait for the native I/O lock; they are host-side timings, not measurements of
the panel's physical refresh. Restart your normal example afterward to restore
its layout.

On the development MK2, a prepared 15-key/45-report upload previously took
4.51–4.52 seconds with a competing 100 ms input poll (5.7 ms without a reader).
After batching, the runtime's 15-key page switches measured 45.9–47.1 ms end to
end, including input handling and flushing; their uninterrupted device bursts
took 5.8–6.9 ms. These are sample host-side measurements, not universal latency
guarantees. Warm cached packet preparation measured about 0.005 ms per frame.

For a custom `StreamDeck.Pages.Runtime.PageDriver`, implement `drawPageFrame`
as a batch callback. The native implementation uses `DeviceManager.setKeyImages`.
`setKeyImage` uses the same transport burst mechanism for a one-key update.

Native keyboard event tests are opt-in and use a private Xvfb server, not your
desktop. Install Xvfb and xauth to run them (`setxkbmap` enables the additional
multi-layout check):

```sh
STREAMDECK_TEST_X11=1 xvfb-run -a cabal test all --test-show-details=direct
```

They check actual event order, modifier/lock restoration, shifted symbols, invalid
and unmapped keys, concurrent chords, cancellation, and active layouts. To check
the missing-XTest error path on a separate server:

```sh
STREAMDECK_TEST_X11=no-xtest xvfb-run -a -s "-screen 0 800x600x24 -extension XTEST" cabal test all --test-show-details=direct
```

Native I/O uses a small local wrapper around the C implementation bundled by
`hidapi`. This avoids the binding's incorrect freeing of HIDAPI-owned error
strings, which can crash cleanup after an interrupted read. Native operations on
each device are serialized to protect HIDAPI's shared error storage. A Linux
regression test exercises error handling and closing without a physical deck.

The MK2 packet format is cross-checked against the
[python-elgato-streamdeck Original V2 implementation](https://github.com/abcminiuser/python-elgato-streamdeck/blob/master/src/StreamDeck/Devices/StreamDeckOriginalV2.py),
which also handles the MK2. Other deck models and automatic reconnection are not
implemented yet.
