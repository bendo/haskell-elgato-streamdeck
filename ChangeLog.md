# Changelog for haskell-elgato-streamdeck

## Unreleased changes

- Add push, pull-request, and manual GitHub Actions checks on Ubuntu (GHC 9.10.3
  and 9.12.2) and an upgraded Arch Linux container (GHC 9.12.2), with native X11
  tests, example checks, preview artifacts, and distribution-specific caches.
- Batch changed page keys under one native I/O lock and pause input polling
  throughout a frame transfer, eliminating per-packet input-poll stalls.
- Cache key-specific USB packets and force complete frame preparation before
  presentation; retain changed-key deduplication and safe error cleanup.
- Give all visible dynamic renderers one shared 50 ms initial-frame budget, using
  cached/blank fallbacks for slow jobs and retaining results arriving mid-transfer.
- Add a live page-transfer benchmark and regression tests for batch scheduling,
  complete preparation, frame callbacks, deadlines, caching, and late results.
- Bundle DejaVu Sans and add `loadTextFont`, two-line `renderLabeledText`, and
  `labeledClockImage` with independently fitted title/value and stable clock digits.
- Update the cycling clock to Munich, London, and Sydney, with each city above
  an `HH:MM` time using IANA timezone rules.
- Test two-line fitting, punctuation, clock formatting, leading zeroes, and stable
  numeric layout; document the bundled text font and labeled clock API.
- Replace the external `xdotool` keyboard action with a native X11/XTest backend.
- Resolve keysyms in the active keyboard layout, serialize complete chords, and
  restore held keys, modifier locks/latches, and keyboard groups after injection.
- Validate key mappings before generating input, provide explicit display/XTest
  errors, and test native keyboard events in an opt-in private Xvfb server.
- Add declarative pages and typed page-switch, command, shell, key-chord, custom IO,
  state-selection, and sequence actions.
- Remember named button states per page for the running session; navigation has
  fully user-defined button images, positions, and destinations.
- Add background appearance refresh, render caching/deduplication, cancellation,
  stale-input/result handling, and blanking of unused page keys.
- Add IANA-zone clock images and a complete pages/cycling-clock example.
- Test page validation, state persistence, effect responsiveness, transitions,
  renderer lifecycle, key chords, and daylight-saving behavior without hardware.
- Add styled TrueType glyph and single-line text rendering with automatic fitting,
  centering, antialiasing, colors, padding, and missing-glyph errors.
- Bundle Font Awesome Free 6.7.2 solid, regular, and brand fonts with their SIL OFL
  license; add named audio, Spotify, media, and weather icon helpers.
- Add `examples/FontAwesome.hs` and upright PNG previews for any key image.
- Test font families, glyph fitting, colors, labels, input validation, and previews.
- Add `examples/Radio.hs`, loading `~/radio.jpeg` and printing `radioactive` on press.
- Treat signal-interrupted native reads as empty polls so Ctrl-C can exit cleanly.
- Add MK2 press/release events and prepared key-image output with serialized writes.
- Add image loading, resizing, orientation correction, JPEG encoding, and solid colors.
- Add `StreamDeck.runDeck` and a single-file example with queued press actions.
- Add CLI `watch` and `image ROW COLUMN FILE` diagnostics.
- Correct the firmware feature report ID from serial report `0x06` to `0x05`.
- Replace the placeholder suite with hardware-independent protocol, image, and
  action-runtime tests.
- Avoid the `hidapi` binding's error-string double-free with a narrow native I/O
  wrapper, serialized device calls, and a Linux cleanup regression test.
- Update to Stack LTS 24.65 (GHC 9.10.3) and enable compiler warnings.
- Fix serial-number handling and CLI output.
- Scope HIDAPI initialization around discovery and device use; add exception-safe
  device cleanup with `withDevice`.
- Export the device types and raw firmware report operation; remove the
  unimplemented reset stub.
- Document Linux dependencies and build/run commands.
