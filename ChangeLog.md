# Changelog for haskell-elgato-streamdeck

## Unreleased changes

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
