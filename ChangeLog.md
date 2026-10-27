# Changelog for haskell-elgato-streamdeck

## Unreleased changes

- Update to Stack LTS 24.65 (GHC 9.10.3) and enable compiler warnings.
- Fix serial-number handling and CLI output.
- Scope HIDAPI initialization around discovery and device use; add exception-safe
  device cleanup with `withDevice`.
- Export the device types and raw firmware report operation; remove the
  unimplemented reset stub.
- Document Linux dependencies and build/run commands.
