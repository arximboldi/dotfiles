# .nixmac

This directory is managed by [nixmac](https://github.com/darkmatter/nixmac).

`settings.json` holds user preferences that should follow you across machines
— things like agent iteration limits, default model, and confirmation
behavior. The file is plain JSON; nixmac reads it on the next agent run.

Per-device settings (developer mode, pinned version, update channel, model
cache) intentionally live elsewhere in your OS app data directory and are
**not** synced here.

If you'd rather not commit these settings, add `.nixmac/` to your
`.gitignore`. Removing the file is harmless — nixmac will recreate it with
defaults on next launch.
