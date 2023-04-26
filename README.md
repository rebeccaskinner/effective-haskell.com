# Building

You can build and view this site on macOS, Linux, or Windows using WSL with
[nix](https://nixos.org/download.html). If you're using an older version of nix,
you may also need to [enable flakes](https://nixos.wiki/wiki/Flakes).

Once you've installed nix, you can run a local server by running:

```
nix run .#buildAndWatch
```

You can also build the static version of the site by running

```
nix build
```

The static site should be available in the `result` directory.

Finally, if you want to make changes to the builder itself, you can get into a
development shell by running

```
nix develop
```
