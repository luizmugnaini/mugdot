# Dotfiles 🖇️

These are my humble configuration files I use in my machine running Arch Linux! I'm currently using my own home-brewed dotfiles
manager, [`stoic-dotfiles`](https://github.com/luizmugnaini/stoic).

There are two main branches: `windows` and `linux`, which I switch to whenever I'm in one of those systems.

## Usage

Install [`stoic-dotfiles`](https://crates.io/crates/stoic-dotfiles) via `cargo`:

```bash
cargo install stoic-dotfiles
```

In order to set all symlinks correctly you simply need to run `stoic-dotfiles` in the repository root directory:

```bash
cd mugdot
stoic
```
