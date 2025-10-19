# Regulator Bot

A Telegram bot written in Haskell for managing FLOSS-UZ (Free/Libre and Open Source Software Uzbekistan) communities.

## About

This is FLOSS community Telegram bot that serves as a community management tool for FLOSS-UZ Telegram groups, providing automated moderation, information resources, and member assistance. Built with Haskell and Cabal, it uses Nix for reproducible builds and deployment.

### Key Capabilities
- Automated community moderation
- Resource sharing for community members
- Cross-community coordination
- Newcomer onboarding

## Features

### Automated Triggers
- **Welcome Messages**: Automatically greets new members joining the community
- **Channel User Protection**: Automatically removes messages from channel users to prevent spam

### Commands

- `/rules`
Redirects users to the community standards and guidelines at [std.floss.uz](https://std.floss.uz)

- `/group` -
Displays a list of FLOSS-UZ rust-related communities available on Telegram

- `/roadmap` -
Provides a learning roadmap for newcomers to the community

- `/useful` -
Shares curated useful resources and links for community members

- `/warn` -
Issues a warning to users for off-topic content. This warning system works cross-community, not just for specific topics, helping maintain consistent standards across all FLOSS-UZ groups.

## Development

The project has `shell.nix` which has development environment preconfigured already for you. Just open your terminal and at the root of this project:

```bash
# Open in bash by default
nix develop

# If you want other shell
nix develop -c $SHELL

# Upon entering development environment for the first
# time, you'll be asked for your development telegram
# bot token, it will be written to .env file for more
# convenient dev env startups. Token is saved at .env
# file at the root of this project. You can change it
# whenever you want!

# After entering development environment, inside the
# env, you can open your editor, so your editor will
# read all $PATH and environmental variables, also
# your terminal inside your editor will adopt all
# variables, so, you can close terminal.

# Neovim
vim .

# VSCode
code .

# Zed Editor
zed .
```

The development environment has whatever you may need already, but feel free to add or remove whatever inside `flake.nix`.

## Building

Well, there are two ways of building your project. You can either go with classic `cabal build` way, but before that, make sure to enter development environment to have cabal and all Haskell toolchain available in your PATH, you may do like that:

```bash
# Entering development environment
nix develop -c $SHELL

# Compile the project
cabal build
```

Or, you can build your project via nix which will do all the dirty work for you. Just, in your terminal:

```bash
# Build in nix environment
nix build
```

## Deploy & Run

Deploying this project, telegram bot requires host machine to have its own flake based configuration.

### Activation

In your configuration, add your project repository to `inputs`.

```nix
{
  inputs = {
    # ...
    # Let's imagine name of this project as `regulatorbot`
    regulatorbot.url = "github:floss-uz/regulator.git";
  };
}
```

## Working Productions

There are bunch of telegram bots that are using this template and are deployed to which you may refer as working examples:

- [Xinux Assistant](https://t.me/xinuxmgrbot) - [GitHub](https://github.com/xinux-org/telegram) / [Deployed At](https://github.com/kolyma-labs/instances/blob/main/nixos/kolyma-2/services/xinux.nix)
- [Rust Uzbekistan Assistant](https://t.me/rustaceanbot) - [GitHub](https://github.com/rust-lang-uz/telegram) / [Deployed At](https://github.com/kolyma-labs/instances/blob/main/nixos/kolyma-2/services/rustina.nix)

## FAQ

### Why not use default.nix for devShell?

There's been cases when I wanted to reproduce totally different behaviors in development environment and production build. This occurs quite a lot lately for some reason and because of that, I tend to keep both shell.nix and default.nix to don't mix things up.

### Error when building or entering development environment

If you see something like that in the end:

```
error: hash mismatch in fixed-output derivation '/nix/store/fsrachja0ig5gijrkbpal1b031lzalf0-channel-rust-stable.toml.drv':
  specified: sha256-vMlz0zHduoXtrlu0Kj1jEp71tYFXyymACW8L4jzrzNA=
     got:    sha256-Hn2uaQzRLidAWpfmRwSRdImifGUCAb9HeAqTYFXWeQk=
```

Just know that something in that version of rustup changed or sha is outdated, so, just copy whatever shown in `got` and place that in both `default.nix` and `shell.nix` at:

```nix
  # Rust Toolchain via fenix
  toolchain = fenix.packages.${pkgs.system}.fromToolchainFile {
    file = ./rust-toolchain.toml;
    # Bla bla bla bla bla, bla bla bla.
    #                     REPLACE THIS LONG THING!
    sha256 = "sha256-Hn2uaQzRLidAWpfmRwSRdImifGUCAb9HeAqTYFXWeQk=";
  };
```

### How do I get a Telegram bot token?

1. Open Telegram and search for [@BotFather](https://t.me/botfather)
2. Send `/newbot` command
3. Follow the instructions to create your bot
4. Copy the token provided by BotFather
5. Save it securely in your `.env` file or NixOS secrets

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Guidelines
- Follow the existing code style
- Add tests for new features
- Update documentation as needed
- Ensure `cabal test` passes before submitting

## Support

For questions or issues, please:
- Open an issue on GitHub
- Join our FLOSS-UZ community discussions
- Contact the maintainers

---

**Note**: This project is under active development. Features and documentation may change.

[Xinux Community]: https://github.com/xinux-org
[available default options]: #available-default-options