<p align="left">
    <a href="https://img.shields.io/badge/git--neodiff-v0.2-purple.svg"><img src="https://img.shields.io/badge/git--neodiff-v0.2-blue.svg" alt="git-neodiff"></a>
    <a href="https://img.shields.io/badge/license-BSD3-orange.svg"><img src="https://img.shields.io/badge/license-BSD3-orange.svg" alt="license"></a>
    <a href="https://img.shields.io/badge/Haskell-2010-purple.svg"><img src="https://img.shields.io/badge/Haskell-2010-purple.svg" alt="vim"></a>
</p>

# git-neodiff

## What is it?

`git-neodiff` is a set of command-line tools to format the output of [git-diff](https://git-scm.com/docs/git-diff) and [git-show](https://git-scm.com/docs/git-show). It consists of the wrappers `git-neodiff` and `git-neoshow`, replacing `git-diff` and `git-show`, and the parser `neodiff`, which formats the output.
To use it, just type `git neodiff` instead of `git diff` and `git neoshow` insead of `git show`. 

```
git neodiff ...
```

```
git neoshow ...
```

## Installation (Linux)

For installation, it is recommend to use the provided installation script, which makes use of a precompiled binary of `neodiff`. If you experience issues with the binary, due to a different computer architecture or even operating system, it is also possible to install it from source code. However, this requires [stack](https://docs.haskellstack.org/en/stable/README/) to be installed.

### Installation script (recommended)

The following command will install `neodiff`, `git-neodiff` and `git-neoshow` to `~/.local/bin`. Please make sure this directory is covered by your `PATH` variable!
```
curl -Ls https://github.com/SchneePingu/git-neodiff/releases/download/v0.2.1.0/install.sh | bash
```

### Installation from source code (advanced)

To install from source code, first clone the repository, change directory to the root of the repository and checkout the release commit.

```
git checkout v0.2.1.0
```

`neodiff` may then be installed by means of [stack](https://docs.haskellstack.org/en/stable/README/). To install it, run
```
stack install --local-bin-path ~/.local/bin
```

Both `git-neodiff` and `git-neoshow` are installed by copying the corresponding scripts.
```
cp sh/git-neodiff ~/.local/bin
chmod +x ~/.local/bin/git-neodiff

cp sh/git-neoshow ~/.local/bin
chmod+x ~/.local/bin/git-neoshow
```

## Example

<p align="left"><img src="/doc/demo.png" alt="demo" width="250"></p>
