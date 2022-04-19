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

The following command will install `neodiff`, `git-neodiff` and `git-neoshow` to `~/.local/bin`. Please make sure this directory is covered by your `PATH` variable!
```
curl -s https://github.com/SchneePingu/git-neodiff/releases/download/v0.2.0.0/install.sh | bash
```

## Example

<p align="left"><img src="/doc/demo.png" alt="demo" width="250"></p>
