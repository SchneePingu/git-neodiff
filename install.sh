#!/bin/bash

RELEASE_TAG="v0.2.0.0"
INSTALL_DIR="$HOME/.local/bin"

if [ ! -d "$INSTALL_DIR" ]; then
  mkdir -p "$INSTALL_DIR"
fi

cd "$INSTALL_DIR"

NEODIFF_URL="https://github.com/SchneePingu/git-neodiff/releases/download/$RELEASE_TAG/neodiff"
if curl --output /dev/null --silent --head --fail -L "$NEODIFF_URL"; then
  if curl --silent --fail -L -O "$NEODIFF_URL"; then
    chmod +x "neodiff"
  fi
fi

GIT_NEODIFF_URL="https://github.com/SchneePingu/git-neodiff/releases/download/$RELEASE_TAG/git-neodiff"
if curl --output /dev/null --silent --head --fail "$GIT_NEODIFF_URL"; then
  if curl --silent --fail -L -O "$GIT_NEODIFF_URL"; then
    chmod +x "git-neodiff"
  fi
fi

GIT_NEOSHOW_URL="https://github.com/SchneePingu/git-neodiff/releases/download/$RELEASE_TAG/git-neoshow"
if curl --output /dev/null --silent --head --fail "$GIT_NEOSHOW_URL"; then
  if curl --silent --fail -L -O "$GIT_NEOSHOW_URL"; then
    chmod +x "git-neoshow"
  fi
fi
