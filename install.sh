#!/bin/sh
# Klassic installer:
#
#   curl -fsSL https://raw.githubusercontent.com/klassic/klassic/main/install.sh | sh
#
# Downloads the latest release binary (plus libklassic_runtime.a for
# the portable C backend) into ~/.klassic/bin. Set KLASSIC_VERSION to
# pin a tag, KLASSIC_HOME to change the install root.
set -eu

REPO="klassic/klassic"
INSTALL_DIR="${KLASSIC_HOME:-$HOME/.klassic}/bin"

OS="$(uname -s)"
ARCH="$(uname -m)"
case "$OS" in
    Linux)
        case "$ARCH" in
            x86_64) TARGET="x86_64-unknown-linux-musl" ;;
            *)
                echo "error: no prebuilt binary for Linux/$ARCH yet (try: cargo install --git https://github.com/$REPO)" >&2
                exit 1
                ;;
        esac
        ;;
    Darwin)
        case "$ARCH" in
            arm64) TARGET="aarch64-apple-darwin" ;;
            x86_64) TARGET="x86_64-apple-darwin" ;;
            *)
                echo "error: no prebuilt binary for macOS/$ARCH" >&2
                exit 1
                ;;
        esac
        ;;
    *)
        echo "error: no prebuilt binary for $OS yet (try: cargo install --git https://github.com/$REPO)" >&2
        exit 1
        ;;
esac

TAG="${KLASSIC_VERSION:-}"
if [ -z "$TAG" ]; then
    TAG="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" \
        | grep -o '"tag_name": *"[^"]*"' | head -1 | cut -d'"' -f4)"
fi
if [ -z "$TAG" ]; then
    echo "error: could not determine the latest release tag" >&2
    exit 1
fi

URL="https://github.com/$REPO/releases/download/$TAG/klassic-$TAG-$TARGET.tar.gz"
echo "downloading klassic $TAG ($TARGET)..."
mkdir -p "$INSTALL_DIR"
curl -fsSL "$URL" | tar xz -C "$INSTALL_DIR"
chmod +x "$INSTALL_DIR/klassic"

# Dogfooding smoke: the freshly installed compiler verifies itself by
# evaluating a Klassic program.
"$INSTALL_DIR/klassic" -e 'println("klassic " + "is " + "alive")' >/dev/null

echo "installed: $("$INSTALL_DIR/klassic" --version) -> $INSTALL_DIR"
case ":$PATH:" in
    *":$INSTALL_DIR:"*) ;;
    *)
        echo
        echo "add it to your PATH, e.g.:"
        echo "  export PATH=\"$INSTALL_DIR:\$PATH\""
        ;;
esac
