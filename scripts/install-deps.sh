#!/usr/bin/env bash
# Install the tools needed to build mdvi. Safe to run again.
set -euo pipefail

if [[ $# -gt 0 ]]; then
    echo "Usage: $0"
    echo "Installs missing build tools and Rust via rustup. May invoke sudo."
    [[ $1 == --help || $1 == -h ]] && exit 0
    exit 1
fi

as_root() {
    if [[ $EUID -eq 0 ]]; then
        "$@"
    else
        sudo "$@"
    fi
}

# Find an existing rustup installation even in a shell with an outdated PATH.
cargo_home=${CARGO_HOME:-"$HOME/.cargo"}
export PATH="$cargo_home/bin:$PATH"

missing=()
for tool in cc make curl; do
    command -v "$tool" >/dev/null 2>&1 || missing+=("$tool")
done

if (( ${#missing[@]} )); then
    echo "Missing build tools: ${missing[*]}"
    case "$(uname -s)" in
        Linux)
            if command -v dnf >/dev/null 2>&1; then
                as_root dnf install -y gcc make curl ca-certificates
            elif command -v apt-get >/dev/null 2>&1; then
                as_root apt-get update
                as_root apt-get install -y build-essential curl ca-certificates
            elif command -v pacman >/dev/null 2>&1; then
                as_root pacman -S --needed base-devel curl ca-certificates
            else
                echo "Install a C compiler/linker, make, curl, and CA certificates using your package manager, then rerun this script." >&2
                exit 1
            fi
            ;;
        Darwin)
            echo "Install Apple's Command Line Tools, then rerun this script."
            xcode-select --install
            exit 1
            ;;
        *)
            echo "Unsupported platform. Use Linux (including WSL) or macOS." >&2
            exit 1
            ;;
    esac
fi

if ! cargo --version >/dev/null 2>&1 || ! rustc --version >/dev/null 2>&1; then
    if command -v rustup >/dev/null 2>&1; then
        rustup toolchain install stable --profile minimal
        rustup default stable
    else
        echo "Installing stable Rust and Cargo with rustup (https://rust-lang.org/tools/install/)."
        installer=$(mktemp)
        trap 'rm -f "$installer"' EXIT
        curl --proto '=https' --tlsv1.2 --fail --silent --show-error https://sh.rustup.rs -o "$installer"
        sh "$installer" -y --profile minimal --default-toolchain stable
    fi
fi

for tool in cc make curl cargo rustc; do
    "$tool" --version >/dev/null
done
echo
echo "Build dependencies are ready:"
cargo --version
rustc --version
echo
echo "Run these commands in your current shell:"
printf '  export PATH=%q/bin:"$PATH"\n' "$cargo_home"
echo "  make install"
