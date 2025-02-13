#!/usr/bin/env bash

set -e  # Exit on any error

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

COMET_DIR="$SCRIPT_DIR/../comet"
BUILD_DIR="$SCRIPT_DIR"

if [[ ! -d "$COMET_DIR" ]]; then
  echo "Error: Directory $COMET_DIR does not exist!"
  exit 1
fi

pushd "$COMET_DIR"
make
popd

# Compile test binary
gfortran -ffree-line-length-none -I"$COMET_DIR/include" -o "$BUILD_DIR/test_2d_element" \
  "$BUILD_DIR/test_2d_element.f90" -L"$COMET_DIR/bin" -Wl,-rpath,"$COMET_DIR/bin" -lglobals -lelement_library

chmod +x "$BUILD_DIR/test_2d_element"

# Run test
"$BUILD_DIR/test_2d_element" > "$BUILD_DIR/element_results.txt"
