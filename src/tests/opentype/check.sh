#!/bin/sh
# Run everything that guards the OpenType math work: the unit tests and the
# sample renders (tuned and untuned), with a pixel diff against
# tests/build/ref when it exists. Use before committing.
#
#   TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/check.sh
set -e
here=$(cd "$(dirname "$0")" && pwd); top=$(cd "$here/../.." && pwd)
cd "$top"
make -C tests
ref=""; [ -d tests/build/ref ] && ref="-c $top/tests/build/ref"
mkdir -p tests/build/vis
stamp="tests/build/vis/.check-stamp"; touch "$stamp"
tests/opentype/render-samples.sh $ref
TM_HAND_TUNED=off tests/opentype/render-samples.sh
# With the native PDF writer every glyph is a font glyph or vectors: a Type 3
# font means a bitmap, and mutool must read every page without a syntax error
if command -v mutool > /dev/null; then
  for pdf in $(find tests/build/vis -name '*.pdf' -newer "$stamp"); do
    mutool info "$pdf" | grep -q Hummus || continue
    t3=$(mutool info -F "$pdf" 2>/dev/null | grep -c Type3 || true)
    err=$(mutool draw -q -F stext -o /dev/null "$pdf" 2>&1 | grep -c 'syntax error' || true)
    if [ "$t3" != "0" ] || [ "$err" != "0" ]; then
      echo "check.sh: $pdf has $t3 bitmap (Type 3) fonts and $err syntax errors"
      exit 1
    fi
  done
fi
# the symbol tables, when the unicode-math list is at hand
if [ -n "$TM_UNICODE_MATH_TABLE" ]; then
  python3 "$here/missing-symbols.py" -t "$TM_UNICODE_MATH_TABLE" --check
fi
echo "check.sh: all passed"
