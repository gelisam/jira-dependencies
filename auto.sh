#!/bin/bash
set -e

BASENAME="$1"

if [ "$BASENAME" ]; then
  mv ~/Downloads/Jira*.csv "$BASENAME.csv" 2> /dev/null || true
  jira-dependencies "$BASENAME.csv" > "$BASENAME.dot"
  dot -Tpng -Granksep=1 "$BASENAME.dot" > "auto-top.png"
  pnmcat -topbottom -jleft <(pngtopnm auto-top.png) <(pngtopnm legend.png) | \
    pnmtopng > "$BASENAME.png"
  imgcat "$BASENAME.png"
else
  echo "usage: $0 BASENAME"
  exit 1
fi
