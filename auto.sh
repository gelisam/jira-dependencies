#!/bin/bash
set -e

BASENAME="$1"

if [ "$BASENAME" ]; then
  mv ~/Downloads/Jira*.csv "$BASENAME.csv" 2> /dev/null || true
  jira-dependencies "$BASENAME.csv" > "$BASENAME.dot"
  dot -Tpng -Granksep=1 "$BASENAME.dot" | pngtopnm > "auto-top.pnm"

  if [ -f "calendar.png" ]; then
    LEFT_PX="3"
    LEFT_DATE="2020-12-03"
    RIGHT_PX="1171"
    RIGHT_DATE="2021-02-10"
    #CURRENT_DATE="2021-01-26"
    LEFT_YYYY="$(echo "$LEFT_DATE" | cut -d'-' -f1)"
    LEFT_MM="$(echo "$LEFT_DATE" | cut -d'-' -f2)"
    LEFT_DD="$(echo "$LEFT_DATE" | cut -d'-' -f3)"
    LEFT_TIMESTAMP="$(date -j "${LEFT_MM}${LEFT_DD}0000${LEFT_YYYY}" +"%s")"
    RIGHT_YYYY="$(echo "$RIGHT_DATE" | cut -d'-' -f1)"
    RIGHT_MM="$(echo "$RIGHT_DATE" | cut -d'-' -f2)"
    RIGHT_DD="$(echo "$RIGHT_DATE" | cut -d'-' -f3)"
    RIGHT_TIMESTAMP="$(date -j "${RIGHT_MM}${RIGHT_DD}0000${RIGHT_YYYY}" +"%s")"
    #CURRENT_YYYY="$(echo "$CURRENT_DATE" | cut -d'-' -f1)"
    #CURRENT_MM="$(echo "$CURRENT_DATE" | cut -d'-' -f2)"
    #CURRENT_DD="$(echo "$CURRENT_DATE" | cut -d'-' -f3)"
    #CURRENT_TIMESTAMP="$(date -j "${CURRENT_MM}${CURRENT_DD}0000${CURRENT_YYYY}" +"%s")"
    CURRENT_TIMESTAMP="$(date +"%s")"
    CURRENT_PX=$(( $LEFT_PX + ($CURRENT_TIMESTAMP - $LEFT_TIMESTAMP) * ($RIGHT_PX - $LEFT_PX) / ($RIGHT_TIMESTAMP - $LEFT_TIMESTAMP) ))

    pngtopnm calendar.png > auto-calendar-orig.pnm
    HEIGHT="$(cat auto-calendar-orig.pnm | head -n 2 | tail -n 1 | cut -d' ' -f2)"
    ppmdraw -script="setpos $CURRENT_PX 0; setcolor #FF0000; line_here 0 $HEIGHT" auto-calendar-orig.pnm > auto-calendar.pnm

    pnmcat -leftright <(pngtopnm legend.png) auto-calendar.pnm > "auto-bottom.pnm"
  else
    pngtopnm legend.png > "auto-bottom.pnm"
  fi

  pnmcat -topbottom -jleft auto-top.pnm auto-bottom.pnm | \
    pnmtopng > "$BASENAME.png"
  imgcat "$BASENAME.png"
else
  echo "usage: $0 BASENAME"
  exit 1
fi
