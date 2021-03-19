#!/bin/bash
set -e

EPIC="$1"; shift
BASENAME="$1"; shift

if [ -n "$EPIC" -a -n "$BASENAME" ]; then
  if [ -f p ]; then
    JIRA_URL="$(cat jira_url)"
    PASSWORD="$(ghc -e 'import Prelude' -e 'import Data.Char' -e 'interact (fmap (chr . read) . words)' < p)"
    curl -u gelisam:"$PASSWORD" -X GET -H "Content-Type: application/json" \
      "${JIRA_URL}/sr/jira.issueviews:searchrequest-csv-all-fields/temp/SearchRequest.csv?jqlQuery=%22Epic+Link%22+%3D+$EPIC&delimiter=," > "$BASENAME.csv"
  else
    mv ~/Downloads/Jira*.csv "$BASENAME.csv" 2> /dev/null || true
  fi

  jira-dependencies "$BASENAME.csv" > "auto.dot"
  if [ "$1" ]; then
    cat "auto.dot" | dot-reverse | dot-closure "$@" | dot-reverse > "$BASENAME.dot"
  else
    cat "auto.dot" > "$BASENAME.dot"
  fi
  dot -Tpng -Granksep=1 "$BASENAME.dot" | pngtopnm > "auto-top.pnm"

  if [ -f "calendar.png" ]; then
    LEFT_PX="3"
    LEFT_DATE="2020-12-03"
    RIGHT_PX="1171"
    RIGHT_DATE="2021-02-10"
    #CURRENT_DATE="2021-01-02"
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

  TODO="$(cat "$BASENAME.dot" | grep 'style="filled,solid",fillcolor=white,penwidth="1.0"' | wc -l | tr -d ' \t')"
  IN_PROGRESS="$(cat "$BASENAME.dot" | grep 'style="filled,solid",fillcolor="#f1ffdb",penwidth="1.0"' | wc -l | tr -d ' \t')"
  DONE="$(cat "$BASENAME.dot" | grep -v 'label=""' | grep 'style="filled,solid",fillcolor=darkolivegreen1,penwidth="1.0"' | wc -l | tr -d ' \t')"
  WORK_DONE=$(( $DONE - 2 ))
  TOTAL=$(( $TODO + $IN_PROGRESS + $WORK_DONE ))
  OPTIMISTIC=$(( $IN_PROGRESS + $WORK_DONE ))
  echo "$(( $WORK_DONE * 100 / $TOTAL))-$(( $OPTIMISTIC * 100 / $TOTAL ))% ${WORK_DONE}-${OPTIMISTIC}/${TOTAL}"
  #[style="filled,solid",fillcolor="#f1ffdb",penwidth="1.0"];
  #[style="filled,solid",fillcolor=darkolivegreen1,penwidth="1.0"];

else
  echo "usage: $0 PS-12345 BASENAME"
  exit 1
fi
