#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

function compile {
  # deno check test.ts src/*.ts 2>&1
  yarn elm-esm make src/Main.elm src/TestRunner.elm --output=dist/elm-unreadable.js \
    && elm-make-readable dist/elm-unreadable.js dist/elm.js \
    && ./src/test.ts 2>&1 | \
      tee \
        >(grep -A2 ".*error.*Uncaught" >uncaught.txt) \
        >(grep "^:" | column -t -s\| >table.txt)

  if [ -s table.txt -o -s uncaught.txt ]; then
    echo
    echo
    echo
    echo "-------------------"
    echo
    echo
    echo
  fi

  cat table.txt
  rm table.txt
  echo

  cat uncaught.txt
  rm uncaught.txt
  echo
}

function run {
  clear;
  tput reset;
  echo -en "\033c\033[3J";

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  compile;
}

run;

chokidar src stdlib | while read WHATEVER; do
  run;
done;
