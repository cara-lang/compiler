#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

function compile {
  # deno check test.ts src/*.ts 2>&1
  yarn elm-esm make src/Main.elm src/TestRunner.elm --output=dist/elm.js \
    && ./src/index.ts 2>&1 | tee >(grep -A2 ".*error.*Uncaught" >uncaught.txt) >(grep "^:" | column -t -s\| >table.txt)

  echo
  echo
  echo
  echo "-------------------"
  echo
  echo
  echo

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

chokidar src | while read WHATEVER; do
  run;
done;
