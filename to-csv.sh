#!/usr/bin/env bash

echo "test,result"
for TEST in $(find end-to-end-tests -type d | sort | tail -n +2); do
  unset FAILED
  NAME=$(basename $TEST)
  TEMP_STDOUT_FILE=$(mktemp)
  TEMP_STDERR_FILE=$(mktemp)
  ./run-test.sh $NAME 1>$TEMP_STDOUT_FILE 2>$TEMP_STDERR_FILE
  WANTED_STDOUT="$TEST/stdout.txt"
  WANTED_STDERR="$TEST/stderr.txt"
  if [ -f "$WANTED_STDOUT" ]; then
    if [ ! -s $TEMP_STDOUT_FILE ]; then
      # didn't have stdout but wanted it
      FAILED=1
    elif ! diff -q $TEMP_STDOUT_FILE $WANTED_STDOUT &>/dev/null; then
      # stdouts differ
      FAILED=1
    fi
  else
    if [ -s $TEMP_STDOUT_FILE ]; then
      # had stdout but didn't want it
      FAILED=1
    fi
  fi
  if [ -f "$WANTED_STDERR" ]; then
    if [ ! -s $TEMP_STDERR_FILE ]; then
      # didn't have stderr but wanted it
      FAILED=1
    elif ! diff -q $TEMP_STDERR_FILE $WANTED_STDERR &>/dev/null; then
      # stderrs differ
      FAILED=1
    fi
  else
    if [ -s $TEMP_STDERR_FILE ]; then
      # had stderr but didn't want it
      FAILED=1
    fi
  fi
  if [ -z ${FAILED+x} ]; then
    RESULT=passed
  else
    RESULT=failed
  fi
  echo "$NAME,$RESULT"
done
