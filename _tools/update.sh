#!/bin/bash

if [ $# -eq 0 ]; then
  echo "Usage: update path/to/post.md"
  echo "Will set field 'updated:' to current time."
  exit 1
fi

date=`date "+%Y-%m-%d %H:%M:%S %z"`
perl -pi -e 's/^updated: .*/updated: '"$date"'/' $1
