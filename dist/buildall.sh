#!/bin/bash

LOG_DIR=$(pwd)/log
BUILD_LOG=$LOG_DIR/buildall.log

mkdir -p $LOG_DIR
cp /dev/null $BUILD_LOG
rm $LOG_DIR/goog-api-client-*.log

ls -d goog-api-client-* | sort | while read proj; do
  cd $proj

  proj_log=$LOG_DIR/$proj.log
  cp /dev/null $proj_log

  # stack clean
  if stack build; then
    echo "$(date '+%Y.%m.%d %H:%M:%S') $proj OK" >>$BUILD_LOG
  else
    echo "$(date '+%Y.%m.%d %H:%M:%S') $proj NG" >>$BUILD_LOG
  fi 2>&1 | tee $proj_log

  cd ..
done
