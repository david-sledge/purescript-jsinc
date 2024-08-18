#!/bin/bash

base_dir="examples/$1"

spago bundle -p $1 --bundle-type app --platform browser --minify --outfile dist/index.js && \
  cp $base_dir/html/* $base_dir/dist/ && \
  mv $base_dir/dist/index.html $base_dir/dist/index0.html && \
  inline-source --compress false --root $base_dir/dist $base_dir/dist/index0.html > $base_dir/dist/index.html && \
  rm $base_dir/dist/index0.html && \
  rm $base_dir/dist/index.js
