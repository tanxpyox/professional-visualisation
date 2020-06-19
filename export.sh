#!/usr/bin/env bash

# Setup
shopt -s globstar

# Set export directory (root if local, /deploy if CI)
if [ -z "$CI" ]; then
  export DIR=.
else
  export DIR=deploy
fi

# Initialise
if [ -d $DIR/codes ]; then
  rm -rf $DIR/codes
fi

if [ -d $DIR/gallery ]; then
  rm -rf $DIR/gallery
fi

# Create output folders
mkdir -p ${DIR}/codes
mkdir ${DIR}/gallery

# Copy files from src
cp src/**/*.R ${DIR}/codes
cp src/**/*.png ${DIR}/gallery

# Clean Rproject file from codes if exist
if [ -f ${DIR}/codes/*.Rproj ]; then
  rm ${DIR}/codes/*.Rproj
fi

# Write to manifest
ls ${DIR}/codes | sort | uniq > deploy/manifest.txt
