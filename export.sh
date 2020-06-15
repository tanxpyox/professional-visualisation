#!/usr/bin/env bash

# Setup
shopt -s globstar

# Create output folders
mkdir -p deploy/codes
mkdir deploy/gallery

# Copy files from src
cp **/*.R deploy/codes
cp **/*.png deploy/gallery

# Clean Rproject file from codes if exist
if [ -f deploy/codes/*.Rproj ]; then
  rm deploy/codes/*.Rproj
fi

# Write to manifest
ls deploy/codes | sort | uniq > deploy/manifest.txt
