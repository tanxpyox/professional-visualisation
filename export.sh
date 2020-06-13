#!/usr/bin/env bash

# Setup
shopt -s globstar

# Initialise
if [ -d codes/ ]; then
  rm -rf codes
fi

if [ -d gallery/ ]; then
  rm -rf gallery
fi

# Create output folders
mkdir codes
mkdir gallery

# Copy files from src
cp **/*.R codes
cp **/*.png gallery

# Write to manifest
ls -R | grep ".R" | sort | uniq > manifest.txt
