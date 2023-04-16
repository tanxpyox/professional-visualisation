#!/usr/bin/env bash
# Setup
shopt -s globstar
# Set export directory (root if local, /deploy if CI)
[ -z "$CI" ] && DIR=. || DIR=deploy
# Initialise
[ -d $DIR/codes ] && rm -rf $DIR/codes
[ -d $DIR/gallery ] && rm -rf $DIR/gallery
# Create output folders
mkdir -p ${DIR}/codes
mkdir ${DIR}/gallery
# Copy files from src
cp src/**/*.R ${DIR}/codes
cp src/**/*.png ${DIR}/gallery
# Clean Rproject file from codes if exist
[ -f ${DIR}/codes/*.Rproj ] && rm ${DIR}/codes/*.Rproj
# Write to manifest
ls ${DIR}/codes | sort | uniq > ${DIR}/manifest.txt
