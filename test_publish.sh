#!/usr/bin/env sh

# Called directly, just to check whether different machines create different output

echo "-- Comparing output"
echo "-- Rebuilding site..."
./hblog rebuild
cd _publish

echo "-- Updating deploy repository..."
git fetch github
git reset --hard github/master

echo "-- Adding new files to deploy repository..."
rm -rf *
cp -r ../_site/ .

echo "-- And the result is..."
git status
