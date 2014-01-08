#!/usr/bin/env sh

# Called by `./hblog deploy`

echo "-- Publishing to abizern.org"
echo "-- Rebuilding site..."
./hblog rebuild
cd _publish

echo "-- Updating deploy repository..."
git fetch github
git reset --hard github/master

echo "-- Adding new files to deploy repository..."
rm -rf *
cp -r ../_site/ .

echo "-- Commiting changes"
dt=`date "+%Y-%m-%d %H:%M:%S %Z"`
message="Site update at $dt"
git add .
git commit -m"$message"

echo "-- Deploying site"
git push github master

echo "-- Site published"
