#!/usr/bin/env zsh

echo "--Preparing to deploy to Abizern.org."
echo "--Update to latest version of site."
cd public
git fetch origin
git reset --hard origin/master
echo "--Updated local version of site."
echo "--Resetting the index, ready for updating."
git rm -rf .
git clean -fxd
echo "--Ready for deployment."
cd ..
echo "--Generating site."
hugo
cd public
echo "--commiting changes"
msg="Site updated at `date`"
git add .
git commit -q -m"$msg"
echo "--Changes Commited."
echo "--Deploying site."
git push origin master
echo "--Site updated."
