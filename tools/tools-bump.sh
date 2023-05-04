#!/usr/bin/env sh

git commit --allow-empty -m "bump version"
git push
make tags

