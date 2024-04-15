#!/bin/bash

quarto render

echo "rendered successfully"

find ~/documents/projects/crickleaze.cc/backup -mindepth 1 -maxdepth 1 -exec cp -a {} ~/documents/projects/crickleaze.cc/docs \;

echo "backup files copied successfully"