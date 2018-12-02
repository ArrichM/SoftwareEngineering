#!/bin/bash
# Run the R application from this bash script - easier for users without RStudio

# get directory of the source the R codes from:

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
echo $DIR

# check if R is installed and return error if not:

if ! [ -x "$(command -v R)" ]; then
  osascript -e 'tell application (path to frontmost application as text) to display dialog "Error: R is not installed. In order to run this program, please install R" buttons {"OK"} with icon stop'
  exit 1
fi

#run the app from R

R -e "shiny::runApp('$DIR', launch.browser=T)"
