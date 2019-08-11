#!/bin/bash

#
# it would be great if Elm init is able to parse `flags: undefined` as `Maybe Flags` but it can't.
# so, we stick in a `flags: null` manually
#

sed 's/node\: document\.getElementById/flags: null, node\: document\.getElementById/g' index.html > index.html.new
mv index.html.new index.html
