#!/bin/bash

if [ "$1" ]; then
    cp ../src/Tron.css ./Tron.css
    cp ../Tron.js ./Tron.js

    cp ./Example/Goose/JetBrainsMono-Regular.woff ./JetBrainsMono-Regular.woff
    cp ./Example/Goose/JetBrainsMono-Regular.woff2 ./JetBrainsMono-Regular.woff2
    elm-live ./$1/Main.elm --port=8101 --open --dir=. --start-page=./$1/index.html -- --output=./app.js
else
    echo "specify which example to run, such as:"
    echo ""
    echo "Basic — just the GUI and the Goose"
    echo "Everything — all the features in one: theme, docking, random, detachable, ..."
    echo "Detachable — the parts of GUI may be detached to a separate tab"
    echo "             (run 'start-server.sh' first)"
    echo "DatGui — connecting to 'dat.gui' using JS transfer"
    echo "OneKnob — only one control and its value, nothing else"
    echo "Random — random interface by a click of a button"
    echo "Constructor — constructor for the interface"
    echo "AFrame — render to virtual reality using A-Frame (currently, the early draft)"
    echo "ReportToJs — a demonstration of sending any value update to port as JSON,"
    echo "             from Tron to JS, as well as the complete GUI structure"
    echo "BuildFromJs — a demonstration of building the complete interface in JS"
    echo "              using 'Tron.ts' helper, and listening to its changes"
    echo "              (similarly to 'dat.gui')"
    echo "ListenFromJs — a demonstration of listening to all the changes in the UI"
    echo "               using 'Tron.ts' helper and the companion object"
fi
