cp ../src/Gui.css ./Gui.css
cp ./Example/Goose/JetBrainsMono-Regular.woff ./JetBrainsMono-Regular.woff
cp ./Example/Goose/JetBrainsMono-Regular.woff2 ./JetBrainsMono-Regular.woff2
elm-live ./Detachable/Main.elm --open --dir=. --start-page=./Detachable/index.html -- --output=./app.js
