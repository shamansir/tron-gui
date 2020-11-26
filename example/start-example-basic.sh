cp ../src/Gui.css ./Gui.css
cp ./Example/Goose/JetBrainsMono-Regular.woff ./JetBrainsMono-Regular.woff
cp ./Example/Goose/JetBrainsMono-Regular.woff2 ./JetBrainsMono-Regular.woff2
elm-live ./Basic/Main.elm --open --dir=. --start-page=./Basic/index.html -- --output=./app.js
