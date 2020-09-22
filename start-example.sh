# elm-live example/Main.elm --open --start-page=./example/index.html -- --output=./example/app.js
cp ./src/Gui/Gui.css ./example/Gui.css
elm-live example/Main.elm --open --dir=./example -- --output=./example/app.js
