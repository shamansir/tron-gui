# elm-live example/Main.elm --open --start-page=./example/index.html -- --output=./example/app.js
cp ./src/Gui/Gui.css ./example/Gui.css
elm-live example/DatGui/Main.elm --open --dir=./example --start-page=./DatGui/index.html -- --output=./example/app.js
