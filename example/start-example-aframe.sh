cp ./Example/Goose/JetBrainsMono-Regular.woff ./JetBrainsMono-Regular.woff
cp ./Example/Goose/JetBrainsMono-Regular.woff2 ./JetBrainsMono-Regular.woff2
elm-live --port 8100 ./AFrame/Main.elm --open --dir=. --start-page=./AFrame/index.html -- --output=./app.js
