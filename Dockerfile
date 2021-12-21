FROM node:8

ARG EXAMPLE=Everything

RUN mkdir /app

WORKDIR /app

COPY . /app

RUN rm -Rf ./node_modules

RUN npm install

RUN npm install elm

RUN npm install typescript

RUN chmod +x ./node_modules/typescript/bin/tsc
RUN chmod +x ./node_modules/elm/bin/elm
RUN chmod -f +rx ./elm-stuff/0.19.1/d.dat || true

# RUN cd ./example

RUN node_modules/typescript/bin/tsc ./Tron.helper.ts
RUN cd ./example && ../node_modules/elm/bin/elm make ./$EXAMPLE/Main.elm --output=./app.js

FROM nginx:1.15

ARG EXAMPLE=Everything

COPY --from=0 /app/Tron.helper.js /usr/share/nginx/html
COPY --from=0 /app/example/app.js /usr/share/nginx/html
COPY --from=0 /app/example/dat.gui.min.js /usr/share/nginx/html
COPY --from=0 /app/src/Tron.css /usr/share/nginx/html
COPY --from=0 /app/example/dat-gui-proxy.js /usr/share/nginx/html
COPY --from=0 /app/example/ws-client.js /usr/share/nginx/html
COPY --from=0 /app/example/$EXAMPLE/index.html /usr/share/nginx/html
COPY --from=0 /app/example/Example/Goose/JetBrainsMono-Regular.woff /usr/share/nginx/html
COPY --from=0 /app/example/Example/Goose/JetBrainsMono-Regular.woff2 /usr/share/nginx/html
COPY --from=0 /app/example/example.css /usr/share/nginx/html
RUN mkdir /usr/share/nginx/html/assets
COPY --from=0 /app/example/assets/ /usr/share/nginx/html/assets/

COPY ./nginx.conf /etc/nginx/conf.d/default.conf
