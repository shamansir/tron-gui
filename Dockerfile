FROM node:8

RUN mkdir /app

WORKDIR /app

COPY . /app

RUN rm -Rf ./node_modules

RUN npm install

RUN npm install elm

RUN chmod +x ./node_modules/elm/bin/elm

RUN ./node_modules/elm/bin/elm make example/Main.elm --output=./example/app.js

FROM nginx:1.15

COPY --from=0 /app/example/app.js /usr/share/nginx/html
COPY --from=0 /app/example/dat.gui.min.js /usr/share/nginx/html
COPY --from=0 /app/example/Gui.css /usr/share/nginx/html
COPY --from=0 /app/example/Gui.js /usr/share/nginx/html
COPY --from=0 /app/example/index.html /usr/share/nginx/html

COPY ./nginx.conf /etc/nginx/conf.d/default.conf