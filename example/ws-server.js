const WebSocket = require('ws');

const WSS_PORT = 80;

const http = require('http');
const server = http.createServer(function (req, res) {
    var url = req.url;
    if (url === '/healthz' ){
       res.writeHead(200, { 'Content-Type': 'text/html' }); // http header
       res.write('<h1>OK<h1>'); //write a response
       res.end(); //end the response
    }
   });

const wss = new WebSocket.Server({ server });

const updates = {};

console.log('starting server on port', WSS_PORT);

wss.on('connection', function connection(ws) {
  console.log("new connection!");
  ws.on('message', function incoming(message) {
    let package = JSON.parse(message);
    console.log(`${package.code} ${package.data.client}`);
    if (package.code == 'ACK') {
        if (updates[package.data.client]) {
            updates[package.data.client].forEach(function(update) {
                ws.send(update);
            });
        }
    } else if (package.code == 'BLD') {
        broadcast(ws, package.data.client, message);
    } else if (package.code == 'CLR') {
        updates[package.data.client] = [];
    } else if (package.code == 'UPD') {
        if (!updates[package.data.client]) { updates[package.data.client] = []; }
        updates[package.data.client].push(message);
        broadcast(ws, package.data.client, message);
    }
    console.log('received: %s', message);
  });

  //ws.send('something');
});


function broadcast(from, clientId, data) {
    wss.clients.forEach(function each(client) {
        if (client !== from && client.readyState === WebSocket.OPEN) {
            client.send(data);
        }
    })
}

server.listen(WSS_PORT);
