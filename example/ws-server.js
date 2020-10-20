const WebSocket = require('ws');

const wss = new WebSocket.Server({ port: 3333 });

const updates = {};

wss.on('connection', function connection(ws) {
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
