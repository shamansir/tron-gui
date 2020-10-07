const WebSocket = require('ws');

const wss = new WebSocket.Server({ port: 3333 });

const updates = {};

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(message) {
    let package = JSON.parse(message);
    console.log('uuid', package.uuid);

    console.log('code', package.code);
    if (package.code == 'ACK') {
        //broadcast(ws, package.uuid, message);
        if (updates[package.uuid]) {
            updates[package.uuid].forEach(function(update) {
                ws.send(update);
            });
        }
    } else if (package.code == 'BLD') {
        broadcast(ws, package.uuid, message);
    } else if (package.code == 'UPD') {
        if (!updates[package.uuid]) { updates[package.uuid] = []; }
        updates[package.uuid].push(message);
        broadcast(ws, package.uuid, message);
    }
    console.log('received: %s', message);
  });

  //ws.send('something');
});


function broadcast(from, uuid, data) {
    wss.clients.forEach(function each(client) {
        if (client !== from && client.readyState === WebSocket.OPEN) {
            client.send(data);
        }
    })
}
