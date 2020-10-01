const WebSocket = require('ws');

const wss = new WebSocket.Server({ port: 3333 });

const data = {};

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(message) {
    let package = JSON.parse(message);
    console.log('uuid', package.uuid);
    console.log('code', package.code);
    if (package.code == 'ASK') {
        broadcast(ws, package);
    } else if (package.code == 'BLD') {
        broadcast(ws, package);
    } else if (package.code == 'UPD') {
        broadcast(ws, package);
    }
    console.log('received: %s', message);
  });

  ws.send('something');
});


function broadcast(from, data) {
    wss.clients.forEach(function each(client) {
        if (client !== from && client.readyState === WebSocket.OPEN) {
            client.send(data);
        }
    })
}
