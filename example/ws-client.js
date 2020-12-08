function startWs(ackToWs, receieveUpdateFromWs, sendUpdateToWs) {
    const WSS_HOST = 'localhost';
    const WSS_PORT = '80';

    let socket = new WebSocket("ws://" + WSS_HOST + ":" + WSS_PORT);

    let clientId = null;

    socket.onopen = function(e) {
        console.info("[open] Connection established");
    };

    ackToWs.subscribe((update) => {
        const package = JSON.stringify({ data : update, code: 'ACK' });
        clientId = update.client;
        console.log(`[message] Client ID established: ${clientId}`);
        if (socket.readyState == 1) {
            socket.send(package);
        } else {
            socket.onopen = () => {
                socket.send(package);
            }
        }
    });

    socket.onmessage = function(event) {
        console.info(`[message] Data received from server: ${event.data}`);
        const package = JSON.parse(event.data);
        if (clientId && (clientId === package.data.client)) {
            receieveUpdateFromWs.send(package.data);
        }
    };

    socket.onclose = function(event) {
        if (event.wasClean) {
            console.info(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
        } else {
            // e.g. server process killed or network down
            // event.code is usually 1006 in this case
            console.info('[close] Connection died');
        }
    };

    socket.onerror = function(error) {
        console.info(`[error] ${error.message}`);
    };

    sendUpdateToWs.subscribe((update) => {
        const package = JSON.stringify({ data : update, code: 'UPD' });
        console.log('send update to WS', package);
        socket.send(package);
    });

}

window.startWs = startWs;

