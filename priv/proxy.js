let liveReloadWebSocket = null;
let reconnectTimeout = null;

function connect() {
  clearTimeout(reconnectTimeout);
  liveReloadWebSocket = new WebSocket(
    `ws://${window.location.host}/ws_livereload`,
  );

  liveReloadWebSocket.onmessage = (event) => {
    window.location.reload();
  };
  liveReloadWebSocket.onclose = reconnect;
  liveReloadWebSocket.onerror = reconnect;
}

function reconnect() {
  clearTimeout(reconnectTimeout);
  reconnectTimeout = setTimeout(connect, 5000);
}

connect();
