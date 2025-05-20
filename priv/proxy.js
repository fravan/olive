let liveReloadWebSocket = null;
let reconnectTimeout = null;

const RELOAD_ON_RECONNECT_KEY = "shouldReload";

function connect() {
  clearTimeout(reconnectTimeout);
  liveReloadWebSocket = new WebSocket(
    `ws://${window.location.host}/ws_livereload`,
  );

  liveReloadWebSocket.onopen = () => {
    if (sessionStorage.getItem(RELOAD_ON_RECONNECT_KEY)) {
      sessionStorage.removeItem(RELOAD_ON_RECONNECT_KEY);
      window.location.reload();
    }
  };
  liveReloadWebSocket.onmessage = (event) => {
    window.location.reload();
  };
  liveReloadWebSocket.onclose = reconnect;
  liveReloadWebSocket.onerror = reconnect;
}

function reconnect() {
  sessionStorage.setItem(RELOAD_ON_RECONNECT_KEY, "true");
  clearTimeout(reconnectTimeout);
  reconnectTimeout = setTimeout(connect, 5000);
}

connect();
