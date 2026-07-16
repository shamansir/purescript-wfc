import { WebSocketServer } from "ws";

export function createWsServer(port) {
  return function () {
    return new WebSocketServer({ port });
  };
}

export function onConnection(wss) {
  return function (handler) {
    return function () {
      wss.on("connection", function (ws) {
        handler(ws)();
      });
    };
  };
}

export function onMessage(ws) {
  return function (handler) {
    return function () {
      ws.on("message", function (data) {
        handler(data.toString())();
      });
    };
  };
}

export function onClose(ws) {
  return function (handler) {
    return function () {
      ws.on("close", function () {
        handler();
      });
    };
  };
}

export function send(ws) {
  return function (msg) {
    return function () {
      if (ws.readyState === ws.OPEN) {
        ws.send(msg);
      }
    };
  };
}
