export function postMessage(msg) {
  return function () {
    self.postMessage(msg);
  };
}

export function onMessage(cb) {
  return function () {
    self.onmessage = function (ev) {
      cb(ev)();
    };
  };
}
