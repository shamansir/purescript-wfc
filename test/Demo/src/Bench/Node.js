export function argv() {
  return process.argv.slice(2);
}

export function onSigint(handler) {
  return function () {
    process.on("SIGINT", function () {
      handler();
    });
  };
}

export function exitProcess(code) {
  return function () {
    process.exit(code);
  };
}
