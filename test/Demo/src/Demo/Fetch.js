export function fetchTextImpl(url) {
  return function (onOk) {
    return function (onErr) {
      return function () {
        fetch(url)
          .then(function (res) {
            if (!res.ok) {
              onErr("HTTP " + res.status + " fetching " + url)();
              return null;
            }
            return res.text();
          })
          .then(function (text) {
            if (text !== null && text !== undefined) {
              onOk(text)();
            }
          })
          .catch(function (e) {
            onErr(String(e && e.message ? e.message : e))();
          });
      };
    };
  };
}
