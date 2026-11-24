// Stand-in for the HTTP services the plugin modules call, for local tests only: push-notification-service
// (/v1/triggered_events), user-details-service (/chat/quarantines, /profile/show) and the support-ticket
// endpoints, and the statsd-exporter sidecar (UDP 11111). Listens on 80, 8080 and 9100; GET /calls returns
// every call and statsd line as JSON lines, DELETE /calls clears.
// /profile/show: user 1002 is staff, user 1003 is a chat moderator.
const http = require('http');
let calls = [];
const handler = (port) => (req, res) => {
  let body = '';
  req.on('data', (c) => { body += c; });
  req.on('end', () => {
    const json = (code, obj) => { res.writeHead(code, {'content-type': 'application/json'}); res.end(typeof obj === 'string' ? obj : JSON.stringify(obj)); };
    if (req.url === '/calls') {
      if (req.method === 'DELETE') calls = [];
      return json(200, calls.map((c) => JSON.stringify(c)).join('\n'));
    }
    calls.push({port, method: req.method, path: req.url, headers: {'skillz-user-id': req.headers['skillz-user-id'], 'skillz-game-id': req.headers['skillz-game-id']}, body});
    if (req.url === '/profile/show') {
      const id = String((body.match(/"target_user_id":\s*(\d+)/) || [])[1]);
      return json(200, {skillz_staff: id === '1002', chat_moderator: id === '1003'});
    }
    json(200, {});
  });
};
for (const port of [80, 8080, 9100]) http.createServer(handler(port)).listen(port);
require('dgram').createSocket('udp4').on('message', (m) => calls.push({port: 11111, method: 'UDP', path: '', headers: {}, body: String(m)})).bind(11111);
