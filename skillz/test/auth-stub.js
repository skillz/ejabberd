// Stand-in for api-gateway /chat-auth/ (check_password, user_exists) for local tests only.
// The valid "token" for user U is pw-U; GET /calls returns how many auth calls it served.
const http = require('http');
let calls = 0;
const known = (u) => /^([0-9]+|mod1|skillz-cas)$/.test(u);
http.createServer((req, res) => {
  const url = new URL(req.url, 'http://stub');
  const user = url.searchParams.get('user') || '';
  const reply = (code, body) => { res.writeHead(code, {'content-type': 'text/plain'}); res.end(body); };
  if (url.pathname === '/calls') return reply(200, String(calls));
  calls++;
  if (url.pathname === '/chat-auth/check_password') return reply(200, String(known(user) && url.searchParams.get('pass') === `pw-${user}`));
  if (url.pathname === '/chat-auth/user_exists') return reply(200, String(known(user)));
  if (url.pathname === '/chat-auth/fail') return reply(500, 'boom');
  reply(404, 'not found');
}).listen(8080);
