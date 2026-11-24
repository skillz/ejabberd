// P15 check: the statsd lines the server sends to the statsd-exporter sidecar (captured by service-stub.js
// on UDP 11111) for a login, a room message and a logout: <host>/<node>.<metric>:<value>|<c|g>.
// Prints the metric|type set and ok or FAIL against the 19.02 fork.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> node p15-statsd.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const lines = () => execFileSync('docker', ['exec', 'ejproof-stub', 'wget', '-qO-', 'http://127.0.0.1:8080/calls']).toString()
  .split('\n').filter(Boolean).map((l) => JSON.parse(l)).filter((c) => c.method === 'UDP').map((c) => c.body);
const FORK = 'chat.skillz.com: connected_users|g erlang_processes|g open_tcp_ports|g sm_register_connection|c sm_remove_connection|c user_receive_packet|c user_send_packet|c';

(async () => {
  const before = lines().length;
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: '1001', password: 'pw-1001', roomJid: `${Date.now() % 100000}@conference.chat.skillz.com`, historySize: 0, onEvent: () => {}});
  await sleep(1500);
  conn.send('metrics', {user_id: '1001', username: 'web1001', user_role: 0});
  await sleep(500);
  conn.close();
  await sleep(1000);
  const got = lines().slice(before);
  const bad = got.filter((l) => !/^[^/]+\/[^.]+\.\w+:\d+\|[cg]$/.test(l));
  const hosts = [...new Set(got.map((l) => l.split('/')[0]))].join(',');
  const set = `${hosts}: ${[...new Set(got.map((l) => l.replace(/^.*?\/[^.]*\./, '').replace(/:\d+\|/, '|')))].sort().join(' ')}`;
  console.log(`${set === FORK && !bad.length ? ' ok ' : 'FAIL'} | ${set}${bad.length ? ` | malformed: ${bad[0]}` : ''}${set === FORK ? '' : ` | fork: ${FORK}`}`);
  process.exit(set === FORK && !bad.length ? 0 : 1);
})();
setTimeout(() => process.exit(2), 15000);
