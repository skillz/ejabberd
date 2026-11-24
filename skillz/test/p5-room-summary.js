// P5 check: GET /api/get_room_summary?room&limit&last_message_id as chat-broadcast-service's spec and the
// public Istio route call it. Prints ok or FAIL per case against the 19.02 fork.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<sdk-core/js/node_modules>:<portal-play/node_modules> node p5-room-summary.js
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const {mk} = require('./compat/native-smoke');
const API = process.env.API || 'http://127.0.0.1:15443/api';
const room = `${Date.now() % 100000 + 100000}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const summary = async (q) => {
  const r = await fetch(`${API}/get_room_summary?${new URLSearchParams(q)}`);
  return {status: r.status, list: r.status === 200 ? await r.json() : await r.text()};
};
const show = ({status, list}) => (status === 200 ? list.map((m) => `${m.body}|${m.from}|${m.user_role}|${m.avatar_url}`).join(' ; ') || '[]' : `${status} ${list}`);
const FORK = {
  'limit 10, no last id': 'w1|web1001|0| ; w2|web1001|0| ; w3|web1003|2| ; n1|user1002|0|https://x/a.png',
  'limit 2': 'w3|web1003|2| ; n1|user1002|0|https://x/a.png',
  'empty limit (default 5)': 'w1|web1001|0| ; w2|web1001|0| ; w3|web1003|2| ; n1|user1002|0|https://x/a.png',
  'after the 2nd message': 'w3|web1003|2| ; n1|user1002|0|https://x/a.png',
  'last id is the newest message': 'n1|user1002|0|https://x/a.png',
  'limit 0': '[]',
  'room that does not exist': '[]',
};

(async () => {
  const web = (id) => new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: `${room}@conference.chat.skillz.com`, historySize: 0, onEvent: () => {}});
  // 1003 is a chat moderator in service-stub.js, so mod_pottymouth lets its user_role 2 through.
  const [w1, w3] = [web('1001'), web('1003')];
  const native = await new Promise((res) => { const c = mk('1002'); c.connectToChat(() => res(c)); });
  await sleep(1500);
  for (const [c, id, b, role] of [[w1, '1001', 'w1', 0], [w1, '1001', 'w2', 0], [w3, '1003', 'w3', 2]]) {
    c.send(b, {user_id: id, username: `web${id}`, user_role: role});
    await sleep(500);
  }
  await new Promise((res) => native.connectToRoomWithCompletion(room, 0, res));
  native.sendMessage('n1', room);
  await sleep(1000);
  const all = await summary({room, limit: '10', last_message_id: ''});
  const ids = all.status === 200 ? all.list.map((m) => m.id) : [];
  const results = [];
  for (const [name, q] of [
    ['limit 10, no last id', {room, limit: '10', last_message_id: ''}],
    ['limit 2', {room, limit: '2', last_message_id: ''}],
    ['empty limit (default 5)', {room, limit: '', last_message_id: ''}],
    ['after the 2nd message', {room, limit: '10', last_message_id: ids[1] || 'none'}],
    ['last id is the newest message', {room, limit: '10', last_message_id: ids[3] || 'none'}],
    ['limit 0', {room, limit: '0', last_message_id: ''}],
    ['room that does not exist', {room: `${room}9`, limit: '10', last_message_id: ''}],
  ]) {
    const got = show(await summary(q));
    results.push(got === FORK[name]);
    console.log(`${got === FORK[name] ? ' ok ' : 'FAIL'} | ${name} | ${got}${got === FORK[name] ? '' : ` | fork: ${FORK[name]}`}`);
  }
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 30000);
