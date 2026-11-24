// P10 check: mod_block_incoming (1003 is on blocking_users in both configs) and mod_block_nonfriends
// (two-user DM rooms a-b only between friends). Prints ok or FAIL per case against the 19.02 fork.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> EJCTL=<ejabberdctl in the container> node p10-block.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const EJCTL = process.env.EJCTL || 'ejabberdctl';
const room = `${Date.now() % 100000 + 400000}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const ctl = (...a) => { try { return execFileSync('docker', ['exec', 'ejproof-ejabberd', EJCTL, ...a], {stdio: ['ignore', 'pipe', 'pipe']}).toString(); } catch (e) { return String(e.stderr); } };
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const user = (id, r = room) => {
  const raw = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: `${r}@conference.chat.skillz.com`, historySize: 0, onEvent: () => {}});
  const orig = conn.receive.bind(conn);
  conn.receive = (x) => { raw.push(x); orig(x); };
  return {conn, raw, has: (s) => raw.some((x) => x.includes(s))};
};
const results = [];
const expect = (name, got, want) => { results.push(got === want); console.log(`${got === want ? ' ok ' : 'FAIL'} | ${name} | ${got}${got === want ? '' : ` | fork: ${want}`}`); };

(async () => {
  for (const [a, b] of [['1001', '1003'], ['1001', '1002'], ['1003', '1001'], ['1001', '1004'], ['1004', '1001']]) ctl('delete_rosteritem', a, 'chat.skillz.com', b, 'chat.skillz.com');
  const [u1, u2, u3] = [user('1001'), user('1002'), user('1003')];
  await sleep(1500);
  // Available presence, so subscription requests are delivered live.
  for (const u of [u1, u2, u3]) u.conn.raw('<presence/>');
  await sleep(300);
  expect('get_blocking_users lists the configured user', (await post('get_blocking_users', {host: 'chat.skillz.com'})).includes('1003'), true);
  u1.conn.raw("<presence to='1003@chat.skillz.com' type='subscribe' id='sub3'/>");
  u1.conn.raw("<presence to='1002@chat.skillz.com' type='subscribe' id='sub2'/>");
  await sleep(1000);
  expect('friend request to a blocking user (not friends) is dropped', u3.has("type='subscribe'"), false);
  expect('friend request to a normal user is delivered', u2.has("type='subscribe'"), true);
  await post('remove_blocking_user', {host: 'chat.skillz.com', jid: '1003'});
  u1.conn.raw("<presence to='1003@chat.skillz.com' type='subscribe' id='sub3b'/>");
  await sleep(1000);
  expect('after remove_blocking_user the request is delivered', u3.has("type='subscribe'"), true);
  await post('add_blocking_user', {host: 'chat.skillz.com', jid: '1003'});

  // DM rooms (two ids joined by one dash): messages between non-friends are dropped at the sender's session.
  // Each room is destroyed after use: the fork does not restore a stopped room whose name fails regexp_room_id.
  const dmSend = async (to, name, text) => {
    await post('create_room', {name, service: 'conference.chat.skillz.com', host: 'chat.skillz.com'});
    const [a, b] = [user('1001', name), user(to, name)];
    await sleep(1500);
    a.conn.send(text, {user_id: '1001', username: 'web1001', user_role: 0});
    await sleep(800);
    a.conn.close();
    b.conn.close();
    await post('destroy_room', {name, service: 'conference.chat.skillz.com'});
    return b.has(text);
  };
  expect('DM room message between non-friends: dropped', await dmSend('1002', '1001-1002', 'dm non-friends'), false);
  ctl('add_rosteritem', '1001', 'chat.skillz.com', '1004', 'chat.skillz.com', '1004', 'friends', 'both');
  ctl('add_rosteritem', '1004', 'chat.skillz.com', '1001', 'chat.skillz.com', '1001', 'friends', 'both');
  expect('DM room message between friends: delivered', await dmSend('1004', '1004-1001', 'dm friends'), true);
  expect('3-id room name is not a DM: delivered', await dmSend('1002', `1001-1002-${room}`, 'dm three'), true);
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 30000);
