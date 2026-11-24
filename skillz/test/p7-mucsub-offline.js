// P7 check: what a MUC/Sub subscriber who is not in the room gets, per sender kind. Prints one line
// per case (live event / stored offline / room still relays) and ok or FAIL against the 19.02 fork.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<sdk-core/js/node_modules>:<portal-play/node_modules> DB=<db> node p7-mucsub-offline.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const {mk} = require('./compat/native-smoke');
const API = process.env.API || 'http://127.0.0.1:15443/api';
const DB = process.env.DB || 'ejabberd';
const MUC = 'conference.chat.skillz.com';
const room = `${Date.now() % 100000 + 500000}`;
const jid = `${room}@${MUC}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const sql = (q) => execFileSync('docker', ['exec', 'ejproof-mysql', 'mysql', '-uroot', '-proot', '-N', DB, '-e', q], {stdio: ['ignore', 'pipe', 'ignore']}).toString();
const spool = () => sql("select xml from spool where username='1004'");
const web = (id) => {
  const events = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: jid, historySize: 0, onEvent: (e) => events.push(e)});
  const me = {user_id: id, username: `web${id}`, user_role: 0};
  return {conn, events, send: (b, o) => conn.send(b, me, o), got: (b) => events.some((e) => e.kind === 'message' && e.message.body === b)};
};
const native = (id) => new Promise((res) => { const c = mk(id); const raw = []; c.client.on('raw:incoming', (x) => raw.push(x)); c.connectToChat(() => res({c, raw})); });

// 19.02 fork results: a subscriber not in the room gets the message in offline storage (so push), even
// when online; a message whose skillz_sdk user_id is the subscriber goes live instead. A groupchat
// with fewer than 3 children (a chat state) crashes the fork's room process (lists:nth/2); the port
// relays it, which is the one intended difference.
const CRASH = 'live=false stored=false relayed=false';
const FORK = {
  'native post': 'live=false stored=true relayed=true',
  'web post (skillz_sdk 1st)': 'live=false stored=true relayed=true',
  'web correction (replace, skillz_sdk 2nd)': 'live=false stored=true relayed=true',
  'subscriber posts without joining': 'live=true stored=false relayed=true',
  'web chat state (no skillz_sdk)': 'live=false stored=false relayed=true',
};

(async () => {
  await post('create_room', {name: room, room, service: MUC, host: 'chat.skillz.com'});
  await post('change_room_option', {name: room, room, service: MUC, option: 'allow_subscription', value: 'true'});
  sql("delete from spool where username='1004'");
  const sub = await native('1004');
  await new Promise((res) => sub.c.client.subscribeToRoom(jid, '1004@chat.skillz.com', res));
  const sender = await native('1002');
  await new Promise((res) => sender.c.connectToRoomWithCompletion(room, 0, res));
  const w = web('1001');
  const obs = web('1003');
  await sleep(1500);
  let n = 0;
  const check = async (name, body, send) => {
    const marker = body || `probe-${++n}`;
    send();
    await sleep(900);
    // A probe after each case shows whether the room process survived it.
    const probe = `alive-${++n}`;
    sender.c.sendMessage(probe, room);
    await sleep(900);
    const result = `live=${sub.raw.some((x) => x.includes(marker))} stored=${spool().includes(marker)} relayed=${obs.got(probe)}`;
    const verdict = result === FORK[name] ? ' ok ' : result === CRASH ? 'CRASH' : 'FAIL';
    console.log(`${verdict} | ${name} | ${result}${verdict === ' ok ' ? '' : ` | expected: ${FORK[name]}`}`);
  };
  await check('native post', 'p7 native', () => sender.c.sendMessage('p7 native', room));
  await check('web post (skillz_sdk 1st)', 'p7 web', () => w.send('p7 web'));
  await check('web correction (replace, skillz_sdk 2nd)', 'p7 web fix', () => w.send('p7 web fix', {replaceId: 'w-x'}));
  await check('subscriber posts without joining', 'p7 own', () => sub.c.sendMessage('p7 own', room));
  await check('web chat state (no skillz_sdk)', 'composing', () => w.conn.raw(`<message type="groupchat" to="${jid}" id="cs-1"><composing xmlns="http://jabber.org/protocol/chatstates"/></message>`));
  process.exit(0);
})();
setTimeout(() => process.exit(2), 40000);
