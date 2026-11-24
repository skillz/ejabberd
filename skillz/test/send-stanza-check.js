// What occupants get when a service posts into a game room with send_stanza (chat-broadcast-service,
// user-details-service mute notices), and whether it is archived. Prints one line; no pass/fail.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> node send-stanza-check.js
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const room = `${Date.now() % 100000 + 700000}`;
const jid = `${room}@conference.chat.skillz.com`;
const raw = [];
const events = [];
const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: '1001', password: 'pw-1001', roomJid: jid, historySize: 50, onEvent: (e) => events.push(e)});
const orig = conn.receive.bind(conn);
conn.receive = (x) => { raw.push(x); orig(x); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
(async () => {
  await sleep(1500);
  const stanza = `<message type='groupchat' to='${jid}' id='svc1'><body>service broadcast</body><skillz_sdk xmlns='xmpp:skillz'><message_type>0</message_type><user_id>0</user_id><username>Skillz</username></skillz_sdk></message>`;
  const r = await fetch(`${API}/send_stanza`, {method: 'POST', body: JSON.stringify({from: 'skillz-cas@chat.skillz.com', to: jid, stanza})});
  const body = await r.text();
  await sleep(1000);
  const got = raw.find((x) => x.includes('service broadcast'));
  const page = await conn.older(new Date(Date.now() + 60000)).catch((e) => ({messages: [], err: String(e)}));
  console.log(`send_stanza=${r.status} ${body.trim()} | occupant got it: ${Boolean(got)} from=${got ? got.match(/from='([^']+)'/)?.[1] : '-'} | in MAM: ${page.messages.some((m) => m.body === 'service broadcast')} ${page.err || ''}`);
  process.exit(0);
})();
setTimeout(() => process.exit(2), 15000);
