// P3 check: after the delete-unused-rooms cron destroys a game room, is its archive kept, and can
// players re-create the room by joining (with its history)? Prints one line; no pass/fail.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> node p3-destroy-recreate.js
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const room = `${Date.now() % 100000 + 900000}`;
const jid = `${room}@conference.chat.skillz.com`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const join = async () => {
  const events = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: '1001', password: 'pw-1001', roomJid: jid, historySize: 50, onEvent: (e) => events.push(e)});
  await sleep(1500);
  return {conn, events};
};
(async () => {
  const a = await join();
  a.conn.send('before destroy', {user_id: '1001', username: 'web1001', user_role: 0});
  await sleep(500);
  a.conn.close();
  await sleep(300);
  const d = await fetch(`${API}/destroy_room`, {method: 'POST', body: JSON.stringify({name: room, service: 'conference.chat.skillz.com'})}).then((r) => r.text());
  await sleep(500);
  const b = await join();
  const joined = b.events.some((e) => e.kind === 'joined');
  const err = b.events.find((e) => e.kind === 'join-error');
  const hist = b.events.find((e) => e.kind === 'history')?.messages.length ?? 0;
  const page = joined ? await b.conn.older(new Date(Date.now() + 60000)).catch(() => ({messages: []})) : {messages: []};
  console.log(`destroy=${d.trim()} | rejoin: ${joined ? 'joined' : `error ${err?.condition}`} | join history=${hist} | MAM keeps old message=${page.messages.some((m) => m.body === 'before destroy')}`);
  process.exit(0);
})();
setTimeout(() => process.exit(2), 15000);
