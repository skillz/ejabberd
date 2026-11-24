// P4 check: join history of a persistent room after a server restart (fork loads it from MAM on
// room start). Phase "post" writes a message; phase "join" (after restart) prints the history size.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> node p4-history-after-restart.js post|join <room>
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const [phase, room] = process.argv.slice(2);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
(async () => {
  const events = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: '1001', password: 'pw-1001', roomJid: `${room}@conference.chat.skillz.com`, historySize: 50, onEvent: (e) => events.push(e)});
  await sleep(1500);
  if (phase === 'post') {
    conn.send('before restart', {user_id: '1001', username: 'web1001', user_role: 0});
    await sleep(500);
    console.log(`posted to ${room}, joined=${events.some((e) => e.kind === 'joined')}`);
  } else {
    const hist = events.find((e) => e.kind === 'history')?.messages ?? [];
    console.log(`join history after restart=${hist.length} has old message=${hist.some((m) => m.body === 'before restart')}`);
  }
  process.exit(0);
})();
setTimeout(() => process.exit(2), 10000);
