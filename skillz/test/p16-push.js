// P16 check: the push-notification-service calls (POST /v1/triggered_events, captured by service-stub.js)
// for MUC/Sub messages to an offline subscriber and for friend requests. Prints ok or FAIL per case
// against the 19.02 fork with ModPushSkillz.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<sdk-core/js/node_modules>:<portal-play/node_modules> EJCTL=<ejabberdctl in the container> node p16-push.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const {mk} = require('./compat/native-smoke');
const API = process.env.API || 'http://127.0.0.1:15443/api';
const MUC = 'conference.chat.skillz.com';
const room = `${Date.now() % 100000 + 300000}`;
const jid = `${room}@${MUC}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const pushes = () => execFileSync('docker', ['exec', 'ejproof-stub', 'wget', '-qO-', 'http://127.0.0.1:8080/calls']).toString()
  .split('\n').filter(Boolean).map((l) => JSON.parse(l)).filter((c) => c.path === '/v1/triggered_events').map((c) => c.body);
const web = (id) => {
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: jid, historySize: 0, onEvent: () => {}});
  return {conn, send: (b, o) => conn.send(b, {user_id: id, username: `web${id}`, user_role: 0}, o)};
};
// Sorted keys, so the fork's and the port's JSON compare as text.
const norm = (body) => { try { const o = JSON.parse(body); return JSON.stringify(Object.fromEntries(Object.keys(o).sort().map((k) => [k, o[k]]))); } catch { return `raw:${body}`; } };
const sdkMsg = (type, data) => `<message type="groupchat" to="${jid}" id="t${type}-${Date.now()}"><body>invite</body><skillz_sdk xmlns="xmpp:skillz"><message_type>${type}</message_type><user_id>1001</user_id><username>web1001</username><data>${data}</data></skillz_sdk></message>`;
const R = '@ROOM@';
// Recorded from the 19.02 fork image with ModPushSkillz (Dockerfile.fork-plugins).
const FORK = {
  'native chat to offline subscriber': [`{"chat_message_text":"p16 chat","from_user_id":1002,"muc_jid":"${R}","trigger_category":9,"trigger_condition":137,"user_ids":[1004]}`],
  'native chat longer than 200 characters': [`{"chat_message_text":"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxéé","from_user_id":1002,"muc_jid":"${R}","trigger_category":9,"trigger_condition":137,"user_ids":[1004]}`],
  'web chat (client sends skillz_sdk 1st)': [`{"chat_message_text":"p16 web","from_user_id":1001,"muc_jid":"${R}","trigger_category":9,"trigger_condition":137,"user_ids":[1004]}`],
  'web correction (replace before skillz_sdk)': [`{"chat_message_text":"","from_user_id":1001,"muc_jid":"${R}","trigger_category":9,"trigger_condition":137,"user_ids":[1004]}`],
  'VsFriends invite (message_type 1)': ['{"game_id":"42","inviting_players_name":"Host","tournament_prize":"$5","trigger_category":10,"trigger_condition":142,"user_ids":[1004]}'],
  'Nudge (message_type 3)': ['{"game_id":"42","inviting_players_name":"web1001","recipient_player_name":"Rec","tournament_prize":null,"trigger_category":10,"trigger_condition":159,"user_ids":[1004]}'],
  'message_type 2 (no push)': [],
  'friend request 1001 -> 1002': ['{"chat_message_text":"","from_user_id":1001,"trigger_category":9,"trigger_condition":138,"user_ids":[1002]}'],
  'friend request 1001 -> 1002 again': [],
  'friend request to a blocking user (1003)': [],
  '1004 requests 1001, 1001 accepts': ['{"chat_message_text":"","from_user_id":1004,"trigger_category":9,"trigger_condition":138,"user_ids":[1001]}', '{"chat_message_text":"","from_user_id":1001,"trigger_category":9,"trigger_condition":139,"user_ids":[1004]}'],
};

(async () => {
  for (const [a, b] of [['1001', '1002'], ['1002', '1001'], ['1001', '1003'], ['1003', '1001'], ['1001', '1004'], ['1004', '1001']]) {
    execFileSync('docker', ['exec', 'ejproof-ejabberd', process.env.EJCTL || 'ejabberdctl', 'delete_rosteritem', a, 'chat.skillz.com', b, 'chat.skillz.com'], {stdio: 'ignore'});
  }
  await post('create_room', {name: room, service: MUC, host: 'chat.skillz.com'});
  await post('change_room_option', {name: room, service: MUC, option: 'allow_subscription', value: 'true'});
  const sub = await new Promise((res) => { const c = mk('1004'); c.connectToChat(() => res(c)); });
  await new Promise((res) => sub.client.subscribeToRoom(jid, '1004@chat.skillz.com', res));
  sub.disconnectFromServer();
  const sender = await new Promise((res) => { const c = mk('1002'); c.connectToChat(() => res(c)); });
  await new Promise((res) => sender.connectToRoomWithCompletion(room, 0, res));
  const w = web('1001');
  await sleep(1500);
  const results = [];
  const check = async (name, act) => {
    const before = pushes().length;
    await act();
    await sleep(1000);
    const got = pushes().slice(before).map(norm).map((s) => s.split(room).join(R));
    const want = FORK[name];
    const ok = want === undefined || JSON.stringify(got) === JSON.stringify(want);
    results.push(ok);
    console.log(`${want === undefined ? 'INFO' : ok ? ' ok ' : 'FAIL'} | ${name} | ${got.join(' ') || 'no push'}${ok ? '' : ` | fork: ${want.join(' ') || 'no push'}`}`);
  };
  await check('native chat to offline subscriber', () => sender.sendMessage('p16 chat', room));
  await check('native chat longer than 200 characters', () => sender.sendMessage(`${'x'.repeat(198)}éé${'y'.repeat(50)}`, room));
  await check('web chat (client sends skillz_sdk 1st)', () => w.send('p16 web'));
  await check('web correction (replace before skillz_sdk)', () => w.send('p16 web fix', {replaceId: 'w-x'}));
  await check('VsFriends invite (message_type 1)', () => w.conn.raw(sdkMsg(1, '{&quot;gameId&quot;:&quot;42&quot;,&quot;tournamentPrize&quot;:&quot;$5&quot;,&quot;hostContactName&quot;:&quot;Host&quot;}')));
  await sleep(500);
  await check('Nudge (message_type 3)', () => w.conn.raw(sdkMsg(3, '{&quot;gameId&quot;:&quot;42&quot;,&quot;recipientPlayerName&quot;:&quot;Rec&quot;}')));
  await sleep(500);
  await check('message_type 2 (no push)', () => w.conn.raw(sdkMsg(2, '{}')));
  // Friend requests: the fork alternates pushes per pair (double_push table) and skips blocking users (mod_block_incoming).
  await check('friend request 1001 -> 1002', () => w.conn.raw("<presence to='1002@chat.skillz.com' type='subscribe'/>"));
  await check('friend request 1001 -> 1002 again', () => w.conn.raw("<presence to='1002@chat.skillz.com' type='subscribe'/>"));
  await check('friend request to a blocking user (1003)', () => w.conn.raw("<presence to='1003@chat.skillz.com' type='subscribe'/>"));
  // 26.09 drops a `subscribed` with no pending request (RFC 6121 pre-approval); the fork passed it on.
  const w4 = web('1004');
  await sleep(1500);
  await check('1004 requests 1001, 1001 accepts', async () => {
    w4.conn.raw("<presence to='1001@chat.skillz.com' type='subscribe'/>");
    await sleep(800);
    w.conn.raw("<presence to='1004@chat.skillz.com' type='subscribed'/>");
  });
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 40000);
