// P17 check: mod_pottymouth word masking, automod strikes (warning, quarantine, support ticket), the
// staff-impersonation filter and DM flagging. HTTP calls are captured by service-stub.js. Prints ok or
// FAIL per case against the 19.02 fork with mod_pottymouth (INFO: a known difference, printed only).
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> DB=<db> EJCTL=<ejabberdctl> node p17-pottymouth.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const DB = process.env.DB || 'ejabberd';
const MUC = 'conference.chat.skillz.com';
const room = `${Date.now() % 100000 + 200000}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const sh = (...a) => execFileSync('docker', a, {stdio: ['ignore', 'pipe', 'ignore']}).toString();
const sql = (q) => sh('exec', 'ejproof-mysql', 'mysql', '-uroot', '-proot', '-N', DB, '-e', q);
const ctl = (...a) => { try { sh('exec', 'ejproof-ejabberd', process.env.EJCTL || 'ejabberdctl', ...a); } catch { /* absent item */ } };
const calls = () => sh('exec', 'ejproof-stub', 'wget', '-qO-', 'http://127.0.0.1:8080/calls').split('\n').filter(Boolean).map((l) => JSON.parse(l));
// The fork writes support-ticket JSON by hand with raw newlines; parse both the same way, sorted keys.
const norm = (c) => {
  let b = c.body;
  try {
    const o = JSON.parse(b.replace(/\n/g, '\\n'));
    if (o.expiration_date) o.expiration_date = '<expiration>';
    b = JSON.stringify(Object.fromEntries(Object.keys(o).sort().map((k) => [k, o[k]])));
  } catch { /* not JSON */ }
  const h = c.headers['skillz-game-id'] ? ` game-header=${c.headers['skillz-game-id']}` : '';
  return `${c.port} ${c.method} ${c.path}${h} ${b}`.split(room).join('@ROOM@');
};
const web = (id, r = room) => {
  const events = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: `${r}@${MUC}`, historySize: 0, onEvent: (e) => events.push(e)});
  return {conn, events, send: (b, role = 0) => conn.send(b, {user_id: id, username: `web${id}`, user_role: role}),
    bodies: () => events.filter((e) => e.kind === 'message').map((e) => `${e.message.username}: ${e.message.body}`)};
};
// Recorded from the 19.02 fork image with mod_pottymouth (Dockerfile.fork-plugins), RECORD=1.
const FORK = {
  "masking + leet + automod warning (1 strike)": [
    "web1001: you **** **** ok",
    "Skillz Admin: @Offender, profanity or the use of numbers, letters, or symbols to disguise such words is not permitted in chat."
  ],
  "mute-list word at 1+2 strikes: quarantine + ticket": [
    "web1001: such an ****",
    "80 POST /chat/quarantines {\"announce_in_chat_game_id\":\"@ROOM@\",\"comment\":\"1001 was muted by the Auto-moderator for posting asshat in the public game room for game ID @ROOM@. This Player had 1 strikes prior to this offense\",\"expiration_date\":\"<expiration>\",\"moderator_id\":\"105432430\",\"moderator_username\":\"auto_moderator\",\"reason\":\"public_room_mute\",\"status\":\"MUTE\",\"user_id\":\"1001\"}",
    "9100 POST /communication/v1/support_tickets {\"game_id\":\"@ROOM@\",\"message\":\"1001 was muted by the Auto-moderator for posting \\\"asshat\\\" in the public game room for game ID @ROOM@. \\n Whole message: \\\"such an asshat\\\" \\n This Player had 1 strikes prior to this offense.\",\"subject\":\"Inappropriate Language\",\"user_id\":\"105432430\"}"
  ],
  "staff role from a non-staff user: dropped, ban + ticket": [
    "80 POST /chat/quarantines {\"comment\":\"1001 was banned by the Auto-moderator for Staff in the public game room for game ID @ROOM@. Whole message: i am staff\",\"expiration_date\":\"<expiration>\",\"moderator_id\":\"105432430\",\"moderator_username\":\"auto_moderator\",\"reason\":\"StaffImpersonationAttempt\",\"status\":\"BAN\",\"user_id\":\"1001\"}",
    "80 POST /profile/show {\"current_user_id\":1001,\"target_user_id\":1001}",
    "9100 POST /communication/v1/support_tickets {\"game_id\":\"@ROOM@\",\"message\":\"1001 was banned by the Auto-moderator for Staff in the public game room for game ID @ROOM@. Whole message: i am staff\",\"subject\":\"StaffImpersonationAttempt\",\"user_id\":\"105432430\"}"
  ],
  "staff role from staff (1002): delivered": [
    "web1002: real staff",
    "80 POST /profile/show {\"current_user_id\":1002,\"target_user_id\":1002}"
  ],
  "accented and emoji words": [
    "web1001: café **** 😀",
    "80 POST /chat/quarantines {\"announce_in_chat_game_id\":\"@ROOM@\",\"comment\":\"1001 was muted by the Auto-moderator for posting arse in the public game room for game ID @ROOM@. This Player had 3 strikes prior to this offense\",\"expiration_date\":\"<expiration>\",\"moderator_id\":\"105432430\",\"moderator_username\":\"auto_moderator\",\"reason\":\"public_room_mute\",\"status\":\"MUTE\",\"user_id\":\"1001\"}",
    "9100 POST /communication/v1/support_tickets {\"game_id\":\"@ROOM@\",\"message\":\"1001 was muted by the Auto-moderator for posting \\\"arse\\\" in the public game room for game ID @ROOM@. \\n Whole message: \\\"café arse 😀\\\" \\n This Player had 3 strikes prior to this offense.\",\"subject\":\"Inappropriate Language\",\"user_id\":\"105432430\"}"
  ],
  "DM private_ban word: support ticket": [
    "web1001: free reward here",
    "9100 POST /communication/v1/support_tickets {\"game_id\":null,\"message\":\"1001 was flagged by the Auto-moderator for posting \\\"reward\\\" in a Direct Message with 1004\\n Whole message: \\\"free reward here\\\"\",\"subject\":\"Report Abuse\",\"user_id\":\"105432430\"}"
  ],
};

(async () => {
  sql('create table if not exists user_warning (id bigint not null auto_increment primary key, comment text, date_created datetime,'
    + ' enabled bit default 1, expiration_date datetime, last_updated datetime, moderator_user_id bigint, reason varchar(255),'
    + ' strikes int, user_id bigint)');
  sql('delete from user_warning where user_id = 1001');
  // The fork posts the warning only for a user with a vCard nickname (no vCard: the hook crashes).
  ctl('set_vcard', '1001', 'chat.skillz.com', 'NICKNAME', 'Offender');
  for (const [a, b] of [['1001', '1004'], ['1004', '1001']]) ctl('add_rosteritem', a, 'chat.skillz.com', b, 'chat.skillz.com', b, 'friends', 'both');
  await post('create_room', {name: room, service: MUC, host: 'chat.skillz.com'});
  const [offender, observer] = [web('1001'), web('1004')];
  await sleep(1500);
  const results = [];
  // Room messages in order, then the HTTP calls sorted (they are posted asynchronously).
  const check = async (name, act, wait = 1500, obs = observer) => {
    const [c0, b0] = [calls().length, obs.bodies().length];
    await act();
    await sleep(wait);
    const got = [...obs.bodies().slice(b0), ...calls().slice(c0).map(norm).sort()].map((s) => s.split(room).join('@ROOM@'));
    if (process.env.RECORD) console.log(`RECORD ${JSON.stringify([name, got])}`);
    const want = FORK[name];
    const ok = want === undefined || JSON.stringify(got) === JSON.stringify(want);
    results.push(ok);
    console.log(`${want === undefined ? 'INFO' : ok ? ' ok ' : 'FAIL'} | ${name}\n    ${got.join('\n    ') || '(nothing)'}${ok ? '' : `\n  fork:\n    ${want.join('\n    ')}`}`);
  };
  await check('masking + leet + automod warning (1 strike)', () => offender.send('you arse  @rse ok'), 2500);
  await check('mute-list word at 1+2 strikes: quarantine + ticket', () => offender.send('such an asshat'));
  await check('staff role from a non-staff user: dropped, ban + ticket', () => offender.send('i am staff', 1));
  const staff = web('1002');
  await sleep(1500);
  await check('staff role from staff (1002): delivered', () => staff.send('real staff', 1));
  await check('accented and emoji words', () => offender.send('café arse 😀'), 2500);
  const dmRoom = '1001-1004';
  await post('create_room', {name: dmRoom, service: MUC, host: 'chat.skillz.com'});
  const [dmA, dmB] = [web('1001', dmRoom), web('1004', dmRoom)];
  await sleep(1500);
  await check('DM private_ban word: support ticket', () => dmA.send('free reward here'), 1500, dmB);
  await post('destroy_room', {name: dmRoom, service: MUC});
  console.log(`user_warning rows for 1001: ${sql('select strikes from user_warning where user_id = 1001 order by id').trim().split('\n').join(',')}`);
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 60000);
