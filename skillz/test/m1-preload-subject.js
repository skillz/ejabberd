// Migration-lane findings: (1) persistent rooms are not started at boot (fork P3; 26.09 needs
// preload_rooms: false); (2) a room subject change is kept out of the MAM archive by
// mod_muc_subject_unarchived. The fork archives it too, and then crashes when it restarts that room
// (el_to_unarchived_msg on the body-less row), so that case differs on purpose. Restarts the server
// in place. Prints [fork, port] per case; TARGET=fork checks the fork column.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> DB=<db> EJCTL=<ejabberdctl> node m1-preload-subject.js
const {execFileSync} = require('child_process');
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const DB = process.env.DB || 'ejabberd';
const EJCTL = process.env.EJCTL || 'ejabberdctl';
const MUC = 'conference.chat.skillz.com';
const room = `${Date.now() % 100000 + 60000}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const sh = (...a) => { try { return execFileSync('docker', a, {stdio: ['ignore', 'pipe', 'ignore']}).toString(); } catch (e) { return String(e.stdout || ''); } };
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const web = (id) => {
  const events = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: `${room}@${MUC}`, historySize: 20, onEvent: (e) => events.push(e)});
  return {conn, events};
};
const results = [];
const col = process.env.TARGET === 'fork' ? 0 : 1;
const expect = (name, got, [fork, port]) => {
  const ok = got === [fork, port][col];
  results.push(ok);
  console.log(`${ok ? ' ok ' : 'FAIL'} | ${name} | ${got} | fork ${fork}, port ${port}${fork === port ? '' : ' (differs on purpose)'}`);
};

(async () => {
  await post('create_room', {name: room, service: MUC, host: 'chat.skillz.com'});
  await post('set_room_affiliation', {name: room, service: MUC, jid: 'mod1@chat.skillz.com', affiliation: 'owner'});
  const owner = web('mod1');
  await sleep(1500);
  owner.conn.send('before pin', {user_id: 'mod1', username: 'mod1', user_role: 0});
  await sleep(500);
  owner.conn.raw(`<message type="groupchat" to="${room}@${MUC}" id="pin1"><subject>Pinned m1</subject></message>`);
  await sleep(800);
  const rows = sh('exec', 'ejproof-mysql', 'mysql', '-uroot', '-proot', '-N', DB, '-e',
    `select count(*) from archive where username = '${room}@${MUC}' and xml not like '%<body%'`).trim();
  expect('body-less archive rows after a subject change', rows, ['1', '0']);
  const joiner = web('1002');
  await sleep(1500);
  const subj = joiner.events.find((e) => e.kind === 'subject');
  expect('joiner gets history then the subject', `${joiner.events.find((e) => e.kind === 'history')?.messages.length}|${subj?.text}`, ['1|Pinned m1', '1|Pinned m1']);
  owner.conn.close();
  joiner.conn.close();
  sh('exec', 'ejproof-ejabberd', EJCTL, 'restart');
  await sleep(3000);
  for (let i = 0; i < 60 && !sh('exec', 'ejproof-ejabberd', EJCTL, 'status').includes('is started'); i++) await sleep(1000);
  await sleep(1000);
  const online = sh('exec', 'ejproof-ejabberd', EJCTL, 'muc_online_rooms', 'global');
  expect('after a restart the room is not running until used', `${online.includes(room)} (status ${sh('exec', 'ejproof-ejabberd', EJCTL, 'status').includes('is started') ? 'up' : 'down'})`, ['false (status up)', 'false (status up)']);
  const back = web('1003');
  await sleep(2500);
  const joined = back.events.some((e) => e.kind === 'joined');
  expect('rejoin after the restart (room rebuilt from the archive)', `${joined ? 'joined' : 'no join'}|${back.events.find((e) => e.kind === 'history')?.messages.length ?? '-'}`, ['no join|-', 'joined|1']);
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 120000);
