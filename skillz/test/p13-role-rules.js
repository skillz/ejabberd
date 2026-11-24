// P13 check: the fork's MUC role/affiliation rule relaxations, in a game room (admin/owner granted with
// set_room_affiliation: the fork's global table, per room on 26.09). The port keeps stock 26.09 rules on
// purpose: on the fork an API-granted admin is stored as `none` (so admin-only relaxations never apply),
// and the catch-all `true` lets any participant ban anyone globally. Each row shows both results;
// TARGET=fork checks the fork column.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<portal-play/node_modules> [TARGET=fork] node p13-role-rules.js
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const {RoomConnection} = require(process.env.WEB_BUNDLE);
const API = process.env.API || 'http://127.0.0.1:15443/api';
const MUC = 'conference.chat.skillz.com';
const room = `${Date.now() % 100000 + 50000}`;
const jid = `${room}@${MUC}`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const user = (id) => {
  const raw = [];
  const conn = new RoomConnection({wsUrl: 'ws://127.0.0.1:15280/websocket', domain: 'chat.skillz.com', userId: id, password: `pw-${id}`, roomJid: jid, historySize: 0, onEvent: () => {}});
  const orig = conn.receive.bind(conn);
  conn.receive = (x) => { raw.push(x); orig(x); };
  const iq = async (id2, type, body) => {
    conn.raw(`<iq type="${type}" id="${id2}" to="${jid}">${body}</iq>`);
    await sleep(700);
    const r = raw.find((x) => x.includes(`id='${id2}'`)) || '';
    return r.includes("type='result'") ? 'result' : (r.match(/<(forbidden|not-allowed|bad-request|item-not-found|not-acceptable)/) || ['', r ? 'error' : 'no reply'])[1];
  };
  return {iq};
};
const OWNER_FORM = '<query xmlns="http://jabber.org/protocol/muc#owner"/>';
const setAff = (who, aff) => `<query xmlns="http://jabber.org/protocol/muc#admin"><item affiliation="${aff}" jid="${who}@chat.skillz.com"/></query>`;
const setRole = (nick, role) => `<query xmlns="http://jabber.org/protocol/muc#admin"><item role="${role}" nick="${nick}"/></query>`;
// [fork, port]
const CASES = {
  'admin reads the room config (owner IQ get)': ['forbidden', 'forbidden'],
  'admin submits the room config (owner IQ set)': ['forbidden', 'forbidden'],
  'admin demotes an owner to member': ['result', 'not-allowed'],
  'admin kicks a participant (role none)': ['not-allowed', 'result'],
  'participant bans another participant (affiliation outcast)': ['result', 'not-allowed'],
  'participant demotes the room owner': ['result', 'not-allowed'],
};

(async () => {
  await post('create_room', {name: room, service: MUC, host: 'chat.skillz.com'});
  for (const [u, a] of [['1001', 'admin'], ['1004', 'owner']]) {
    await post('set_room_affiliation', {name: room, service: MUC, jid: `${u}@chat.skillz.com`, affiliation: a});
  }
  const [admin, part] = [user('1001'), user('1002'), user('1003'), user('1004')];
  await sleep(1800);
  const results = [];
  const col = process.env.TARGET === 'fork' ? 0 : 1;
  const row = (name, got) => {
    const [fork, port] = CASES[name];
    results.push(got === CASES[name][col]);
    console.log(`${got === CASES[name][col] ? ' ok ' : 'FAIL'} | ${name} | ${got} | fork ${fork}, port ${port}${fork === port ? '' : ' (differs on purpose)'}`);
  };
  row('admin reads the room config (owner IQ get)', await admin.iq('o1', 'get', OWNER_FORM));
  row('admin submits the room config (owner IQ set)', await admin.iq('o2', 'set', '<query xmlns="http://jabber.org/protocol/muc#owner"><x xmlns="jabber:x:data" type="submit"><field var="FORM_TYPE"><value>http://jabber.org/protocol/muc#roomconfig</value></field><field var="muc#roomconfig_roomname"><value>P13</value></field></x></query>'));
  row('admin demotes an owner to member', await admin.iq('a1', 'set', setAff('1004', 'member')));
  row('admin kicks a participant (role none)', await admin.iq('r1', 'set', setRole('1003', 'none')));
  row('participant bans another participant (affiliation outcast)', await part.iq('p1', 'set', setAff('1003', 'outcast')));
  row('participant demotes the room owner', await part.iq('p2', 'set', setAff('1004', 'none')));
  // Undo what the fork let through: its outcast is global.
  await post('set_user_affiliation', {host: 'chat.skillz.com', user: '1003', affiliation: 'none'});
  process.exit(results.every(Boolean) ? 0 : 1);
})();
setTimeout(() => process.exit(2), 30000);
