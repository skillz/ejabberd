// Read checks on a migrated dump, with raw XMPP over WebSocket so the same script runs on 26.09 and the
// 19.02 fork. Users are existing JIDs of the dump, registered locally (Mnesia) with password pw-<id>.
// usage: node verify.js read | write | read-written
//   env: USER_ID (roster/privacy/vCard/MUC-Sub user), ROOM (game room with history), CLUB (club room),
//        MUTED, OUTCAST (enabled user_affiliation rows), VHOST, WS, API
const WS = process.env.WS || 'ws://127.0.0.1:15280/websocket';
const API = process.env.API || 'http://127.0.0.1:15443/api';
const D = process.env.VHOST || 'chat.staging.skillz.com';
const MUC = `conference.${D}`;
const {USER_ID, ROOM, CLUB, MUTED, OUTCAST} = process.env;
const NEW_ROOM = '990001-club-migtest';
const MARK = 'migration-check-26';
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const out = (name, value) => console.log(`${name} | ${value}`);
const count = (xml, re) => (xml.match(re) || []).length;

class Xmpp {
  constructor(user) { this.user = user; this.frames = []; this.n = 0; }
  async connect() {
    this.ws = new WebSocket(WS, 'xmpp');
    this.ws.onmessage = (e) => this.frames.push(String(e.data));
    await new Promise((res, rej) => { this.ws.onopen = res; this.ws.onerror = rej; });
    await this.open();
    this.send(`<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>${Buffer.from(`\0${this.user}\0pw-${this.user}`).toString('base64')}</auth>`);
    const r = await this.wait((f) => /<success|<failure/.test(f));
    if (!r.includes('<success')) throw new Error(`auth ${this.user}: ${r}`);
    await this.open();
    await this.iq('set', '', `<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>mig</resource></bind>`);
    await this.iq('set', '', `<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>`);
    return this;
  }
  async open() {
    const from = this.frames.length;
    this.send(`<open xmlns='urn:ietf:params:xml:ns:xmpp-framing' to='${D}' version='1.0'/>`);
    await this.wait((f) => f.includes('<stream:features') || f.includes('<features'), 5000, from);
  }
  send(x) { this.ws.send(x); }
  async wait(pred, ms = 5000, from = 0) {
    for (let t = 0; t < ms; t += 25) {
      const f = this.frames.slice(from).find(pred);
      if (f) return f;
      await sleep(25);
    }
    throw new Error(`timeout waiting (${this.user})`);
  }
  async iq(type, to, body) {
    const id = `q${++this.n}`;
    this.send(`<iq type='${type}' id='${id}'${to ? ` to='${to}'` : ''}>${body}</iq>`);
    return this.wait((f) => f.startsWith('<iq') && (f.includes(`id='${id}'`) || f.includes(`id="${id}"`)), 15000);
  }
  // Joins with history; returns the history frames up to and including the subject (or a timeout).
  async join(room, maxstanzas = 20) {
    const from = this.frames.length;
    this.send(`<presence to='${room}@${MUC}/${this.user}'><x xmlns='http://jabber.org/protocol/muc'><history maxstanzas='${maxstanzas}'/></x></presence>`);
    const end = await this.wait((f) => /<subject/.test(f) || (f.startsWith('<presence') && f.includes("type='error'")) || (f.startsWith('<presence') && f.includes('type="error"')), 8000, from).catch(() => 'timeout');
    return {end, frames: this.frames.slice(from)};
  }
  close() { try { this.send(`<close xmlns='urn:ietf:params:xml:ns:xmpp-framing'/>`); this.ws.close(); } catch {} }
}

// get_room_options is an object on the fork and a [{name, value}] list on 26.09.
const opts = (res) => {
  const j = JSON.parse(res.slice(4));
  return Array.isArray(j) ? Object.fromEntries(j.map((o) => [o.name, o.value])) : j;
};
const api = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then(async (r) => `${r.status} ${await r.text()}`);

async function read() {
  const u = await new Xmpp(USER_ID).connect();
  out('login existing JID', 'ok');
  out('roster items', count(await u.iq('get', '', `<query xmlns='jabber:iq:roster'/>`), /<item /g));
  const lists = await u.iq('get', '', `<query xmlns='jabber:iq:privacy'/>`);
  const def = (lists.match(/<default name=['"]([^'"]+)/) || [])[1];
  out('privacy default list', def || 'none');
  if (def) out('privacy default list items', count(await u.iq('get', '', `<query xmlns='jabber:iq:privacy'><list name='${def}'/></query>`), /<item /g));
  const own = await u.iq('get', `${USER_ID}@${D}`, `<vCard xmlns='vcard-temp'/>`);
  out('own vCard bytes', own.includes("type='result'") || own.includes('type="result"') ? own.length : `error ${own.slice(0, 120)}`);
  const multi = await u.iq('get', `${USER_ID}+${MUTED}@${D}`, `<vCard xmlns='vcard-temp'><JABBERID>${USER_ID}+${MUTED}</JABBERID></vCard>`);
  out('multi vCard cards', count(multi, /<vCard /g));
  const subs = await u.iq('get', MUC, `<subscriptions xmlns='urn:xmpp:mucsub:0'/>`);
  const names = [...subs.matchAll(/<subscription [^>]*jid=['"]([^@'"]+)@/g)].map((m) => m[1]);
  out('MUC/Sub subscriptions', names.length);
  out('MUC/Sub first 5 (order)', names.slice(0, 5).join(','));
  const h = await u.join(ROOM);
  out(`join ${ROOM} history msgs (delay)`, h.frames.filter((f) => f.startsWith('<message') && f.includes('urn:xmpp:delay') && !f.includes('<subject')).length);
  out(`join ${ROOM} subject closes history`, /<subject/.test(h.end));
  const mam = await (async () => {
    const from = u.frames.length;
    const fin = await u.iq('set', `${ROOM}@${MUC}`, `<query xmlns='urn:xmpp:mam:2'><set xmlns='http://jabber.org/protocol/rsm'><max>10</max><before/></set></query>`);
    return {n: u.frames.slice(from).filter((f) => /<result [^>]*urn:xmpp:mam:2/.test(f)).length, fin};
  })();
  out(`MAM ${ROOM} last 10`, mam.n);
  const c = await u.join(CLUB);
  out(`join ${CLUB}`, /<subject/.test(c.end) ? `ok, ${c.frames.filter((f) => f.includes('urn:xmpp:delay') && !f.includes('<subject')).length} history msgs` : c.end.slice(0, 160));
  u.close();

  for (const r of [ROOM, CLUB]) {
    const o = await api('get_room_options', {name: r, service: MUC});
    out(`get_room_options ${r}`, o.startsWith('200') ? `200, ${Object.keys(opts(o)).length} options, persistent=${opts(o).persistent}, mam=${opts(o).mam}` : o.slice(0, 160));
  }
  out(`get_room_affiliations ${CLUB}`, (await api('get_room_affiliations', {name: CLUB, service: MUC})).slice(0, 200));

  const m = await new Xmpp(MUTED).connect();
  const mj = await m.join(ROOM, 0);
  out('global muted: join', /<subject/.test(mj.end) ? 'joined' : mj.end.slice(0, 160));
  const from = m.frames.length;
  m.send(`<message to='${ROOM}@${MUC}' type='groupchat' id='mute1'><body>x</body></message>`);
  const mr = await m.wait((f) => f.includes("id='mute1'") || f.includes('id="mute1"'), 3000, from).catch(() => 'no echo, no error');
  out('global muted: post', mr.includes('error') ? `refused (${(mr.match(/<(forbidden|not-allowed|not-acceptable)/) || [])[1]})` : mr.includes('groupchat') ? 'DELIVERED' : mr);
  m.close();
  const o = await new Xmpp(OUTCAST).connect();
  const oj = await o.join(ROOM, 0);
  out('global outcast: join', /<subject/.test(oj.end) ? 'JOINED' : `refused (${(oj.end.match(/<(forbidden|not-allowed|registration-required)/) || [])[1]})`);
  o.close();
}

// Writes with the server under test: a message in CLUB, a new persistent room with a changed option + subscriber.
async function write() {
  out('create_room', await api('create_room', {name: NEW_ROOM, service: MUC, host: D}));
  out('change_room_option', await api('change_room_option', {name: NEW_ROOM, service: MUC, option: 'allow_subscription', value: 'true'}));
  const u = await new Xmpp(USER_ID).connect();
  out('subscribe', (await u.iq('set', `${NEW_ROOM}@${MUC}`, `<subscribe xmlns='urn:xmpp:mucsub:0' nick='${USER_ID}'><event node='urn:xmpp:mucsub:nodes:messages'/></subscribe>`)).slice(0, 80));
  for (const r of [CLUB, NEW_ROOM]) {
    await u.join(r, 0);
    u.send(`<message to='${r}@${MUC}' type='groupchat' id='w-${r}'><body>${MARK}</body></message>`);
    await sleep(800);
  }
  u.close();
}

async function readWritten() {
  for (const r of [CLUB, NEW_ROOM]) {
    const o = await api('get_room_options', {name: r, service: MUC});
    out(`get_room_options ${r}`, o.startsWith('200') ? `200, allow_subscription=${opts(o).allow_subscription}` : o.slice(0, 160));
  }
  const u = await new Xmpp(USER_ID).connect();
  const subs = await u.iq('get', MUC, `<subscriptions xmlns='urn:xmpp:mucsub:0'/>`);
  out(`subscribed to ${NEW_ROOM}`, subs.includes(NEW_ROOM));
  for (const r of [CLUB, NEW_ROOM]) {
    const h = await u.join(r, 5);
    out(`join ${r}: marker in join history`, h.frames.some((f) => f.includes(MARK)));
    const from = u.frames.length;
    await u.iq('set', `${r}@${MUC}`, `<query xmlns='urn:xmpp:mam:2'><set xmlns='http://jabber.org/protocol/rsm'><max>5</max><before/></set></query>`);
    out(`MAM ${r}: marker`, u.frames.slice(from).some((f) => f.includes(MARK)));
  }
  u.close();
}

const modes = {read, write, 'read-written': readWritten};
modes[process.argv[2]]().then(() => process.exit(0), (e) => { console.log(`FAILED | ${e.message}`); process.exit(1); });
setTimeout(() => { console.log('FAILED | global timeout'); process.exit(2); }, 90000);
