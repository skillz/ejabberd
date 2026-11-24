// Compatibility matrix: the shipped native client (@skillz/chat-client 0.2.1) and the web client
// (portal-play xmpp.ts, bundled with esbuild) against a local ejabberd. Prints one line per cell.
// run: WEB_BUNDLE=<web-xmpp.cjs> NODE_PATH=<sdk-core/js/node_modules>:<portal-play/node_modules> node matrix.js
const {JSDOM} = require('jsdom');
global.DOMParser = new JSDOM('').window.DOMParser;
const web = require(process.env.WEB_BUNDLE);
const {mk} = require('./native-smoke');

const API = process.env.API || 'http://127.0.0.1:15443/api';
const WS = process.env.WS || 'ws://127.0.0.1:15280/websocket';
const DOMAIN = 'chat.skillz.com';
const MUC = `conference.${DOMAIN}`;
const GAME = `${process.env.ROOM || 4242}`;
const CLUB = `${GAME}-club-test`;
const results = [];
const cell = (name, ok, note = '') => {
  results.push({name, ok, note});
  console.log(`${ok === true ? 'WORKS' : ok === false ? 'BREAKS' : 'INFO '} | ${name}${note ? ` | ${note}` : ''}`);
};
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
async function waitFor(pred, ms = 3000) {
  for (let t = 0; t < ms; t += 50) {
    if (pred()) return true;
    await sleep(50);
  }
  return Boolean(pred());
}
async function post(cmd, body) {
  const r = await fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)});
  return {status: r.status, body: await r.text()};
}
// 26.09 takes room/user/host; the 19.02 fork takes name/jid. Retry in the old form so one harness runs on both.
async function api(cmd, body) {
  const r = await post(cmd, body);
  if (r.status === 200 || !body.room) return r;
  const {room, user, host, ...rest} = body;
  const old = {...rest, name: room, ...(user ? {jid: `${user}@${host}`} : cmd === 'create_room' ? {host} : {})};
  const r2 = await post(cmd, old);
  return {...r2, body: `${r2.body} (19.02 args)`};
}

function webUser(id, room, opts = {}) {
  const events = [];
  const raw = [];
  const me = {user_id: id, username: `web${id}`, user_role: 0};
  const conn = new web.RoomConnection({
    wsUrl: WS,
    domain: DOMAIN,
    userId: id,
    password: `pw-${id}`,
    roomJid: `${room}@${MUC}`,
    historySize: opts.historySize ?? 50,
    onEvent: (e) => events.push(e),
  });
  const orig = conn.receive.bind(conn);
  conn.receive = (xml) => {
    raw.push(xml);
    orig(xml);
  };
  return {id, conn, events, raw, me, send: (body, o) => conn.send(body, me, o), sendRaw: (x) => conn.raw(x)};
}

function nativeUser(id) {
  const c = mk(id);
  const raw = [];
  c.client.on('raw:incoming', (x) => raw.push(x));
  const subjects = [];
  c.subscribeToEvent('historyReceived', () => subjects.push(Date.now()));
  // What the client's groupchat handler received (stanza.io parsed), independent of its ChatStore dedupe.
  const gc = [];
  c.client.on('groupchat', (m) => gc.push({room: String(m.from).split('@')[0], body: m.body, user_id: m.skillz_sdk?.user_id, username: m.skillz_sdk?.username, history: Boolean(m.delay), subjectSeen: subjects.length}));
  c.client.on('message', (m) => { const inner = m.event?.updated?.published?.[0]?.message; if (inner) gc.push({room: String(inner.from).split('@')[0], body: inner.body, user_id: inner.skillz_sdk?.user_id, mucsub: true}); });
  const connected = new Promise((res) => c.connectToChat(res));
  return {id, c, raw, subjects, gc, connected, msgs: (room) => gc.filter((m) => m.room === room)};
}
const nativeJoin = (n, room, count = 20) => new Promise((res) => n.c.connectToRoomWithCompletion(room, count, res));
const call = (fn) => Promise.race([new Promise((res) => fn((err, data) => res({err, data}))), sleep(3000).then(() => ({err: 'no completion within 3 s'}))]);

const thrown = [];
process.on('uncaughtException', (e) => { thrown.push(String(e && e.message)); });

(async () => {
  // Rooms: game room owned by mod1, club room for the mute test.
  for (const r of [GAME, CLUB]) await api('create_room', {room: r, service: MUC, host: DOMAIN});
  const aff = await api('set_room_affiliation', {room: GAME, service: MUC, user: 'mod1', host: DOMAIN, affiliation: 'owner'});
  cell('admin API set_room_affiliation owner (v3 args room/service/user/host)', aff.status === 200, aff.body);

  // 1. Login + join
  const w1 = webUser('1001', GAME);
  const n2 = nativeUser('1002');
  const okW = await waitFor(() => w1.events.some((e) => e.kind === 'joined'));
  cell('web login + join (SASL PLAIN, bind, session, MUC presence)', okW, JSON.stringify(w1.events.find((e) => e.kind === 'join-error') || ''));
  cell('native login (stanza.io picks SCRAM-SHA-1 on internal auth; PLAIN only under http auth)', await n2.connected);
  cell('native join', (await Promise.race([nativeJoin(n2, GAME), sleep(3000)])) === true);

  // 2. Send / receive with skillz_sdk both ways
  // Rooms enforce min_message_interval 0.4 s per sender (same value as the fork chart).
  for (let i = 1; i <= 3; i++) { w1.send(`web msg ${i}`); await sleep(450); }
  n2.c.sendMessage('native msg 1', GAME);
  await sleep(450);
  n2.c.sendMessage('native msg 2', GAME);
  await waitFor(() => n2.msgs(GAME).length >= 5 && w1.events.filter((e) => e.kind === 'message').length >= 5);
  const nm = n2.msgs(GAME).find((m) => String(m.user_id) === '1001');
  cell('web -> native groupchat with skillz_sdk', Boolean(nm && nm.username === 'web1001'), nm ? `native parsed user_id=${nm.user_id} username=${nm.username}` : 'not received');
  const wm = w1.events.find((e) => e.kind === 'message' && e.message.user_id === '1002');
  cell('native -> web groupchat with skillz_sdk', Boolean(wm && wm.message.username === 'user1002'), wm ? `web parsed id=${wm.message.id}` : 'not received');
  const echoed = w1.events.filter((e) => e.kind === 'message' && e.message.user_id === '1001').map((e) => e.message.id);
  cell('message id echoed back to sender (client dedupe)', echoed.length === 3 && echoed.every((id) => id.startsWith('w')), echoed.join(','));
  const nativeRawMsg = w1.raw.find((x) => x.includes('native msg 1')) || '';
  cell('native skillz_sdk child position as seen by peers', null, (nativeRawMsg.match(/<message[^>]*>(.*)$/s)?.[1] || '').replace(/>[^<]*</g, '><').slice(0, 220));
  const sid = nativeRawMsg.match(/<stanza-id[^>]*id='(\d+)'/);
  cell('live groupchat carries XEP-0359 stanza-id (needed for XEP-0425)', Boolean(sid), sid ? sid[0] : '');

  // 3. Subject set by owner, then history + subject on join for new joiners
  const mod = webUser('mod1', GAME);
  await waitFor(() => mod.events.some((e) => e.kind === 'joined'));
  mod.sendRaw(`<message type="groupchat" to="${GAME}@${MUC}" id="subj1"><subject>Pinned!</subject><skillz_sdk xmlns="xmpp:skillz"><pinned_by>mod1</pinned_by></skillz_sdk></message>`);
  await waitFor(() => w1.events.some((e) => e.kind === 'subject' && e.text === 'Pinned!'));
  const subjRaw = w1.raw.filter((x) => x.includes('<subject>Pinned!')).pop() || '';
  cell('owner sets subject; occupants get it', Boolean(subjRaw), `extra skillz_sdk child kept on subject: ${subjRaw.includes('pinned_by')}`);

  const w3 = webUser('1003', GAME, {historySize: 50});
  await waitFor(() => w3.events.some((e) => e.kind === 'subject'));
  const hist = w3.events.find((e) => e.kind === 'history');
  const histIdx = w3.events.findIndex((e) => e.kind === 'history');
  const subjIdx = w3.events.findIndex((e) => e.kind === 'subject');
  cell('web: history on join, closed by subject', Boolean(hist && hist.messages.length === 5 && subjIdx > histIdx), `history=${hist?.messages.length} subject='${w3.events[subjIdx]?.text}' order ok=${subjIdx > histIdx}`);

  const n4 = nativeUser('1004');
  await n4.connected;
  const before = n4.subjects.length;
  await Promise.race([nativeJoin(n4, GAME), sleep(3000)]);
  await waitFor(() => n4.subjects.length > before);
  const nh = n4.msgs(GAME).filter((m) => m.history && m.subjectSeen === before);
  cell('native: history on join, then subject callback', nh.length === 5 && n4.subjects.length > before, `history=${nh.length} subjectCallbacks=${n4.subjects.length - before}`);
  const emptySubj = n4.raw.filter((x) => /<subject\/>|<subject><\/subject>/.test(x)).length;
  cell('initial empty subject (25.03) seen by native', null, `count=${emptySubj} (room has a subject, so expected 0)`);

  // 4. MUC/Sub. Neither server's default room options allow it; the room owner turns it on.
  mod.sendRaw(`<iq type="set" id="cfgsub" to="${GAME}@${MUC}"><query xmlns="http://jabber.org/protocol/muc#owner"><x xmlns="jabber:x:data" type="submit"><field var="FORM_TYPE"><value>http://jabber.org/protocol/muc#roomconfig</value></field><field var="allow_subscription"><value>1</value></field></x></query></iq>`);
  await waitFor(() => mod.raw.some((x) => x.includes("id='cfgsub'")));
  const cfg = mod.raw.find((x) => x.includes("id='cfgsub'")) || '';
  cell('owner turns on allow_subscription (muc#owner form)', cfg.includes("type='result'"), cfg.slice(0, 200));
  const sub = await call((cb) => n4.c.client.subscribeToRoom(`${GAME}@${MUC}`, `1004@${DOMAIN}`, cb));
  cell('MUC/Sub subscribe IQ (native)', !sub.err, sub.err ? JSON.stringify(sub.err).slice(0, 200) : '');
  const subs = await n4.c.fetchSubscriptions().catch((e) => `ERR ${JSON.stringify(e).slice(0, 200)}`);
  cell('MUC/Sub subscriptions list (native fetchSubscriptions)', Array.isArray(subs) && subs.includes(GAME), JSON.stringify(subs));
  await new Promise((res) => n4.c.disconnectFromRoomWithCompletion(GAME, res));
  await sleep(300);
  const n4Before = n4.msgs(GAME).length;
  w1.send('to subscribers while 1004 is out of the room');
  await waitFor(() => n4.msgs(GAME).length > n4Before);
  const subRaw = n4.raw.find((x) => x.includes('while 1004 is out')) || '';
  cell('MUC/Sub live delivery to subscriber not in room (native renders it)', n4.msgs(GAME).length > n4Before, subRaw ? `wrapped in pubsub event: ${subRaw.includes('urn:xmpp:mucsub:nodes:messages')}` : 'not received');

  // offline subscriber: store + retrieve via XEP-0013 headers
  n4.c.disconnectFromServer();
  await sleep(500);
  w1.send('to subscribers while 1004 is offline');
  await sleep(500);
  const n4b = nativeUser('1004');
  await n4b.connected;
  const headers = n4b.c.getChatStore().getUnreadMessageContentCache?.() || [];
  cell('MUC/Sub offline delivery (stored offline, XEP-0013 headers on reconnect)', headers.length > 0, `offline headers=${headers.length} ${JSON.stringify(headers).slice(0, 160)}`);

  // 5. Privacy: mute a club room (message-only item), block a user (item value <id>@conference)
  const n2Club = nativeUser('1002');
  await n2Club.connected;
  await Promise.race([nativeJoin(n2Club, CLUB), sleep(3000)]);
  const w1Club = webUser('1001', CLUB);
  await waitFor(() => w1Club.events.some((e) => e.kind === 'joined'));
  const mute = await call((cb) => n2Club.c.setMuteStatus(CLUB, true, cb));
  cell('native mute (privacy list set + default + active)', !mute.err, mute.err ? JSON.stringify(mute.err).slice(0, 200) : '');
  const cnt = n2Club.msgs(CLUB).length;
  w1Club.send('club message while muted');
  await waitFor(() => n2Club.msgs(CLUB).length > cnt, 1500);
  cell('mute: live room messages still delivered (fork respect_mute)', n2Club.msgs(CLUB).length > cnt, 'stock: a message-only deny item on the room JID drops live delivery');

  const block = await call((cb) => n2.c.setBlockStatus('1001', true, cb));
  cell('native block (privacy item 1001@conference...)', !block.err, block.err ? JSON.stringify(block.err).slice(0, 200) : '');
  const c2 = n2.msgs(GAME).length;
  w1.send('game message from a blocked user');
  await sleep(1200);
  cell('block: blocked user\'s room messages hidden (fork matches item to occupant nick)', n2.msgs(GAME).length === c2, `delivered=${n2.msgs(GAME).length > c2}`);
  // Mute vs offline storage: a muted room's messages must not be stored for an offline subscriber (no push).
  const opt = await api('change_room_option', {room: CLUB, service: MUC, option: 'allow_subscription', value: 'true'});
  const n6 = nativeUser('1004');
  await n6.connected;
  const sub6 = await call((cb) => n6.c.client.subscribeToRoom(`${CLUB}@${MUC}`, `1004@${DOMAIN}`, cb));
  const mute6 = await call((cb) => n6.c.setMuteStatus(CLUB, true, cb));
  n6.c.disconnectFromServer();
  await sleep(500);
  w1Club.send('club message while the muted subscriber is offline');
  await sleep(600);
  const n6b = nativeUser('1004');
  await n6b.connected;
  const clubHeaders = (n6b.c.getChatStore().getUnreadMessageContentCache?.() || []).filter((h) => String(h.name).startsWith(CLUB));
  cell('mute: muted room not stored offline for a subscriber (no push)', clubHeaders.length === 0, `room option=${opt.status} subscribe=${sub6.err ? 'ERR' : 'ok'} mute=${mute6.err ? 'ERR' : 'ok'} offline headers for the room=${clubHeaders.length}`);
  const pl = await call((cb) => n2.c.client.getPrivacyList(cb));
  cell('privacy list read back (native parses muted/blocked)', null, JSON.stringify(pl.data?.privacy?.privacyLists?.[0]?.privacyItems || pl.err).slice(0, 200));

  // 6. vCard: publish at connect, single + multi fetch
  const n5 = nativeUser('1003');
  await n5.connected;
  await sleep(300);
  const one = await Promise.race([n5.c.fetchVCard('1001').then((v) => v, (e) => `ERR ${JSON.stringify(e).slice(0, 160)}`), sleep(2000).then(() => `no answer; client threw: ${thrown.join('; ')}`)]);
  cell('vCard single fetch (native multi-vCard IQ with one id)', typeof one === 'object', typeof one === 'object' ? `nick=${one.nicknames}` : one);
  const many = await Promise.race([n5.c.fetchVCardsForUserIds(['1002', '1004'], 10).then((v) => v, (e) => `ERR ${JSON.stringify(e).slice(0, 160)}`), sleep(2000).then(() => `no answer; client threw: ${thrown.join('; ')}`)]);
  cell('vCard multi fetch JABBERID "a+b" (native)', Array.isArray(many) && many.length === 2, Array.isArray(many) ? `${many.length} cards` : many);

  // 6b. Global mute/ban (P2): fork command set_user_affiliation; applies to rooms with no '-' only.
  const gm = await post('set_user_affiliation', {host: DOMAIN, user: '1003', affiliation: 'muted'});
  cell('set_user_affiliation muted (fork command, P2)', gm.status === 200, gm.body.slice(0, 120));
  w3.send('muted while already in the room');
  await sleep(800);
  cell('global mute: user already in the room when muted', null, `can still post=${w1.events.some((e) => e.kind === 'message' && e.message.body === 'muted while already in the room')} (fork: yes until rejoin; callers kick the session)`);
  const w3m = webUser('1003', GAME);
  await waitFor(() => w3m.events.some((e) => e.kind === 'joined' || e.kind === 'join-error'));
  const mh = w3m.events.find((e) => e.kind === 'history');
  cell('global mute: muted user can join and read history', w3m.events.some((e) => e.kind === 'joined') && Boolean(mh), `history=${mh?.messages.length}`);
  const ownPres = w3m.raw.find((x) => x.includes("code='110'")) || '';
  cell('global mute: muted user\'s own presence role', null, (ownPres.match(/role='(\w+)'/) || [])[0] || 'none');
  w3m.send('muted user speaks');
  await sleep(800);
  const merr = w3m.events.find((e) => e.kind === 'send-error');
  const leaked = w1.events.some((e) => e.kind === 'message' && e.message.body === 'muted user speaks');
  cell('global mute: muted user cannot post in a game room', !leaked && Boolean(merr), `sender error=${merr?.condition} leaked=${leaked}`);
  const w3club = webUser('1003', CLUB);
  await waitFor(() => w3club.events.some((e) => e.kind === 'joined' || e.kind === 'join-error'));
  w3club.send('muted user in a club room');
  await sleep(800);
  cell('global mute: does not apply to rooms with "-"', w1Club.events.some((e) => e.kind === 'message' && e.message.body === 'muted user in a club room'));
  w3m.conn.close();
  w3club.conn.close();
  await post('set_user_affiliation', {host: DOMAIN, user: '1003', affiliation: 'outcast'});
  const w3o = webUser('1003', GAME);
  await waitFor(() => w3o.events.some((e) => e.kind === 'joined' || e.kind === 'join-error'));
  const jerr = w3o.events.find((e) => e.kind === 'join-error');
  cell('global ban (outcast): banned user cannot join a game room', Boolean(jerr), `join-error=${jerr?.condition}`);
  w3o.conn.close();
  await post('set_user_affiliation', {host: DOMAIN, user: '1003', affiliation: 'none'});
  const w3u = webUser('1003', GAME);
  await waitFor(() => w3u.events.some((e) => e.kind === 'joined' || e.kind === 'join-error'));
  cell('global affiliation none: user can join again', w3u.events.some((e) => e.kind === 'joined'));
  w3u.conn.close();

  // 7. Affiliation changes
  const a1 = await api('set_room_affiliation', {room: GAME, service: MUC, user: '1003', host: DOMAIN, affiliation: 'admin'});
  const a2 = await api('get_room_affiliations', {room: GAME, service: MUC});
  cell('admin API: grant admin + list affiliations', a1.status === 200 && a2.body.includes('1003'), a2.body.slice(0, 200));
  mod.sendRaw(`<iq type="set" id="aff1" to="${GAME}@${MUC}"><query xmlns="http://jabber.org/protocol/muc#admin"><item affiliation="member" jid="1003@${DOMAIN}"/></query></iq>`);
  await waitFor(() => mod.raw.some((x) => x.includes("id='aff1'")));
  const affIq = mod.raw.find((x) => x.includes("id='aff1'")) || '';
  cell('owner demotes admin -> member by IQ (owner client)', affIq.includes("type='result'"), affIq.slice(0, 160));
  const w3c = w3.events.length;
  const out = await api('set_room_affiliation', {room: GAME, service: MUC, user: '1003', host: DOMAIN, affiliation: 'outcast'});
  await sleep(500);
  const w3After = w3.events.slice(w3c);
  cell('outcast via API: what the occupant sees (fork: becomes visitor, stays)', null, `api=${out.status} web events=${JSON.stringify(w3After).slice(0, 160)}`);
  await api('set_room_affiliation', {room: GAME, service: MUC, user: '1003', host: DOMAIN, affiliation: 'none'});

  // 8. XEP-0425 moderator retract
  const target = w1.send('message to be moderated');
  await waitFor(() => mod.raw.some((x) => x.includes('message to be moderated')));
  const targetRaw = mod.raw.find((x) => x.includes('message to be moderated'));
  const targetSid = targetRaw.match(/<stanza-id[^>]*id='(\d+)'/)?.[1];
  const moderate = (from, id, sid) =>
    from.sendRaw(`<iq type="set" id="${id}" to="${GAME}@${MUC}"><moderate xmlns="urn:xmpp:message-moderate:1" id="${sid}"><retract xmlns="urn:xmpp:message-retract:1"/><reason>Spam</reason></moderate></iq>`);
  const w4 = webUser('1004', GAME);
  await waitFor(() => w4.events.some((e) => e.kind === 'joined'));
  moderate(w4, 'modbad', targetSid);
  await waitFor(() => w4.raw.some((x) => x.includes("id='modbad'")));
  cell('XEP-0425: plain member is refused', (w4.raw.find((x) => x.includes("id='modbad'")) || '').includes('forbidden'));
  const n2subj = n2.subjects.length;
  const w1ev = w1.events.length;
  moderate(mod, 'mod1', targetSid);
  await waitFor(() => mod.raw.some((x) => x.includes("id='mod1'")));
  const modRes = mod.raw.find((x) => x.includes("id='mod1'")) || '';
  cell('XEP-0425: owner retract IQ accepted', modRes.includes("type='result'"), modRes.slice(0, 160));
  await sleep(500);
  const bcast = w1.raw.find((x) => x.includes('urn:xmpp:message-retract:1')) || '';
  cell('XEP-0425: retraction broadcast reaches occupants', Boolean(bcast), bcast.replace(/xmlns='jabber:client' /, '').slice(0, 260));
  cell('XEP-0425: web (current xmpp.ts) reaction', null, `new events=${JSON.stringify(w1.events.slice(w1ev).map((e) => e.kind))} -> row stays until reload`);
  cell('XEP-0425: native reaction', null, `subject/history callbacks fired=${n2.subjects.length - n2subj}; message still in store=${n2.msgs(GAME).some((m) => m.body === 'message to be moderated')}`);
  const w5 = webUser('1002', GAME, {historySize: 50});
  await waitFor(() => w5.events.some((e) => e.kind === 'subject'));
  const h5 = w5.events.find((e) => e.kind === 'history')?.messages || [];
  cell('XEP-0425: retracted message gone from join history', !h5.some((m) => m.body === 'message to be moderated'), `history=${h5.length}`);
  const page = await w5.conn.older(new Date(Date.now() + 60000)).catch((e) => ({messages: [], err: String(e)}));
  cell('XEP-0425: retracted message gone from MAM', !page.messages.some((m) => m.body === 'message to be moderated'), `mam page=${page.messages.length} ${page.err || ''}`);
  const row = await api('get_room_history', {room: GAME, service: MUC});
  cell('get_room_history admin command (replaces fork get_room_summary source)', row.status === 200, row.body.slice(0, 120));

  console.log(`\nSUMMARY works=${results.filter((r) => r.ok === true).length} breaks=${results.filter((r) => r.ok === false).length} info=${results.filter((r) => r.ok === null).length}`);
  process.exit(0);
})().catch((e) => {
  console.error('HARNESS ERROR', e);
  process.exit(1);
});
setTimeout(() => {
  console.error('TIMEOUT');
  process.exit(2);
}, 180000);
