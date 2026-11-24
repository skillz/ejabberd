// P14 check: the fork's POST /api/kick_session {user, host, reason} kicks every resource of the
// user, answers 0, and the shipped native client gets the reason as its quarantine status.
// Runs unchanged against the fork baseline and 26.09 + mod_kick_sessions.
// run: NODE_PATH=<sdk-core/js/node_modules> node p14-kick-session.js
const assert = require('assert');
const {mk, ChatUtils} = require('./compat/native-smoke');
const API = process.env.API || 'http://127.0.0.1:15443/api';

const connect = (id) =>
  new Promise((res) => {
    const c = mk(id);
    const seen = {quarantine: null, disconnected: false};
    c.subscribeToEvent('quarantineChange', (s) => (seen.quarantine = s));
    c.subscribeToEvent('disconnected', () => (seen.disconnected = true));
    c.connectToChat(() => res({c, seen}));
  });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

(async () => {
  const a = await connect('1003');
  const b = await connect('1003');
  const other = await connect('1004');
  const r = await fetch(`${API}/kick_session`, {method: 'POST', body: JSON.stringify({user: '1003', host: 'chat.skillz.com', reason: 'muted'})});
  const body = await r.text();
  await sleep(1000);
  assert.strictEqual(r.status, 200, `HTTP ${r.status} ${body}`);
  assert.strictEqual(body.trim(), '0', `user-details-service expects 0, got ${body}`);
  for (const s of [a, b]) {
    assert.strictEqual(s.seen.disconnected, true, 'every resource kicked');
    assert.strictEqual(s.seen.quarantine, 'muted', `quarantine status from the stream error text, got ${s.seen.quarantine}`);
  }
  assert.strictEqual(other.seen.disconnected, false, 'other users untouched');
  console.log(`P14 ok (quarantine status seen by native: ${ChatUtils.quarantineStatus ?? a.seen.quarantine})`);
  process.exit(0);
})().catch((e) => {
  console.error('P14 FAILED', e.message);
  process.exit(1);
});
setTimeout(() => {
  console.error('P14 TIMEOUT');
  process.exit(2);
}, 20000);
