// P1 check: ejabberd_auth_http against test/auth-stub.js. Needs `run-local.sh up http`.
// run: NODE_PATH=<sdk-core/js/node_modules> node p1-auth-http.js
const assert = require('assert');
const {mk} = require('./compat/native-smoke');
const WS = process.env.WS || 'ws://127.0.0.1:15280/websocket';

// Raw SASL exchange over the websocket; resolves with the server's verdict and the offered mechanisms.
function sasl(authzid, user, pass) {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(WS, 'xmpp');
    let mechs = '';
    ws.onopen = () => ws.send('<open xmlns="urn:ietf:params:xml:ns:xmpp-framing" to="chat.skillz.com" version="1.0"/>');
    ws.onmessage = ({data}) => {
      if (data.includes('<mechanisms')) {
        mechs = [...data.matchAll(/<mechanism>([^<]+)</g)].map((m) => m[1]).join(',');
        ws.send(`<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN">${Buffer.from(`${authzid}\0${user}\0${pass}`).toString('base64')}</auth>`);
      } else if (/<success|<failure/.test(data)) {
        ws.close();
        resolve({ok: data.includes('<success'), mechs});
      }
    };
    ws.onerror = reject;
  });
}
const nativeLogin = (id, token) =>
  new Promise((res) => {
    const c = mk(id);
    c.user.auth_token = token;
    c.config = c._getConnectionInfo();
    c.connectToChat((ok) => { res(ok); setTimeout(() => c.client.disconnect(), 0); });
    setTimeout(() => res('timeout'), 5000);
  });

(async () => {
  const ok = await sasl('', '1001', 'pw-1001');
  assert.ok(ok.ok, "valid token accepted");
  assert.ok(!/SCRAM|DIGEST/.test(ok.mechs), `only plain-text mechanisms under http auth, got ${ok.mechs}`);
  assert.strictEqual((await sasl('', '1001', 'wrong')).ok, false, 'wrong token refused');
  assert.strictEqual((await sasl('', 'alice', 'pw-alice')).ok, false, 'user unknown to the endpoint refused');
  assert.strictEqual((await sasl('1002@chat.skillz.com', '1001', 'pw-1001')).ok, false, 'authzid for another user refused');
  assert.strictEqual((await sasl('', '1001', '')).ok, false, 'empty token refused');
  assert.strictEqual(await nativeLogin('1002', 'pw-1002'), true, 'shipped native client logs in');
  assert.strictEqual(await nativeLogin('1002', 'nope'), false, 'shipped native client refused with a bad token');
  console.log(`P1 ok (mechanisms offered: ${ok.mechs})`);
  process.exit(0);
})().catch((e) => {
  console.error('P1 FAILED', e.message);
  process.exit(1);
});
