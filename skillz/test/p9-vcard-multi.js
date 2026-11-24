// P9 check: shipped native clients publish their vCard on connect and batch-fetch others with
// JABBERID "a+b". Runs unchanged against the fork baseline and 26.09 + mod_vcard_multi.
// run: NODE_PATH=<sdk-core/js/node_modules> node p9-vcard-multi.js
const assert = require('assert');
const {mk} = require('./compat/native-smoke');

const connect = (id) => new Promise((res) => { const c = mk(id); c.connectToChat(() => res(c)); });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const thrown = [];
process.on('uncaughtException', (e) => thrown.push(e.message));

(async () => {
  await connect('1001');
  await connect('1002');
  const c = await connect('1003');
  await sleep(500);
  const cards = await c.fetchVCardsForUserIds(['1001', '1002', 'nobody'], 10);
  assert.deepStrictEqual(cards.map((v) => v.nicknames[0]).sort(), ['user1001', 'user1002']);
  assert.deepStrictEqual(cards.map((v) => v.jids[0]).sort(), ['1001@chat.skillz.com', '1002@chat.skillz.com']);
  assert.strictEqual(cards[0].photo.url, 'https://x/a.png');
  const one = await c.fetchVCard('1002');
  assert.strictEqual(one.nicknames[0], 'user1002');
  // A batch where nobody has a vCard: the fork answers an empty <vCards/>, which makes the shipped
  // client throw inside its callback. Same on both servers; kept as a regression marker.
  c.fetchVCard('nobody').catch(() => undefined);
  await sleep(800);
  assert.deepStrictEqual(thrown, ["Cannot read properties of undefined (reading 'length')"]);
  console.log('P9 ok (2-card batch, single fetch, empty-batch client throw reproduced)');
  process.exit(0);
})().catch((e) => {
  console.error('P9 FAILED', e.message);
  process.exit(1);
});
setTimeout(() => { console.error('P9 TIMEOUT'); process.exit(2); }, 20000);
