// P12 check: the order of the MUC/Sub `subscriptions` list the shipped native client uses for its
// room list. The fork orders by latest archived message; prints the order seen. No pass/fail.
// run: NODE_PATH=<sdk-core/js/node_modules> node p12-subscriptions-order.js
const {mk} = require('./compat/native-smoke');
const API = process.env.API || 'http://127.0.0.1:15443/api';
const MUC = 'conference.chat.skillz.com';
const base = Date.now() % 100000 + 800000;
const rooms = [`${base}-club-a`, `${base}-club-b`, `${base}-club-c`];
const post = (cmd, body) => fetch(`${API}/${cmd}`, {method: 'POST', body: JSON.stringify(body)}).then((r) => r.text());
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const connect = (id) => new Promise((res) => { const c = mk(id); c.connectToChat(() => res(c)); });
const call = (fn) => new Promise((res) => fn((err, data) => res({err, data})));

(async () => {
  for (const r of rooms) {
    await post('create_room', {name: r, room: r, service: MUC, host: 'chat.skillz.com'});
    await post('change_room_option', {name: r, room: r, service: MUC, option: 'allow_subscription', value: 'true'});
  }
  const c = await connect('1002');
  for (const r of rooms) await call((cb) => c.client.subscribeToRoom(`${r}@${MUC}`, '1002@chat.skillz.com', cb));
  // Post into a, then c, then b: most recent first would be b, c, a.
  const sender = await connect('1001');
  for (const r of [rooms[0], rooms[2], rooms[1]]) {
    await new Promise((res) => sender.connectToRoomWithCompletion(r, 0, res));
    sender.sendMessage(`hello ${r}`, r);
    await sleep(1200);
  }
  const subs = (await c.fetchSubscriptions()).filter((n) => rooms.includes(n)).map((n) => n.slice(-1));
  console.log(`subscriptions order: ${subs.join(',')} (latest message first would be b,c,a)`);
  process.exit(0);
})();
setTimeout(() => process.exit(2), 30000);
