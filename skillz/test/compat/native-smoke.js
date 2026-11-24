// Drives the shipped native client (@skillz/chat-client 0.2.1 + stanza.io 10) against the local server.
// run: NODE_PATH=<sdk-core/js/node_modules> node native-smoke.js
const ChatUtils = require('skillz-chat-client/lib/Utilities/ChatUtils').default;
const Client = require('skillz-chat-client/lib/Client/SkillzChatClient').default;
ChatUtils.getSocketUrl = () => process.env.WS || 'ws://127.0.0.1:15280/websocket';
const settings = {domain: 'chat.skillz.com', muc_resource: 'conference', room_name: 4242};
const mk = (id) => {
  const c = new Client({id, username: `user${id}`, auth_token: `pw-${id}`, avatar_url: 'https://x/a.png'}, settings);
  c.initialize(!!process.env.VERBOSE);
  return c;
};
module.exports = {mk, ChatUtils};
if (require.main === module) {
  const a = mk('1001');
  a.connectToChat((ok) => {
    console.log('connect', ok);
    a.connectToGamewideRoomWithCompletion(20, (j) => {
      console.log('joined', j);
      a.sendMessage('hello from native', undefined, '@none', true);
      setTimeout(() => { console.log('store', JSON.stringify(a.getChatStore().getMessages?.('4242') ?? null).slice(0, 400)); process.exit(0); }, 1500);
    });
  });
  setTimeout(() => { console.log('timeout'); process.exit(1); }, 10000);
}
