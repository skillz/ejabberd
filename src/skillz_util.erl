%%%-------------------------------------------------------------------
%%% File    : skillz_util.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 31 Mar 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(skillz_util).

%% Exports
-export([send_cas_message/3, send_message/4, get_message_xml_bin/4, get_env/0,
  get_admin_logo/0, get_flag_url/0, get_cas_jid/0, get_host/0, get_host/1,
  get_uuid/0]).

-include("logger.hrl").

send_cas_message(GameIdBin, SenderUsernameBin, BodyBin) ->
  RoomJidBin = <<GameIdBin/binary, (<<"@">>)/binary, (get_host("conference."))/binary>>,
  send_message(get_cas_jid(), RoomJidBin, SenderUsernameBin, BodyBin)
.

send_message(FromJidBin, RoomJidBin, SenderUsernameBin, BodyBin) ->
  mod_admin_extra:send_stanza(FromJidBin, RoomJidBin, get_message_xml_bin(FromJidBin, RoomJidBin, SenderUsernameBin, BodyBin))
.

get_message_xml_bin(FromJidBin, ToJidBin, SenderUsernameBin, BodyBin) ->
  UserId = "-1", %% unused but required as part of schema in SDK
  UserRole = "3", %% Admin role in SDK
  UserMentions = "@none", %% unused but required as part of schema in SDK
  MessageType = "0", %% always zero
  binary:list_to_bin(lists:flatten(io_lib:format(
    "<message xmlns='jabber:client'
        from='~s'
        to='~s'
        id='~s'
        type='groupchat'>
      <body>~s</body>
      <skillz_sdk xmlns='xmpp:skillz'>
        <avatar_url>~s</avatar_url>
        <flag_url>~s</flag_url>
        <user_id>~s</user_id>
        <username>~s</username>
        <user_role>~s</user_role>
        <user_mentions>~s</user_mentions>
        <message_type>~s</message_type>
      </skillz_sdk>
    </message>",
    [FromJidBin, ToJidBin, get_uuid(), BodyBin, get_admin_logo(), get_flag_url(), UserId, SenderUsernameBin, UserRole, UserMentions, MessageType]
  )))
.

get_admin_logo() ->
  case get_env() of
    "production" -> "https://s3.amazonaws.com/skillz-content-prod/default-profile-pics/Skillz-Profile-Picture.png";
    "staging" ->    "https://s3.amazonaws.com/skillz-content-stage/default-profile-pics/Skillz-Profile-Picture.png";
    _ ->            "https://s3.amazonaws.com/skillz-content-islay/default-profile-pics/Skillz-Profile-Picture.png"
  end
.

get_flag_url() ->
  case get_env() of
    "production" -> "https://cdn.skillz.com/flags/US.png";
    "staging" ->    "https://cdn.staging.skillz.com/flags/US.png";
    _ ->            "https://cdn.qa.skillz.com/flags/US.png"
  end
.

get_cas_jid() ->
  binary:list_to_bin(
    case get_env() of
      "production" -> "skillz-cas@chat-admin.skillz.com/skillz-cas";
      "staging" ->    "skillz-cas@chat-admin.staging.skillz.com/skillz-cas";
      "qa" ->         "skillz-cas@chat-admin.qa.skillz.com/skillz-cas";
      "dev" ->        "skillz-cas@localhost/skillz-cas"
    end
  )
.

get_host(Prefix) ->
  <<(binary:list_to_bin(Prefix))/binary, (get_host())/binary>>
.

get_host() ->
  case get_env() of
    "production" -> <<"chat.skillz.com">>;
    "staging" ->    <<"chat.staging.skillz.com">>;
    "qa" ->         <<"chat.qa.skillz.com">>;
    "dev" ->        <<"localhost">>
  end
.

in_node(Env) ->
  string:str(atom_to_list(node()), Env) > 0
.

get_env() ->
  case in_node(".production.") of
    true -> "production";
    _ ->
      case in_node(".staging.") of
        true -> "staging";
        _ -> case in_node(".qa.") of
          true -> "qa";
          _ -> "dev"
        end
      end
  end
.

get_hex_digits(NumDigits) ->
  integer_to_list(crypto:rand_uniform(trunc(math:pow(16, NumDigits - 1)), trunc(math:pow(16, NumDigits))) - 1, 16)
.

get_uuid() ->
  get_hex_digits(8) ++ "-" ++ get_hex_digits(4) ++ "-" ++ get_hex_digits(4) ++ "-" ++ get_hex_digits(4) ++ "-" ++ get_hex_digits(12)
.
