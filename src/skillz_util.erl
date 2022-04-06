%%%-------------------------------------------------------------------
%%% File    : skillz_util.erl
%%% Author  : Devin Sills <dsills@skillz.com>
%%% Created : 31 Mar 2022 by Devin Sills <dsills@skillz.com>
%%%
%%%----------------------------------------------------------------------

-module(skillz_util).

%% Exports
-export([send_cas_message/3, send_cas_message/4, send_message/6, get_message_xml_bin/4, get_env/0,
  get_admin_logo/0, get_flag_url/0, get_cas_jid/0, get_host/0, get_host/1,
  get_uuid/0]).

-include("logger.hrl").

send_cas_message(RoomNameBin, SenderDisplayNameBin, BodyBin) ->
  send_cas_message(RoomNameBin, SenderDisplayNameBin, BodyBin, none)
.

send_cas_message(RoomNameBin, SenderDisplayNameBin, BodyBin, DefaultFromResource) ->
  send_message(get_cas_jid(), RoomNameBin, SenderDisplayNameBin, BodyBin, "moderator", DefaultFromResource)
.

send_message(FromJidBin, RoomNameBin, SenderDisplayNameBin, BodyBin, FromUserType, DefaultFromResource) ->
  Host = get_host("conference."),
  ToJidBin = <<RoomNameBin/binary, (<<"@">>)/binary, Host/binary>>,
  FromJidWithResourceBin = add_uuid_as_resource(Host, FromJidBin, RoomNameBin, FromUserType, DefaultFromResource),
  Msg = get_message_xml_bin(FromJidWithResourceBin, ToJidBin, SenderDisplayNameBin, BodyBin),
  mod_admin_extra:send_stanza(FromJidWithResourceBin, ToJidBin, Msg)
.

get_message_xml_bin(FromJidBin, ToJidBin, SenderDisplayNameBin, BodyBin) ->
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
    [FromJidBin, ToJidBin, get_uuid(), BodyBin, get_admin_logo(), get_flag_url(), UserId, SenderDisplayNameBin, UserRole, UserMentions, MessageType]
  )))
.

get_resource_of_jid_in_room(HostBin, JidBin, RoomNameBin, UserType, DefaultResource) ->
  try
    case mod_muc_admin:get_room_occupants(RoomNameBin, HostBin) of
      ListOfUsers when is_list(ListOfUsers) ->
        case lists:filtermap(
          fun({User, Resource, Type}) ->
            case Type == UserType andalso binary:match(User, JidBin) /= nomatch of
              true -> {true, Resource};
              _ -> false
            end
          end, ListOfUsers
        ) of
          [] -> DefaultResource;
          ListOfResource -> lists:last(ListOfResource)
        end;
      _ -> DefaultResource
    end
  catch TypeOfError:Error ->
    ?ERROR_MSG("Failed to get resource of jid in room. Host: [~p]. Jid: [~p]. Room: [~p]. Type of Error: [~p]. Error: [~p].", [HostBin, JidBin, RoomNameBin, TypeOfError, Error]),
    DefaultResource
  end
.

add_resource(JidBin, ResourceBin) ->
  <<JidBin/binary, (<<"/">>)/binary, ResourceBin/binary>>
.

add_uuid_as_resource(HostBin, JidBin, RoomNameBin, UserType, DefaultResource) ->
  case get_resource_of_jid_in_room(HostBin, JidBin, RoomNameBin, UserType, DefaultResource) of
    none -> JidBin;
    Resource -> add_resource(JidBin, Resource)
  end
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
  case get_env() of
    "production" -> <<"skillz-cas@chat-admin.skillz.com">>;
    "staging" ->    <<"skillz-cas@chat-admin.staging.skillz.com">>;
    "qa" ->         <<"skillz-cas@chat-admin.qa.skillz.com">>;
    "dev" ->        <<"skillz-cas@localhost">>
  end
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
