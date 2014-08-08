%%%---------------------------------------------------------------------------
%%% @doc
%%%   Protocol handling.
%%% @end
%%%---------------------------------------------------------------------------

-module(ealirc_proto).

%% line processing
-export([encode/2, encode/3, decode/1]).
-export([parse/1]).
%% RFC 2812 messages
%% http://tools.ietf.org/rfc/rfc2812.txt
-export([pass/2, nick/2, user/4, user/5, oper/3, mode/3, service/5, service/7]).
-export([quit/1, quit/2, squit/3]).
-export([join/2, part/2, part/3, topic/2, topic/3]). % also mode/3
-export([names/2, names/3, list/1, list/2, list/3]).
-export([invite/3, kick/3, kick/4]).
-export([privmsg/3, notice/3]).
-export([motd/1, motd/2, lusers/1, lusers/2, lusers/3, version/1, version/2]).
-export([stats/1, stats/2, stats/3, links/1, links/2, links/3]).
-export([time/1, time/2, connect/3, connect/4, trace/1, trace/2]).
-export([admin/1, admin/2, info/1, info/2]).
-export([servlist/1, servlist/2, servlist/3, squery/3]).
-export([who/1, who/2, who/3, whois/2, whois/3]).
-export([whowas/2, whowas/3, whowas/4]).
-export([kill/3, ping/2, ping/3, pong/2, pong/3, error/2]).
-export([away/1, away/2, rehash/1, die/1, restart/1]).
-export([summon/2, summon/3, summon/4]).
-export([users/1, users/2]).
-export([wallops/2]).
-export([userhost/2, ison/2]).

%%%---------------------------------------------------------------------------
%%% convenience types

-type prefix() :: ealirc:prefix().
-type command() :: ealirc:command().
-type argument() :: ealirc:argument().
-type nick() :: ealirc:nick().
-type channel() :: ealirc:channel().
-type server() :: ealirc:server().

% +o -i ...
-type user_mode() :: term().
-type channel_mode() :: term().

%%%---------------------------------------------------------------------------

%% @doc Encode IRC command as a string, ending with CR+LF.

-spec encode(command(), [argument()]) ->
  {ok, iolist()} | {error, term()}.

encode(Cmd, Args) ->
  encode(none, Cmd, Args).

%% @doc Encode IRC command as a string, ending with CR+LF.

-spec encode(prefix(), command(), [argument()]) ->
  {ok, iolist()} | {error, term()}.

encode(_Prefix, _Cmd, _Args) ->
  'TODO'.

%% @doc Decode line into a well-formed message.
%%   Function does not interpret or validate the message.

-spec decode(string()) ->
  {ok, {prefix(), command(), [argument()]}} | {error, term()}.

decode(_Line) ->
  'TODO'.

%%%---------------------------------------------------------------------------

%% @doc Decode parsed message to one of the requests.
%%   Decoding includes checking if the message has appropriate arguments.

-spec parse({prefix(), command(), [argument()]}) ->
  {ok, term()} | {error, term()}.

parse({_Prefix, _Cmd, _Args} = _Message) ->
  'TODO'.

%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% 3.1 connection registration {{{

%% @doc Password message.

-spec pass(prefix(), string()) ->
  ok.

pass(_Prefix, _Password) ->
  'TODO'.

%% @doc Nick change/set request.

-spec nick(prefix(), nick()) ->
  ok.

nick(_Prefix, _Nick) ->
  'TODO'.

%% @doc Passing username and realname.

-spec user(prefix(), nick(), none | invisible | wallops | invisible_wallops,
           string()) ->
  ok.

user(Prefix, User, Mode, RealName) ->
  user(Prefix, User, Mode, "*", RealName).

%% @doc Passing username and realname.

-spec user(prefix(), nick(), none | invisible | wallops | invisible_wallops,
           string(), string()) ->
  ok.

user(_Prefix, _User, _Mode, _Unused, _RealName) ->
  % Mode -- bitmask; 8 for invisible, 4 for wallops
  'TODO'.

%% @doc Obtain operator privileges.

-spec oper(prefix(), nick(), string()) ->
  ok.

oper(_Prefix, _User, _Password) ->
  'TODO'.

%% @doc Set user or channel mode.

-spec mode(prefix(), nick(),    [user_mode()])    -> ok;
          (prefix(), channel(), [channel_mode()]) -> ok.

mode(_Prefix, "#" ++ _ = _Channel, _Modes) -> 'TODO';
mode(_Prefix, "+" ++ _ = _Channel, _Modes) -> 'TODO';
mode(_Prefix, "!" ++ _ = _Channel, _Modes) -> 'TODO';
mode(_Prefix, "&" ++ _ = _Channel, _Modes) -> 'TODO';
mode(_Prefix, _Nick, _Modes) ->
  'TODO'.

%% @doc Service registration message.

-spec service(prefix(), nick(), string(), string(), string()) ->
  ok.

service(_Prefix, _Nick, _Distribution, _Type, _Info) ->
  'TODO'.

%% @doc Service registration message.

-spec service(prefix(), nick(), string(), string(), string(), string(),
              string()) ->
  ok.

service(_Prefix, _Nick, _Reserved, _Distribution, _Type, _Reserved1, _Info) ->
  'TODO'.

%% @doc Quit message.

-spec quit(prefix()) ->
  ok.

quit(_Prefix) ->
  'TODO'.

%% @doc Quit message.

-spec quit(prefix(), string()) ->
  ok.

quit(_Prefix, _Message) ->
  'TODO'.

%% @doc Disconnect server links.

-spec squit(prefix(), string(), string()) ->
  ok.

squit(_Prefix, _Server, _Comment) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.2 channel operations {{{

%% @doc Join provided channels.
%%   Channel may be provided as a name or a tuple of `{Chan,Key}', containing
%%   channel key.
%%
%%   Special case of integer 0 is for <i>JOIN 0</i> IRC command, which leaves
%%   all the channels user is in.

-spec join(prefix(), [channel() | {channel(), string()}] | 0) ->
  ok.

join(_Prefix, 0 = _Channels) ->
  'TODO';
join(_Prefix, _Channels) ->
  'TODO'.

%% @doc Leave specified channels.

-spec part(prefix(), [channel()]) ->
  ok.

part(_Prefix, _Channels) ->
  'TODO'.

%% @doc Leave specified channels.

-spec part(prefix(), [channel()], string()) ->
  ok.

part(_Prefix, _Channels, _Message) ->
  'TODO'.

%% @doc Request a topic for the channel.

-spec topic(prefix(), channel()) ->
  ok.

topic(_Prefix, _Channel) ->
  'TODO'.

%% @doc Set topic for specified channel.

-spec topic(prefix(), channel(), string()) ->
  ok.

topic(_Prefix, _Channel, _Topic) ->
  'TODO'.

%% @doc List nicknames on specified channels.

-spec names(prefix(), [channel()]) ->
  ok.

names(_Prefix, _Channels) ->
  'TODO'.

%% @doc List nicknames on specified channels.

-spec names(prefix(), [channel()], server()) ->
  ok.

names(_Prefix, _Channels, _Server) ->
  'TODO'.

%% @doc List channels and their topics.

-spec list(prefix()) ->
  ok.

list(_Prefix) ->
  'TODO'.

%% @doc List channels and their topics.

-spec list(prefix(), [channel()]) ->
  ok.

list(_Prefix, _Channels) ->
  'TODO'.

%% @doc List channels and their topics.

-spec list(prefix(), [channel()], server()) ->
  ok.

list(_Prefix, _Channels, _Server) ->
  'TODO'.

%% @doc Invite user to a channel.

-spec invite(prefix(), nick(), channel()) ->
  ok.

invite(_Prefix, _Nick, _Channel) ->
  'TODO'.

%% @doc Kick users from channels.

-spec kick(prefix(), [channel()], [nick()]) ->
  ok.

kick(_Prefix, _Channels, _Nicks) ->
  'TODO'.

%% @doc Kick users from channels.

-spec kick(prefix(), [channel()], [nick()], string()) ->
  ok.

kick(_Prefix, _Channels, _Nicks, _Comment) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.3 sending messages {{{

%% @doc Send a message to user or channel.
%%
%% @TODO sending messages to `nick@server'
%% @TODO sending messages to `user%host@server'
%% @TODO sending messages to `user%host'
%% @TODO sending messages to `nick!user@host'
%% @TODO sending messages to `$*.fi' (server mask)
%% @TODO sending messages to `#*.edu' (client host mask)

-spec privmsg(prefix(), nick() | channel(), string()) ->
  ok.

privmsg(_Prefix, "#" ++ _ = _Channel, _Message) -> 'TODO';
privmsg(_Prefix, "+" ++ _ = _Channel, _Message) -> 'TODO';
privmsg(_Prefix, "!" ++ _ = _Channel, _Message) -> 'TODO';
privmsg(_Prefix, "&" ++ _ = _Channel, _Message) -> 'TODO';
privmsg(_Prefix, _Nick, _Message) ->
  'TODO'.

%% @doc Send a message to user or channel.
%%   Messages sent with <i>NOTICE</i> are never getting any automated
%%   response (e.g. delivery errors).

-spec notice(prefix(), nick() | channel(), string()) ->
  ok.

notice(_Prefix, "#" ++ _ = _Channel, _Message) -> 'TODO';
notice(_Prefix, "+" ++ _ = _Channel, _Message) -> 'TODO';
notice(_Prefix, "!" ++ _ = _Channel, _Message) -> 'TODO';
notice(_Prefix, "&" ++ _ = _Channel, _Message) -> 'TODO';
notice(_Prefix, _Nick, _Message) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.4 sending queries and commands {{{

%% @doc Request message-of-the-day from server.

-spec motd(prefix()) ->
  ok.

motd(_Prefix) ->
  'TODO'.

%% @doc Request message-of-the-day from specific server.

-spec motd(prefix(), server()) ->
  ok.

motd(_Prefix, _Server) ->
  'TODO'.

%% @doc Request statistics about IRC network users.

-spec lusers(prefix()) ->
  ok.

lusers(_Prefix) ->
  'TODO'.

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(prefix(), string()) ->
  ok.

lusers(_Prefix, _Mask) ->
  'TODO'.

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(prefix(), string(), server()) ->
  ok.

lusers(_Prefix, _Mask, _Server) ->
  'TODO'.

%% @doc Request version information from the server.

-spec version(prefix()) ->
  ok.

version(_Prefix) ->
  'TODO'.

%% @doc Request version information from the server.

-spec version(prefix(), server()) ->
  ok.

version(_Prefix, _Server) ->
  'TODO'.

%% @doc Request server statistics.
%% @see stats/3

-spec stats(prefix()) ->
  ok.

stats(_Prefix) ->
  'TODO'.

%% @doc Request server statistics.
%% @see stats/3

-spec stats(prefix(), string()) ->
  ok.

stats(_Prefix, _Query) ->
  'TODO'.

%% @doc Request server statistics.
%%   Standard stats are:
%%   <ul>
%%     <li><i>l</i> -- list of server's connections</li>
%%     <li><i>m</i> -- usage count of each command supported by the
%%         server</li>
%%     <li><i>o</i> -- list of privileged users</li>
%%     <li><i>u</i> -- server's uptime</li>
%%   </ul>

-spec stats(prefix(), string(), server()) ->
  ok.

stats(_Prefix, _Query, _Server) ->
  'TODO'.

%% @doc List servers known to the queried server.

-spec links(prefix()) ->
  ok.

links(_Prefix) ->
  'TODO'.

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.

-spec links(prefix(), string()) ->
  ok.

links(_Prefix, _Mask) ->
  'TODO'.

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.
%%   `Target' is a mask and the first matching server is expected to answer
%%   the query.

-spec links(prefix(), string(), string()) ->
  ok.

links(_Prefix, _Target, _Mask) ->
  'TODO'.

%% @doc Request current time from server.

-spec time(prefix()) ->
  ok.

time(_Prefix) ->
  'TODO'.

%% @doc Request current time from server.

-spec time(prefix(), server()) ->
  ok.

time(_Prefix, _Server) ->
  'TODO'.

%% @doc Try to establish connection to target server.

-spec connect(prefix(), server(), integer()) ->
  ok.

connect(_Prefix, _Target, _Port) ->
  'TODO'.

%% @doc Try to establish connection to target server.

-spec connect(prefix(), server(), integer(), server()) ->
  ok.

connect(_Prefix, _Target, _Port, _Server) ->
  'TODO'.

%% @doc List direct connections of local server.
%%   At least, this is what RFC 2812 recommends for <i>TRACE</i> command
%%   without arguments.

-spec trace(prefix()) ->
  ok.

trace(_Prefix) ->
  'TODO'.

%% @doc Trace connection to specified server.

-spec trace(prefix(), server()) ->
  ok.

trace(_Prefix, _Target) ->
  'TODO'.

%% @doc Request information about administrator of current server.

-spec admin(prefix()) ->
  ok.

admin(_Prefix) ->
  'TODO'.

%% @doc Request information about administrator of specified server.

-spec admin(prefix(), nick() | server()) ->
  ok.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
admin(_Prefix, _NickOrServer) ->
  'TODO'.

%% @doc Request information about the software server runs on.

-spec info(prefix()) ->
  ok.

info(_Prefix) ->
  'TODO'.

%% @doc Request information about the software server runs on.

-spec info(prefix(), nick() | server()) ->
  ok.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
info(_Prefix, _NickOrServer) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.5 service query and commands {{{

%% @doc List servers known to the queried server.

-spec servlist(prefix()) ->
  ok.

servlist(_Prefix) ->
  'TODO'.

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.

-spec servlist(prefix(), string()) ->
  ok.

servlist(_Prefix, _ServerMask) ->
  'TODO'.

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.
%%   `Type' limits the services to the matching ones.
%%
%% @see service/5

-spec servlist(prefix(), string(), string()) ->
  ok.

servlist(_Prefix, _ServerMask, _Type) ->
  'TODO'.

%% @doc Send a query to service.

-spec squery(prefix(), nick(), string()) ->
  ok.

squery(_Prefix, _Service, _Query) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.6 user based queries {{{

%% @doc List all visible users.

-spec who(prefix()) ->
  ok.

who(_Prefix) ->
  'TODO'.

%% @doc List all users matching the mask.

-spec who(prefix(), string()) ->
  ok.

who(_Prefix, _Mask) ->
  'TODO'.

%% @doc List all users matching the mask who are also operators.

-spec who(prefix(), string(), o) ->
  ok.

who(_Prefix, _Mask, o = _Limit) ->
  'TODO'.

%% @doc Request information about specified user(s).

-spec whois(prefix(), [string()]) ->
  ok.

whois(_Prefix, _UserMasks) ->
  'TODO'.

%% @doc Request information about specified user(s).

-spec whois(prefix(), server(), [string()]) ->
  ok.

whois(_Prefix, _Server, _UserMasks) ->
  'TODO'.

%% @doc Request information about nicknames that no longer exist.

-spec whowas(prefix(), [nick()]) ->
  ok.

whowas(_Prefix, _Nicks) ->
  'TODO'.

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas(prefix(), [nick()], integer()) ->
  ok.

whowas(_Prefix, _Nicks, _Count) ->
  'TODO'.

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas(prefix(), [nick()], integer(), server()) ->
  ok.

whowas(_Prefix, _Nicks, _Count, _Server) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 3.7 miscellaneous messages {{{

%% @doc Terminate connection between specified user and his server.

-spec kill(prefix(), nick(), string()) ->
  ok.

kill(_Prefix, _Nick, _Comment) ->
  'TODO'.

%% @doc Send <i>ping</i> message to server.

-spec ping(prefix(), server()) ->
  ok.

ping(_Prefix, _Target) ->
  'TODO'.

%% @doc Order a server to send <i>ping</i> message to server.

-spec ping(prefix(), server(), server()) ->
  ok.

ping(_Prefix, _Server, _Target) ->
  'TODO'.

%% @doc Send <i>pong</i> reply to server.

-spec pong(prefix(), server()) ->
  ok.

pong(_Prefix, _Responder) ->
  'TODO'.

%% @doc Send <i>pong</i> reply to server.

-spec pong(prefix(), server(), server()) ->
  ok.

pong(_Prefix, _Responder, _Target) ->
  'TODO'.

%% @doc Report error.
%%   This message is not intended to be sent from typical client.

-spec error(prefix(), string()) ->
  ok.

error(_Prefix, _Message) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% 4 optional features {{{

%% @doc Mark the user as being away from keyboard.

-spec away(prefix()) ->
  ok.

away(_Prefix) ->
  'TODO'.

%% @doc Mark the user as being away from keyboard.

-spec away(prefix(), string()) ->
  ok.

away(_Prefix, _Message) ->
  'TODO'.

%% @doc Force the server to reload configuration file.

-spec rehash(prefix()) ->
  ok.

rehash(_Prefix) ->
  'TODO'.

%% @doc Shutdown the server.

-spec die(prefix()) ->
  ok.

die(_Prefix) ->
  'TODO'.

%% @doc Restart the server

-spec restart(prefix()) ->
  ok.

restart(_Prefix) ->
  'TODO'.

%% @doc Call user logged in on local server to join IRC.

-spec summon(prefix(), nick()) ->
  ok.

summon(_Prefix, _User) ->
  'TODO'.

%% @doc Call user logged in on specified server to join IRC.

-spec summon(prefix(), nick(), server()) ->
  ok.

summon(_Prefix, _User, _Server) ->
  'TODO'.

%% @doc Call user logged in on specified server to join IRC channel.

-spec summon(prefix(), nick(), server(), channel()) ->
  ok.

summon(_Prefix, _User, _Server, _Channel) ->
  'TODO'.

%% @doc Request a list of logged in users.

-spec users(prefix()) ->
  ok.

users(_Prefix) ->
  'TODO'.

%% @doc Request a list of logged in users.

-spec users(prefix(), server()) ->
  ok.

users(_Prefix, _Server) ->
  'TODO'.

%% @doc Send a message to all users who have <i>+w</i> mode.

-spec wallops(prefix(), string()) ->
  ok.

wallops(_Prefix, _Message) ->
  'TODO'.

%% @doc List users' hosts.

-spec userhost(prefix(), [nick()]) ->
  ok.

%% up to 5 users, list is sent space-separated
userhost(_Prefix, _Nicks) ->
  'TODO'.

%% @doc Check if specified users are present on IRC.
%%   The query is processed by local server only and not passed further (but
%%   servers generally know global state, so the response should be accurate).

-spec ison(prefix(), [nick()]) ->
  ok.

%% list is sent space-separated
ison(_Prefix, _Nicks) ->
  'TODO'.

%% }}}
%%----------------------------------------------------------
%% server replies (numeric) {{{

%% TODO

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker