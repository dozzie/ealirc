%%%---------------------------------------------------------------------------
%%% @doc
%%%   IRC protocol strings.
%%%
%%%   Note that all functions defined here that return string with IRC command
%%%   produce the string <i>without</i> CR+LF line ending. To pass them to
%%%   server you still need to add it yourself.
%%% @end
%%%---------------------------------------------------------------------------

-module(ealirc_proto).

%% line processing
-export([encode/2, encode/3, decode/1]).
-export([encode_prefix/1, decode_prefix/1]).
-export([server_prefix/1, user_prefix/1, user_prefix/2, user_prefix/3]).
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

-type user_mode() :: ealirc:user_mode().
-type channel_mode() :: ealirc:channel_mode().

-type server_prefix() :: ealirc:server_prefix().
-type user_prefix() :: ealirc:user_prefix().

-record(msg, {prefix = none, cmd, args = []}).

%%%---------------------------------------------------------------------------

%% @doc Encode IRC command as a string, without line ending.

-spec encode(command(), [argument()]) ->
  {ok, string()} | {error, term()}.

encode(Cmd, Args) ->
  encode(none, Cmd, Args).

%% @doc Encode IRC command as a string, without line ending.

-spec encode(prefix(), command(), [argument()]) ->
  {ok, string()} | {error, term()}.

encode(Prefix, Cmd, Args) ->
  StrPrefix = case Prefix of
    none -> "";
    % TODO: check `Prefix' for validity
    _ when is_list(Prefix) -> ":" ++ Prefix ++ " "
  end,
  StrSuffix = case Args of
    [] -> "";
    _ when is_list(Args) -> " " ++ encode_args(Args)
  end,
  {ok, StrPrefix ++ Cmd ++ StrSuffix}.

%%----------------------------------------------------------
%% encoding helper {{{

%% @doc Encode arguments to IRC command.
%%   Result has no leading spaces and <i>is not</i> terminated with CR+LF.
%%
%% @TODO check if the `Args' is not longer than 15 entries
%% @TODO check if each of the `Args' is acceptable (i.e. CR and LF are
%%   prohibited entirely, leading ":" and " " are only allowed in the last
%%   argument)

-spec encode_args([string()]) ->
  string().

encode_args([] = _Args) ->
  "";
encode_args([String] = _Args) ->
  ":" ++ String;
encode_args([String | Rest] = _Args) ->
  String ++ " " ++ encode_args(Rest).

%% }}}
%%----------------------------------------------------------

%% @doc Decode line into a well-formed message.
%%   Function does not interpret or validate the message. The returned command
%%   is always upper case.
%%
%% @TODO split `Prefix' to tuple:
%%   <ul>
%%     <li>`servername' (contains `"."')</li>
%%     <li>`nick'</li>
%%     <li>`nick@host'</li>
%%     <li>`nick!user@host'</li>
%%   </ul>

-spec decode(string()) ->
  {ok, {prefix(), command(), [argument()]}} | {error, term()}.

decode(Line) ->
  try
    #msg{prefix = Prefix, cmd = Cmd, args = Args}
      = parse_prefix(strip_crlf(Line)),
    {ok, {Prefix, string:to_upper(Cmd), Args}}
  catch
    % XXX: errors if_clause and try_clause won't show up here
    error:{badmatch, _Value} ->
      {error, badarg};
    error:function_clause ->
      {error, badarg};
    error:{case_clause, _Value} ->
      {error, badarg}
  end.

%%----------------------------------------------------------
%% decoding helpers {{{

%% @doc Decode command prefix and pass the line further.

-spec parse_prefix(string()) ->
  #msg{}.

parse_prefix(":" ++ Rest = _Line) ->
  % TODO: validate prefix (servername | nick [["!" user] "@" host]
  {Prefix, Rest1} = space_split(Rest),
  {Cmd, Args} = parse_command(Rest1),
  #msg{prefix = Prefix, cmd = Cmd, args = Args};
parse_prefix(Line) when hd(Line) /= $  ->
  {Cmd, Args} = parse_command(Line),
  #msg{prefix = none, cmd = Cmd, args = Args}.

%% @doc Decode command and pass the line further.

-spec parse_command(string()) ->
  {string(), [string()]}.

parse_command([C1, C2, C3, $  | Rest] = _Line)
when C1 >= $0, C1 =< $9, C2 >= $0, C2 =< $9, C3 >= $0, C3 =< $9 ->
  Cmd = (C1 - $0) * 100 + (C2 - $0) * 10 + (C3 - $0),
  Args = parse_args(string:strip(Rest, left)),
  {Cmd, Args};
parse_command(Line) ->
  {Cmd, ArgsString} = space_split(Line),
  Args = parse_args(ArgsString),
  {Cmd, Args}.

%% @doc Decode arguments.

-spec parse_args(string()) ->
  [string()].

parse_args(":" ++ Arg = _Line) ->
  [Arg];
parse_args(Line) ->
  case space_split(Line) of
    {Arg, ""} -> [Arg];
    {Arg, Rest} -> [Arg | parse_args(Rest)]
  end.

%% }}}
%%----------------------------------------------------------
%% helpers for decoding helpers {{{

%% @doc Strip line ending, CR+LF or LF.

-spec strip_crlf(string()) ->
  string().

strip_crlf("\r\n") -> "";
strip_crlf("\n")   -> "";
strip_crlf("")     -> "";
strip_crlf([C | Rest]) -> [C | strip_crlf(Rest)].

%% @doc Split string into two substrings on first spaces sequence.

-spec space_split(string(), string()) ->
  {string(), string()}.

space_split("" = _String, Acc) ->
  {lists:reverse(Acc), ""}; % or `none'?
space_split(" " ++ Rest = _String, Acc) ->
  {lists:reverse(Acc), string:strip(Rest, left)};
space_split([C | Rest] = _String, Acc) ->
  space_split(Rest, [C | Acc]).

%% @doc Split string into two substrings on first spaces sequence.

-spec space_split(string()) ->
  {string(), string()}.

space_split(String) ->
  space_split(String, "").

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%% @doc Encode prefix tuple into string suitable to prepend to IRC message.
%%
%%   Typically you want to call {@link user_prefix/3} or
%%   {@link server_prefix/1}.
%%
%% @see user_prefix/1
%% @see user_prefix/2
%% @see user_prefix/3
%% @see server_prefix/1

-spec encode_prefix(server_prefix() | user_prefix()) ->
  string().

encode_prefix({server, Server} = _Prefix) ->
  Server;
encode_prefix({user, Nick, undefined, undefined} = _Prefix) ->
  Nick;
encode_prefix({user, Nick, undefined, Host} = _Prefix) ->
  Nick ++ "@" ++ Host;
encode_prefix({user, Nick, User, Host} = _Prefix) ->
  Nick ++ "!" ++ User ++ "@" ++ Host.

%% @doc Encode server prefix for IRC message.

-spec server_prefix(server()) ->
  string().

server_prefix(Server) ->
  encode_prefix({server, Server}).

%% @doc Encode user prefix for IRC message.

-spec user_prefix(nick()) ->
  string().

user_prefix(Nick) ->
  encode_prefix({user, Nick, undefined, undefined}).

%% @doc Encode user prefix for IRC message.

-spec user_prefix(nick(), string()) ->
  string().

user_prefix(Nick, Host) ->
  encode_prefix({user, Nick, undefined, Host}).

%% @doc Encode user prefix for IRC message.
%%
%%   `User' is a username local to `Host'.

-spec user_prefix(nick(), string(), string()) ->
  string().

user_prefix(Nick, User, Host) ->
  encode_prefix({user, Nick, User, Host}).

%% @doc Decode IRC message prefix.
%%
%%   There are four forms of the prefixes:
%%   <ul>
%%     <li>`servername' (contains `"."')</li>
%%     <li>`nick'</li>
%%     <li>`nick@host'</li>
%%     <li>`nick!user@host'</li>
%%   </ul>

-spec decode_prefix(string()) ->
  server_prefix() | user_prefix().

decode_prefix(_Prefix) ->
  'TODO'.

%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% 3.1 connection registration {{{

%% @doc Password message.

-spec pass(prefix(), string()) ->
  {ok, string()} | {error, term()}.

pass(Prefix, Password) ->
  encode(Prefix, "PASS", [Password]).

%% @doc Nick change/set request.

-spec nick(prefix(), nick()) ->
  {ok, string()} | {error, term()}.

nick(Prefix, Nick) ->
  encode(Prefix, "NICK", [Nick]).

%% @doc Passing username and realname.

-spec user(prefix(), nick(), none | invisible | wallops | invisible_wallops,
           string()) ->
  {ok, string()} | {error, term()}.

user(Prefix, User, Mode, RealName) ->
  user(Prefix, User, Mode, "*", RealName).

%% @doc Passing username and realname.

-spec user(prefix(), nick(), none | invisible | wallops | invisible_wallops,
           string(), string()) ->
  {ok, string()} | {error, term()}.

user(Prefix, User, Mode, Unused, RealName) ->
  ModeStr = case Mode of
    none -> "0";
    wallops -> "4";
    invisible -> "8";
    invisible_wallops -> "12"
  end,
  encode(Prefix, "USER", [User, ModeStr, Unused, RealName]).

%% @doc Obtain operator privileges.

-spec oper(prefix(), nick(), string()) ->
  {ok, string()} | {error, term()}.

oper(Prefix, User, Password) ->
  encode(Prefix, "OPER", [User, Password]).

%% @doc Set user or channel mode.
%%
%%   <b>WARNING</b>: be careful when mixing modes with and without arguments
%%   (channel modes only). There's a risk of passing argument to wrong mode if
%%   you incidentally omitted argument to, for example, `"+o"' or `"+I"'.
%%
%%   Usage examples:
%%   ```
%%   ealirc_proto:mode(none, "WiZ", ["+i", "-w"]).
%%   ealirc_proto:mode(none, "#42", ["+m", {"+k", "secret"}, {"-o", "Trillian"}]).
%%   '''
%%
%% @spec mode(prefix(), nick() | channel(),
%%            [user_mode()] | [channel_mode()]) ->
%%   ok

-spec mode(prefix(), nick(),    [user_mode()])    -> ok;
          (prefix(), channel(), [channel_mode()]) -> ok.

mode(Prefix, [T | _] = Channel, Modes)
when T == $#; T == $+; T == $!; T == $& ->
  {ModesArg, Args} = combine_channel_modes(Modes, none, [], []),
  encode(Prefix, "MODE", [Channel, ModesArg | Args]);
mode(Prefix, Nick, Modes) ->
  ModesArg = combine_user_modes(Modes, none, []),
  encode(Prefix, "MODE", [Nick, ModesArg]).

%%----------------------------------------------------------
%% combining modes into single string {{{

%% @doc Combine modes specific for user into a nice string sequence.

-spec combine_user_modes([string()], char() | none, iolist()) ->
  string().

combine_user_modes([] = _Modes, _O, ModeAcc) ->
  lists:flatten(ModeAcc);

%% mode operation (set/unset) the same as previous one
combine_user_modes([[O, M] | Rest] = _Modes, O, ModeAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_user_modes(Rest, O, [ModeAcc, M]);

%% mode operation changed since the last time
combine_user_modes([[O, M] | Rest] = _Modes, _OOld, ModeAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_user_modes(Rest, O, [ModeAcc, [O, M]]).

%% @doc Combine modes specific for channel into a nice string sequence + list
%%   of arguments.

-spec combine_channel_modes([string() | {string(), string()}], char() | none,
                         iolist(), [string()]) ->
  {string(), [string()]}.

combine_channel_modes([] = _Modes, _O, ModeAcc, ArgsAcc) ->
  {lists:flatten(ModeAcc), lists:reverse(ArgsAcc)};

%% mode operation (set/unset) the same as previous one
combine_channel_modes([[O, M] | Rest] = _Modes, O, ModeAcc, ArgsAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_channel_modes(Rest, O, [ModeAcc, M], ArgsAcc);

%% mode operation changed since the last time
combine_channel_modes([[O, M] | Rest] = _Modes, _OOld, ModeAcc, ArgsAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_channel_modes(Rest, O, [ModeAcc, [O, M]], ArgsAcc);

%% mode operation (set/unset) the same as previous one
combine_channel_modes([{[O, M], Arg} | Rest] = _Modes, O, ModeAcc, ArgsAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_channel_modes(Rest, O, [ModeAcc, M], [Arg | ArgsAcc]);

%% mode operation changed since the last time
combine_channel_modes([{[O, M], Arg} | Rest] = _Modes, _OOld, ModeAcc, ArgsAcc)
when (O == $+ orelse O == $-), M /= $ , M /= $+, M /= $- ->
  combine_channel_modes(Rest, O, [ModeAcc, [O, M]], [Arg | ArgsAcc]).

%% }}}
%%----------------------------------------------------------

%% @doc Service registration message.

-spec service(prefix(), nick(), string(), string(), string()) ->
  {ok, string()} | {error, term()}.

service(Prefix, Nick, Distribution, Type, Info) ->
  service(Prefix, Nick, "*", Distribution, Type, "*", Info).

%% @doc Service registration message.

-spec service(prefix(), nick(), string(), string(), string(), string(),
              string()) ->
  {ok, string()} | {error, term()}.

service(Prefix, Nick, Reserved, Distribution, Type, Reserved1, Info) ->
  encode(Prefix, "SERVICE",
                 [Nick, Reserved, Distribution, Type, Reserved1, Info]).

%% @doc Quit message.

-spec quit(prefix()) ->
  {ok, string()} | {error, term()}.

quit(Prefix) ->
  encode(Prefix, "QUIT", []).

%% @doc Quit message.

-spec quit(prefix(), string()) ->
  {ok, string()} | {error, term()}.

quit(Prefix, Message) ->
  encode(Prefix, "QUIT", [Message]).

%% @doc Disconnect server links.

-spec squit(prefix(), string(), string()) ->
  {ok, string()} | {error, term()}.

squit(Prefix, Server, Comment) ->
  encode(Prefix, "SQUIT", [Server, Comment]).

%% }}}
%%----------------------------------------------------------
%% 3.2 channel operations {{{

%% @doc Join provided channels.
%%   Channel may be provided as a name or a tuple of `{Chan,Key}', containing
%%   channel key.
%%
%%   Special case of integer 0 is for <i>JOIN 0</i> IRC command, which leaves
%%   all the channels user is in.

-spec join(prefix(), [channel()] | [{channel(), string()}] | 0) ->
  {ok, string()} | {error, term()}.

join(Prefix, 0 = _Channels) ->
  encode(Prefix, "JOIN", ["0"]);
join(Prefix, [{_,_} | _] = Channels) ->
  ChanList = string:join([C || {C,_} <- Channels], ","),
  KeysList = string:join([K || {_,K} <- Channels], ","),
  encode(Prefix, "JOIN", [ChanList, KeysList]);
join(Prefix, [_ | _] = Channels) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "JOIN", [ChanList]).

%% @doc Leave specified channels.

-spec part(prefix(), [channel()]) ->
  {ok, string()} | {error, term()}.

part(Prefix, Channels) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "PART", [ChanList]).

%% @doc Leave specified channels.

-spec part(prefix(), [channel()], string()) ->
  {ok, string()} | {error, term()}.

part(Prefix, Channels, Message) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "PART", [ChanList, Message]).

%% @doc Request a topic for the channel.

-spec topic(prefix(), channel()) ->
  {ok, string()} | {error, term()}.

topic(Prefix, Channel) ->
  encode(Prefix, "TOPIC", [Channel]).

%% @doc Set topic for specified channel.

-spec topic(prefix(), channel(), string()) ->
  {ok, string()} | {error, term()}.

topic(Prefix, Channel, Topic) ->
  encode(Prefix, "TOPIC", [Channel, Topic]).

%% @doc List nicknames on specified channels.

-spec names(prefix(), [channel()]) ->
  {ok, string()} | {error, term()}.

names(Prefix, Channels) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "NAMES", [ChanList]).

%% @doc List nicknames on specified channels.

-spec names(prefix(), [channel()], server()) ->
  {ok, string()} | {error, term()}.

names(Prefix, Channels, Server) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "NAMES", [ChanList, Server]).

%% @doc List channels and their topics.

-spec list(prefix()) ->
  {ok, string()} | {error, term()}.

list(Prefix) ->
  encode(Prefix, "LIST", []).

%% @doc List channels and their topics.

-spec list(prefix(), [channel()]) ->
  {ok, string()} | {error, term()}.

list(Prefix, Channels) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "LIST", [ChanList]).

%% @doc List channels and their topics.

-spec list(prefix(), [channel()], server()) ->
  {ok, string()} | {error, term()}.

list(Prefix, Channels, Server) ->
  ChanList = string:join(Channels, ","),
  encode(Prefix, "LIST", [ChanList, Server]).

%% @doc Invite user to a channel.

-spec invite(prefix(), nick(), channel()) ->
  {ok, string()} | {error, term()}.

invite(Prefix, Nick, Channel) ->
  encode(Prefix, "INVITE", [Nick, Channel]).

%% @doc Kick users from channels.

-spec kick(prefix(), [channel()], [nick()]) ->
  {ok, string()} | {error, term()}.

kick(Prefix, Channels, Nicks) ->
  ChanList = string:join(Channels, ","),
  NickList = string:join(Nicks, ","),
  encode(Prefix, "KICK", [ChanList, NickList]).

%% @doc Kick users from channels.

-spec kick(prefix(), [channel()], [nick()], string()) ->
  {ok, string()} | {error, term()}.

kick(Prefix, Channels, Nicks, Comment) ->
  ChanList = string:join(Channels, ","),
  NickList = string:join(Nicks, ","),
  encode(Prefix, "KICK", [ChanList, NickList, Comment]).

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
  {ok, string()} | {error, term()}.

privmsg(Prefix, [T | _] = Channel, Message)
when T == $#; T == $+; T == $!; T == $& ->
  encode(Prefix, "PRIVMSG", [Channel, Message]);
privmsg(Prefix, Nick, Message) ->
  encode(Prefix, "PRIVMSG", [Nick, Message]).

%% @doc Send a message to user or channel.
%%   Messages sent with <i>NOTICE</i> are never getting any automated
%%   response (e.g. delivery errors).
%%
%% @see privmsg/3

-spec notice(prefix(), nick() | channel(), string()) ->
  {ok, string()} | {error, term()}.

notice(Prefix, [T | _] = Channel, Message)
when T == $#; T == $+; T == $!; T == $& ->
  encode(Prefix, "NOTICE", [Channel, Message]);
notice(Prefix, Nick, Message) ->
  encode(Prefix, "NOTICE", [Nick, Message]).

%% }}}
%%----------------------------------------------------------
%% 3.4 sending queries and commands {{{

%% @doc Request message-of-the-day from server.

-spec motd(prefix()) ->
  {ok, string()} | {error, term()}.

motd(Prefix) ->
  encode(Prefix, "MOTD", []).

%% @doc Request message-of-the-day from specific server.

-spec motd(prefix(), server()) ->
  {ok, string()} | {error, term()}.

motd(Prefix, Server) ->
  encode(Prefix, "MOTD", [Server]).

%% @doc Request statistics about IRC network users.

-spec lusers(prefix()) ->
  {ok, string()} | {error, term()}.

lusers(Prefix) ->
  encode(Prefix, "LUSERS", []).

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(prefix(), string()) ->
  {ok, string()} | {error, term()}.

lusers(Prefix, Mask) ->
  encode(Prefix, "LUSERS", [Mask]).

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(prefix(), string(), server()) ->
  {ok, string()} | {error, term()}.

lusers(Prefix, Mask, Server) ->
  encode(Prefix, "LUSERS", [Mask, Server]).

%% @doc Request version information from the server.

-spec version(prefix()) ->
  {ok, string()} | {error, term()}.

version(Prefix) ->
  encode(Prefix, "VERSION", []).

%% @doc Request version information from the server.

-spec version(prefix(), server()) ->
  {ok, string()} | {error, term()}.

version(Prefix, Server) ->
  encode(Prefix, "VERSION", [Server]).

%% @doc Request server statistics.
%%
%% @see stats/3

-spec stats(prefix()) ->
  {ok, string()} | {error, term()}.

stats(Prefix) ->
  encode(Prefix, "STATS", []).

%% @doc Request server statistics.
%%
%% @see stats/3

-spec stats(prefix(), string()) ->
  {ok, string()} | {error, term()}.

stats(Prefix, Query) ->
  encode(Prefix, "STATS", [Query]).

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
  {ok, string()} | {error, term()}.

stats(Prefix, Query, Server) ->
  encode(Prefix, "STATS", [Query, Server]).

%% @doc List servers known to the queried server.

-spec links(prefix()) ->
  {ok, string()} | {error, term()}.

links(Prefix) ->
  encode(Prefix, "LINKS", []).

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.

-spec links(prefix(), string()) ->
  {ok, string()} | {error, term()}.

links(Prefix, Mask) ->
  encode(Prefix, "LINKS", [Mask]).

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.
%%   `Target' is a mask and the first matching server is expected to answer
%%   the query.

-spec links(prefix(), string(), string()) ->
  {ok, string()} | {error, term()}.

links(Prefix, Target, Mask) ->
  encode(Prefix, "LINKS", [Target, Mask]).

%% @doc Request current time from server.

-spec time(prefix()) ->
  {ok, string()} | {error, term()}.

time(Prefix) ->
  encode(Prefix, "TIME", []).

%% @doc Request current time from server.

-spec time(prefix(), server()) ->
  {ok, string()} | {error, term()}.

time(Prefix, Server) ->
  encode(Prefix, "TIME", [Server]).

%% @doc Try to establish connection to target server.

-spec connect(prefix(), server(), integer()) ->
  {ok, string()} | {error, term()}.

connect(Prefix, Target, Port) ->
  encode(Prefix, "CONNECT", [Target, integer_to_list(Port)]).

%% @doc Try to establish connection to target server.

-spec connect(prefix(), server(), integer(), server()) ->
  {ok, string()} | {error, term()}.

connect(Prefix, Target, Port, Server) ->
  encode(Prefix, "CONNECT", [Target, integer_to_list(Port), Server]).

%% @doc List direct connections of local server.
%%   At least, this is what RFC 2812 recommends for <i>TRACE</i> command
%%   without arguments.

-spec trace(prefix()) ->
  {ok, string()} | {error, term()}.

trace(Prefix) ->
  encode(Prefix, "TRACE", []).

%% @doc Trace connection to specified server.

-spec trace(prefix(), server()) ->
  {ok, string()} | {error, term()}.

trace(Prefix, Target) ->
  encode(Prefix, "TRACE", [Target]).

%% @doc Request information about administrator of current server.

-spec admin(prefix()) ->
  {ok, string()} | {error, term()}.

admin(Prefix) ->
  encode(Prefix, "ADMIN", []).

%% @doc Request information about administrator of specified server.

-spec admin(prefix(), nick() | server()) ->
  {ok, string()} | {error, term()}.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
admin(Prefix, NickOrServer) ->
  encode(Prefix, "ADMIN", [NickOrServer]).

%% @doc Request information about the software server runs on.

-spec info(prefix()) ->
  {ok, string()} | {error, term()}.

info(Prefix) ->
  encode(Prefix, "INFO", []).

%% @doc Request information about the software server runs on.

-spec info(prefix(), nick() | server()) ->
  {ok, string()} | {error, term()}.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
info(Prefix, NickOrServer) ->
  encode(Prefix, "INFO", [NickOrServer]).

%% }}}
%%----------------------------------------------------------
%% 3.5 service query and commands {{{

%% @doc List servers known to the queried server.

-spec servlist(prefix()) ->
  {ok, string()} | {error, term()}.

servlist(Prefix) ->
  encode(Prefix, "SERVLIST", []).

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.

-spec servlist(prefix(), string()) ->
  {ok, string()} | {error, term()}.

servlist(Prefix, ServerMask) ->
  encode(Prefix, "SERVLIST", [ServerMask]).

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.
%%   `Type' limits the services to the matching ones.
%%
%% @see service/5

-spec servlist(prefix(), string(), string()) ->
  {ok, string()} | {error, term()}.

servlist(Prefix, ServerMask, Type) ->
  encode(Prefix, "SERVLIST", [ServerMask, Type]).

%% @doc Send a query to service.

-spec squery(prefix(), nick(), string()) ->
  {ok, string()} | {error, term()}.

squery(Prefix, Service, Query) ->
  encode(Prefix, "SERVLIST", [Service, Query]).

%% }}}
%%----------------------------------------------------------
%% 3.6 user based queries {{{

%% @doc List all visible users.

-spec who(prefix()) ->
  {ok, string()} | {error, term()}.

who(Prefix) ->
  encode(Prefix, "WHO", []).

%% @doc List all users matching the mask.

-spec who(prefix(), string()) ->
  {ok, string()} | {error, term()}.

who(Prefix, Mask) ->
  encode(Prefix, "WHO", [Mask]).

%% @doc List all users matching the mask who are also operators.

-spec who(prefix(), string(), o) ->
  {ok, string()} | {error, term()}.

who(Prefix, Mask, o = _Limit) ->
  encode(Prefix, "WHO", [Mask, "o"]).

%% @doc Request information about specified user(s).

-spec whois(prefix(), [string()]) ->
  {ok, string()} | {error, term()}.

whois(Prefix, UserMasks) ->
  encode(Prefix, "WHOIS", [UserMasks]).

%% @doc Request information about specified user(s).

-spec whois(prefix(), server(), [string()]) ->
  {ok, string()} | {error, term()}.

whois(Prefix, Server, UserMasks) ->
  UserMaskList = string:join(UserMasks, ","),
  encode(Prefix, "WHOIS", [Server, UserMaskList]).

%% @doc Request information about nicknames that no longer exist.

-spec whowas(prefix(), [nick()]) ->
  {ok, string()} | {error, term()}.

whowas(Prefix, Nicks) ->
  NickList = string:join(Nicks, ","),
  encode(Prefix, "WHOWAS", [NickList]).

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas(prefix(), [nick()], integer()) ->
  {ok, string()} | {error, term()}.

whowas(Prefix, Nicks, Count) ->
  NickList = string:join(Nicks, ","),
  encode(Prefix, "WHOWAS", [NickList, integer_to_list(Count)]).

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas(prefix(), [nick()], integer(), server()) ->
  {ok, string()} | {error, term()}.

whowas(Prefix, Nicks, Count, Server) ->
  NickList = string:join(Nicks, ","),
  encode(Prefix, "WHOWAS", [NickList, integer_to_list(Count), Server]).

%% }}}
%%----------------------------------------------------------
%% 3.7 miscellaneous messages {{{

%% @doc Terminate connection between specified user and his server.

-spec kill(prefix(), nick(), string()) ->
  {ok, string()} | {error, term()}.

kill(Prefix, Nick, Comment) ->
  encode(Prefix, "KILL", [Nick, Comment]).

%% @doc Send <i>ping</i> message to server.

-spec ping(prefix(), server()) ->
  {ok, string()} | {error, term()}.

ping(Prefix, Target) ->
  encode(Prefix, "PING", [Target]).

%% @doc Order a server to send <i>ping</i> message to server.

-spec ping(prefix(), server(), server()) ->
  {ok, string()} | {error, term()}.

ping(Prefix, Server, Target) ->
  encode(Prefix, "PING", [Server, Target]).

%% @doc Send <i>pong</i> reply to server.

-spec pong(prefix(), server()) ->
  {ok, string()} | {error, term()}.

pong(Prefix, Responder) ->
  encode(Prefix, "PONG", [Responder]).

%% @doc Send <i>pong</i> reply to server.

-spec pong(prefix(), server(), server()) ->
  {ok, string()} | {error, term()}.

pong(Prefix, Responder, Target) ->
  encode(Prefix, "PONG", [Responder, Target]).

%% @doc Report error.
%%   This message is not intended to be sent from typical client.

-spec error(prefix(), string()) ->
  {ok, string()} | {error, term()}.

error(Prefix, Message) ->
  encode(Prefix, "ERROR", [Message]).

%% }}}
%%----------------------------------------------------------
%% 4 optional features {{{

%% @doc Mark the user as being away from keyboard.

-spec away(prefix()) ->
  {ok, string()} | {error, term()}.

away(Prefix) ->
  encode(Prefix, "AWAY", []).

%% @doc Mark the user as being away from keyboard.

-spec away(prefix(), string()) ->
  {ok, string()} | {error, term()}.

away(Prefix, Message) ->
  encode(Prefix, "AWAY", [Message]).

%% @doc Force the server to reload configuration file.

-spec rehash(prefix()) ->
  {ok, string()} | {error, term()}.

rehash(Prefix) ->
  encode(Prefix, "REHASH", []).

%% @doc Shutdown the server.

-spec die(prefix()) ->
  {ok, string()} | {error, term()}.

die(Prefix) ->
  encode(Prefix, "DIE", []).

%% @doc Restart the server

-spec restart(prefix()) ->
  {ok, string()} | {error, term()}.

restart(Prefix) ->
  encode(Prefix, "RESTART", []).

%% @doc Call user logged in on local server to join IRC.

-spec summon(prefix(), nick()) ->
  {ok, string()} | {error, term()}.

summon(Prefix, User) ->
  encode(Prefix, "SUMMON", [User]).

%% @doc Call user logged in on specified server to join IRC.

-spec summon(prefix(), nick(), server()) ->
  {ok, string()} | {error, term()}.

summon(Prefix, User, Server) ->
  encode(Prefix, "SUMMON", [User, Server]).

%% @doc Call user logged in on specified server to join IRC channel.

-spec summon(prefix(), nick(), server(), channel()) ->
  {ok, string()} | {error, term()}.

summon(Prefix, User, Server, Channel) ->
  encode(Prefix, "SUMMON", [User, Server, Channel]).

%% @doc Request a list of logged in users.

-spec users(prefix()) ->
  {ok, string()} | {error, term()}.

users(Prefix) ->
  encode(Prefix, "USERS", []).

%% @doc Request a list of logged in users.

-spec users(prefix(), server()) ->
  {ok, string()} | {error, term()}.

users(Prefix, Server) ->
  encode(Prefix, "USERS", [Server]).

%% @doc Send a message to all users who have <i>+w</i> mode.

-spec wallops(prefix(), string()) ->
  {ok, string()} | {error, term()}.

wallops(Prefix, Message) ->
  encode(Prefix, "WALLOPS", [Message]).

%% @doc List users' hosts.

-spec userhost(prefix(), [nick()]) ->
  {ok, string()} | {error, term()}.

%% up to 5 users, list is sent space-separated
userhost(Prefix, Nicks) when is_list(hd(Nicks)), length(Nicks) =< 5 ->
  encode(Prefix, "USERHOST", Nicks).

%% @doc Check if specified users are present on IRC.
%%   The query is processed by local server only and not passed further (but
%%   servers generally know global state, so the response should be accurate).

-spec ison(prefix(), [nick()]) ->
  {ok, string()} | {error, term()}.

%% list is sent space-separated
ison(Prefix, Nicks) when is_list(hd(Nicks)) ->
  encode(Prefix, "ISON", Nicks).

%% }}}
%%----------------------------------------------------------
%% server replies (numeric) {{{

%% TODO

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
