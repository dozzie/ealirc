%%%---------------------------------------------------------------------------
%%% @doc
%%%   IRC protocol strings.
%%%
%%%   Note that all functions defined here that return string with IRC command
%%%   produce the string <i>without</i> CR+LF line ending. To pass them to
%%%   server you still need to add it yourself.
%%%
%%%   Note also that the functions (except for {@link encode/3}) only return
%%%   raw IRC command, without prefix. Should you need to prepend the prefix,
%%%   you can use {@link server_prefix/1} or {@link user_prefix/3}.
%%%
%%%   ```
%%%   Prefix = ealirc_proto:user_prefix("Trillian", "irc-shell.example.net"),
%%%   {ok, Message} = ealirc_proto:away("went to lunch"),
%%%   gen_tcp:send(IRC, [":", Prefix, " ", Message]).
%%%   % ":Trillian@irc-shell.example.net AWAY :went to lunch"
%%%   '''
%%% @end
%%%---------------------------------------------------------------------------

-module(ealirc_proto).

%% line processing
-export([encode/2, encode/3, decode/1]).
-export([encode_prefix/1, decode_prefix/1]).
-export([server_prefix/1, user_prefix/1, user_prefix/2, user_prefix/3]).
%% RFC 2812 messages
%% http://tools.ietf.org/rfc/rfc2812.txt
-export([pass/1, nick/1, user/3, user/4, oper/2, mode/2, service/4, service/6]).
-export([quit/0, quit/1, squit/2]).
-export([join/1, part/1, part/2, topic/1, topic/2]). % also mode/2
-export([names/1, names/2, list/0, list/1, list/2]).
-export([invite/2, kick/2, kick/3]).
-export([privmsg/2, notice/2]).
-export([motd/0, motd/1, lusers/0, lusers/1, lusers/2, version/0, version/1]).
-export([stats/0, stats/1, stats/2, links/0, links/1, links/2]).
-export([time/0, time/1, connect/2, connect/3, trace/0, trace/1]).
-export([admin/0, admin/1, info/0, info/1]).
-export([servlist/0, servlist/1, servlist/2, squery/2]).
-export([who/0, who/1, who/2, whois/1, whois/2]).
-export([whowas/1, whowas/2, whowas/3]).
-export([kill/2, ping/1, ping/2, pong/1, pong/2, error/1]).
-export([away/0, away/1, rehash/0, die/0, restart/0]).
-export([summon/1, summon/2, summon/3]).
-export([users/0, users/1]).
-export([wallops/1]).
-export([userhost/1, ison/1]).

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
    _ -> ":" ++ encode_prefix(Prefix) ++ " "
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
  #msg{prefix = decode_prefix(Prefix), cmd = Cmd, args = Args};
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
%%   Returned string <i>does not</i> start with `":"' nor ends with space, you
%%   need to add it by yourself.

-spec server_prefix(server()) ->
  string().

server_prefix(Server) ->
  encode_prefix({server, Server}).

%% @doc Encode user prefix for IRC message.
%%   Returned string <i>does not</i> start with `":"' nor ends with space, you
%%   need to add it by yourself.

-spec user_prefix(nick()) ->
  string().

user_prefix(Nick) ->
  encode_prefix({user, Nick, undefined, undefined}).

%% @doc Encode user prefix for IRC message.
%%   Returned string <i>does not</i> start with `":"' nor ends with space, you
%%   need to add it by yourself.

-spec user_prefix(nick(), string() | undefined) ->
  string().

user_prefix(Nick, Host) ->
  encode_prefix({user, Nick, undefined, Host}).

%% @doc Encode user prefix for IRC message.
%%   Returned string <i>does not</i> start with `":"' nor ends with space, you
%%   need to add it by yourself.
%%
%%   `User' is a username local to `Host'.

-spec user_prefix(nick(), string() | undefined, string() | undefined) ->
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

decode_prefix(Prefix) ->
  parse_prefix_nick_or_server(Prefix, []).

%%----------------------------------------------------------
%% prefix decoding helper {{{

%% @doc Extract user or server prefix structure from prefix string.

-spec parse_prefix_nick_or_server(string(), string()) ->
  server_prefix() | user_prefix().

%% still can't tell whether nick or server
parse_prefix_nick_or_server([C | Rest] = _Prefix, Acc)
when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C == $- ->
  parse_prefix_nick_or_server(Rest, [C | Acc]);

%% this surely is prefix of some user (hostname can't have this in name)
parse_prefix_nick_or_server([C | Rest] = _Prefix, Acc)
when C == $[; C == $]; C == $\\; C == $`; C == $_; C == $^;
     C == ${; C == $}; C == $| ->
  parse_prefix_nick(Rest, [C | Acc]);

%% `nick!user@host' or `nick@host'
parse_prefix_nick_or_server([C | _Rest] = Prefix, Acc) when C == $!; C == $@ ->
  parse_prefix_nick(Prefix, Acc);

%% this surely is host, as nicks can't contain period
parse_prefix_nick_or_server("." ++ _Rest = Prefix, Acc) ->
  {server, lists:reverse(Acc) ++ Prefix};

%% still could be nick or server, but end of prefix occurred => it's a nick
parse_prefix_nick_or_server([] = _Prefix, Acc) ->
  Nick = lists:reverse(Acc),
  {user, Nick, undefined, undefined}.

%% @doc Extract nick, user and host parts from prefix.

-spec parse_prefix_nick(string(), string()) ->
  user_prefix().

%% `nick', not caught by parse_prefix_nick_or_server/1 because of []\`_^{}|
%% characters
parse_prefix_nick([] = _Prefix, Acc) ->
  Nick = lists:reverse(Acc),
  {user, Nick, undefined, undefined};

%% `nick@host'
parse_prefix_nick("@" ++ Host, Acc) ->
  Nick = lists:reverse(Acc),
  {user, Nick, undefined, Host};

%% `nick!user@host'
parse_prefix_nick("!" ++ Rest, Acc) ->
  [User, Host] = string:tokens(Rest, "@"),
  Nick = lists:reverse(Acc),
  {user, Nick, User, Host};

%% nick not finished yet
parse_prefix_nick([C | Rest] = _Prefix, Acc)
when C >= $a, C =< $z; C >= $A, C =< $Z; C >= $0, C =< $9; C == $-;
     C == $[; C == $]; C == $\\; C == $`; C == $_; C == $^;
     C == ${; C == $}; C == $| ->
  parse_prefix_nick(Rest, [C | Acc]).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% 3.1 connection registration {{{

%% @doc Password message.

-spec pass(string()) ->
  {ok, string()} | {error, term()}.

pass(Password) ->
  encode("PASS", [Password]).

%% @doc Nick change/set request.

-spec nick(nick()) ->
  {ok, string()} | {error, term()}.

nick(Nick) ->
  encode("NICK", [Nick]).

%% @doc Passing username and realname.

-spec user(nick(), none | invisible | wallops | invisible_wallops,
           string()) ->
  {ok, string()} | {error, term()}.

user(User, Mode, RealName) ->
  user(User, Mode, "*", RealName).

%% @doc Passing username and realname.

-spec user(nick(), none | invisible | wallops | invisible_wallops,
           string(), string()) ->
  {ok, string()} | {error, term()}.

user(User, Mode, Unused, RealName) ->
  ModeStr = case Mode of
    none -> "0";
    wallops -> "4";
    invisible -> "8";
    invisible_wallops -> "12"
  end,
  encode("USER", [User, ModeStr, Unused, RealName]).

%% @doc Obtain operator privileges.

-spec oper(nick(), string()) ->
  {ok, string()} | {error, term()}.

oper(User, Password) ->
  encode("OPER", [User, Password]).

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
%% @spec mode(nick() | channel(), [user_mode()] | [channel_mode()]) ->
%%   ok

-spec mode(nick(),    [user_mode()])    -> ok;
          (channel(), [channel_mode()]) -> ok.

mode([T | _] = Channel, Modes)
when T == $#; T == $+; T == $!; T == $& ->
  {ModesArg, Args} = combine_channel_modes(Modes, none, [], []),
  encode("MODE", [Channel, ModesArg | Args]);
mode(Nick, Modes) ->
  ModesArg = combine_user_modes(Modes, none, []),
  encode("MODE", [Nick, ModesArg]).

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

-spec service(nick(), string(), string(), string()) ->
  {ok, string()} | {error, term()}.

service(Nick, Distribution, Type, Info) ->
  service(Nick, "*", Distribution, Type, "*", Info).

%% @doc Service registration message.

-spec service(nick(), string(), string(), string(), string(),
              string()) ->
  {ok, string()} | {error, term()}.

service(Nick, Reserved, Distribution, Type, Reserved1, Info) ->
  encode("SERVICE",
                 [Nick, Reserved, Distribution, Type, Reserved1, Info]).

%% @doc Quit message.

-spec quit() ->
  {ok, string()} | {error, term()}.

quit() ->
  encode("QUIT", []).

%% @doc Quit message.

-spec quit(string()) ->
  {ok, string()} | {error, term()}.

quit(Message) ->
  encode("QUIT", [Message]).

%% @doc Disconnect server links.

-spec squit(string(), string()) ->
  {ok, string()} | {error, term()}.

squit(Server, Comment) ->
  encode("SQUIT", [Server, Comment]).

%% }}}
%%----------------------------------------------------------
%% 3.2 channel operations {{{

%% @doc Join provided channels.
%%   Channel may be provided as a name or a tuple of `{Chan,Key}', containing
%%   channel key.
%%
%%   Special case of integer 0 is for <i>JOIN 0</i> IRC command, which leaves
%%   all the channels user is in.

-spec join([channel()] | [{channel(), string()}] | 0) ->
  {ok, string()} | {error, term()}.

join(0 = _Channels) ->
  encode("JOIN", ["0"]);
join([{_,_} | _] = Channels) ->
  ChanList = string:join([C || {C,_} <- Channels], ","),
  KeysList = string:join([K || {_,K} <- Channels], ","),
  encode("JOIN", [ChanList, KeysList]);
join([_ | _] = Channels) ->
  ChanList = string:join(Channels, ","),
  encode("JOIN", [ChanList]).

%% @doc Leave specified channels.

-spec part([channel()]) ->
  {ok, string()} | {error, term()}.

part(Channels) ->
  ChanList = string:join(Channels, ","),
  encode("PART", [ChanList]).

%% @doc Leave specified channels.

-spec part([channel()], string()) ->
  {ok, string()} | {error, term()}.

part(Channels, Message) ->
  ChanList = string:join(Channels, ","),
  encode("PART", [ChanList, Message]).

%% @doc Request a topic for the channel.

-spec topic(channel()) ->
  {ok, string()} | {error, term()}.

topic(Channel) ->
  encode("TOPIC", [Channel]).

%% @doc Set topic for specified channel.

-spec topic(channel(), string()) ->
  {ok, string()} | {error, term()}.

topic(Channel, Topic) ->
  encode("TOPIC", [Channel, Topic]).

%% @doc List nicknames on specified channels.

-spec names([channel()]) ->
  {ok, string()} | {error, term()}.

names(Channels) ->
  ChanList = string:join(Channels, ","),
  encode("NAMES", [ChanList]).

%% @doc List nicknames on specified channels.

-spec names([channel()], server()) ->
  {ok, string()} | {error, term()}.

names(Channels, Server) ->
  ChanList = string:join(Channels, ","),
  encode("NAMES", [ChanList, Server]).

%% @doc List channels and their topics.

-spec list() ->
  {ok, string()} | {error, term()}.

list() ->
  encode("LIST", []).

%% @doc List channels and their topics.

-spec list([channel()]) ->
  {ok, string()} | {error, term()}.

list(Channels) ->
  ChanList = string:join(Channels, ","),
  encode("LIST", [ChanList]).

%% @doc List channels and their topics.

-spec list([channel()], server()) ->
  {ok, string()} | {error, term()}.

list(Channels, Server) ->
  ChanList = string:join(Channels, ","),
  encode("LIST", [ChanList, Server]).

%% @doc Invite user to a channel.

-spec invite(nick(), channel()) ->
  {ok, string()} | {error, term()}.

invite(Nick, Channel) ->
  encode("INVITE", [Nick, Channel]).

%% @doc Kick users from channels.

-spec kick([channel()], [nick()]) ->
  {ok, string()} | {error, term()}.

kick(Channels, Nicks) ->
  ChanList = string:join(Channels, ","),
  NickList = string:join(Nicks, ","),
  encode("KICK", [ChanList, NickList]).

%% @doc Kick users from channels.

-spec kick([channel()], [nick()], string()) ->
  {ok, string()} | {error, term()}.

kick(Channels, Nicks, Comment) ->
  ChanList = string:join(Channels, ","),
  NickList = string:join(Nicks, ","),
  encode("KICK", [ChanList, NickList, Comment]).

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

-spec privmsg(nick() | channel(), string()) ->
  {ok, string()} | {error, term()}.

privmsg([T | _] = Channel, Message)
when T == $#; T == $+; T == $!; T == $& ->
  encode("PRIVMSG", [Channel, Message]);
privmsg(Nick, Message) ->
  encode("PRIVMSG", [Nick, Message]).

%% @doc Send a message to user or channel.
%%   Messages sent with <i>NOTICE</i> are never getting any automated
%%   response (e.g. delivery errors).
%%
%% @see privmsg/3

-spec notice(nick() | channel(), string()) ->
  {ok, string()} | {error, term()}.

notice([T | _] = Channel, Message)
when T == $#; T == $+; T == $!; T == $& ->
  encode("NOTICE", [Channel, Message]);
notice(Nick, Message) ->
  encode("NOTICE", [Nick, Message]).

%% }}}
%%----------------------------------------------------------
%% 3.4 sending queries and commands {{{

%% @doc Request message-of-the-day from server.

-spec motd() ->
  {ok, string()} | {error, term()}.

motd() ->
  encode("MOTD", []).

%% @doc Request message-of-the-day from specific server.

-spec motd(server()) ->
  {ok, string()} | {error, term()}.

motd(Server) ->
  encode("MOTD", [Server]).

%% @doc Request statistics about IRC network users.

-spec lusers() ->
  {ok, string()} | {error, term()}.

lusers() ->
  encode("LUSERS", []).

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(string()) ->
  {ok, string()} | {error, term()}.

lusers(Mask) ->
  encode("LUSERS", [Mask]).

%% @doc Request statistics about IRC network users.
%%   `Mask' specifies what part of the network statistics will be about.

-spec lusers(string(), server()) ->
  {ok, string()} | {error, term()}.

lusers(Mask, Server) ->
  encode("LUSERS", [Mask, Server]).

%% @doc Request version information from the server.

-spec version() ->
  {ok, string()} | {error, term()}.

version() ->
  encode("VERSION", []).

%% @doc Request version information from the server.

-spec version(server()) ->
  {ok, string()} | {error, term()}.

version(Server) ->
  encode("VERSION", [Server]).

%% @doc Request server statistics.
%%
%% @see stats/3

-spec stats() ->
  {ok, string()} | {error, term()}.

stats() ->
  encode("STATS", []).

%% @doc Request server statistics.
%%
%% @see stats/3

-spec stats(string()) ->
  {ok, string()} | {error, term()}.

stats(Query) ->
  encode("STATS", [Query]).

%% @doc Request server statistics.
%%   Standard stats are:
%%   <ul>
%%     <li><i>l</i> -- list of server's connections</li>
%%     <li><i>m</i> -- usage count of each command supported by the
%%         server</li>
%%     <li><i>o</i> -- list of privileged users</li>
%%     <li><i>u</i> -- server's uptime</li>
%%   </ul>

-spec stats(string(), server()) ->
  {ok, string()} | {error, term()}.

stats(Query, Server) ->
  encode("STATS", [Query, Server]).

%% @doc List servers known to the queried server.

-spec links() ->
  {ok, string()} | {error, term()}.

links() ->
  encode("LINKS", []).

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.

-spec links(string()) ->
  {ok, string()} | {error, term()}.

links(Mask) ->
  encode("LINKS", [Mask]).

%% @doc List servers known to the queried server.
%%   `Mask' limits the returned list to only matching entries.
%%   `Target' is a mask and the first matching server is expected to answer
%%   the query.

-spec links(string(), string()) ->
  {ok, string()} | {error, term()}.

links(Target, Mask) ->
  encode("LINKS", [Target, Mask]).

%% @doc Request current time from server.

-spec time() ->
  {ok, string()} | {error, term()}.

time() ->
  encode("TIME", []).

%% @doc Request current time from server.

-spec time(server()) ->
  {ok, string()} | {error, term()}.

time(Server) ->
  encode("TIME", [Server]).

%% @doc Try to establish connection to target server.

-spec connect(server(), integer()) ->
  {ok, string()} | {error, term()}.

connect(Target, Port) ->
  encode("CONNECT", [Target, integer_to_list(Port)]).

%% @doc Try to establish connection to target server.

-spec connect(server(), integer(), server()) ->
  {ok, string()} | {error, term()}.

connect(Target, Port, Server) ->
  encode("CONNECT", [Target, integer_to_list(Port), Server]).

%% @doc List direct connections of local server.
%%   At least, this is what RFC 2812 recommends for <i>TRACE</i> command
%%   without arguments.

-spec trace() ->
  {ok, string()} | {error, term()}.

trace() ->
  encode("TRACE", []).

%% @doc Trace connection to specified server.

-spec trace(server()) ->
  {ok, string()} | {error, term()}.

trace(Target) ->
  encode("TRACE", [Target]).

%% @doc Request information about administrator of current server.

-spec admin() ->
  {ok, string()} | {error, term()}.

admin() ->
  encode("ADMIN", []).

%% @doc Request information about administrator of specified server.

-spec admin(nick() | server()) ->
  {ok, string()} | {error, term()}.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
admin(NickOrServer) ->
  encode("ADMIN", [NickOrServer]).

%% @doc Request information about the software server runs on.

-spec info() ->
  {ok, string()} | {error, term()}.

info() ->
  encode("INFO", []).

%% @doc Request information about the software server runs on.

-spec info(nick() | server()) ->
  {ok, string()} | {error, term()}.

%% nick contains no "."; nick causes the server hosting user called `Nick' to
%% reply
info(NickOrServer) ->
  encode("INFO", [NickOrServer]).

%% }}}
%%----------------------------------------------------------
%% 3.5 service query and commands {{{

%% @doc List servers known to the queried server.

-spec servlist() ->
  {ok, string()} | {error, term()}.

servlist() ->
  encode("SERVLIST", []).

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.

-spec servlist(string()) ->
  {ok, string()} | {error, term()}.

servlist(ServerMask) ->
  encode("SERVLIST", [ServerMask]).

%% @doc List services visible to current user.
%%   `ServerMask' limits the services to the ones running on matching servers.
%%   `Type' limits the services to the matching ones.
%%
%% @see service/5

-spec servlist(string(), string()) ->
  {ok, string()} | {error, term()}.

servlist(ServerMask, Type) ->
  encode("SERVLIST", [ServerMask, Type]).

%% @doc Send a query to service.

-spec squery(nick(), string()) ->
  {ok, string()} | {error, term()}.

squery(Service, Query) ->
  encode("SERVLIST", [Service, Query]).

%% }}}
%%----------------------------------------------------------
%% 3.6 user based queries {{{

%% @doc List all visible users.

-spec who() ->
  {ok, string()} | {error, term()}.

who() ->
  encode("WHO", []).

%% @doc List all users matching the mask.

-spec who(string()) ->
  {ok, string()} | {error, term()}.

who(Mask) ->
  encode("WHO", [Mask]).

%% @doc List all users matching the mask who are also operators.

-spec who(string(), o) ->
  {ok, string()} | {error, term()}.

who(Mask, o = _Limit) ->
  encode("WHO", [Mask, "o"]).

%% @doc Request information about specified user(s).

-spec whois([string()]) ->
  {ok, string()} | {error, term()}.

whois(UserMasks) ->
  encode("WHOIS", [UserMasks]).

%% @doc Request information about specified user(s).

-spec whois(server(), [string()]) ->
  {ok, string()} | {error, term()}.

whois(Server, UserMasks) ->
  UserMaskList = string:join(UserMasks, ","),
  encode("WHOIS", [Server, UserMaskList]).

%% @doc Request information about nicknames that no longer exist.

-spec whowas([nick()]) ->
  {ok, string()} | {error, term()}.

whowas(Nicks) ->
  NickList = string:join(Nicks, ","),
  encode("WHOWAS", [NickList]).

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas([nick()], integer()) ->
  {ok, string()} | {error, term()}.

whowas(Nicks, Count) ->
  NickList = string:join(Nicks, ","),
  encode("WHOWAS", [NickList, integer_to_list(Count)]).

%% @doc Request information about nicknames that no longer exist.
%%   At most `Count' entries are returned. If `Count' is 0 or negative, all
%%   entries are returned.

-spec whowas([nick()], integer(), server()) ->
  {ok, string()} | {error, term()}.

whowas(Nicks, Count, Server) ->
  NickList = string:join(Nicks, ","),
  encode("WHOWAS", [NickList, integer_to_list(Count), Server]).

%% }}}
%%----------------------------------------------------------
%% 3.7 miscellaneous messages {{{

%% @doc Terminate connection between specified user and his server.

-spec kill(nick(), string()) ->
  {ok, string()} | {error, term()}.

kill(Nick, Comment) ->
  encode("KILL", [Nick, Comment]).

%% @doc Send <i>ping</i> message to server.

-spec ping(server()) ->
  {ok, string()} | {error, term()}.

ping(Target) ->
  encode("PING", [Target]).

%% @doc Order a server to send <i>ping</i> message to server.

-spec ping(server(), server()) ->
  {ok, string()} | {error, term()}.

ping(Server, Target) ->
  encode("PING", [Server, Target]).

%% @doc Send <i>pong</i> reply to server.

-spec pong(server()) ->
  {ok, string()} | {error, term()}.

pong(Responder) ->
  encode("PONG", [Responder]).

%% @doc Send <i>pong</i> reply to server.

-spec pong(server(), server()) ->
  {ok, string()} | {error, term()}.

pong(Responder, Target) ->
  encode("PONG", [Responder, Target]).

%% @doc Report error.
%%   This message is not intended to be sent from typical client.

-spec error(string()) ->
  {ok, string()} | {error, term()}.

error(Message) ->
  encode("ERROR", [Message]).

%% }}}
%%----------------------------------------------------------
%% 4 optional features {{{

%% @doc Mark the user as being back.

-spec away() ->
  {ok, string()} | {error, term()}.

away() ->
  encode("AWAY", []).

%% @doc Mark the user as being away from keyboard.

-spec away(string()) ->
  {ok, string()} | {error, term()}.

away(Message) ->
  encode("AWAY", [Message]).

%% @doc Force the server to reload configuration file.

-spec rehash() ->
  {ok, string()} | {error, term()}.

rehash() ->
  encode("REHASH", []).

%% @doc Shutdown the server.

-spec die() ->
  {ok, string()} | {error, term()}.

die() ->
  encode("DIE", []).

%% @doc Restart the server

-spec restart() ->
  {ok, string()} | {error, term()}.

restart() ->
  encode("RESTART", []).

%% @doc Call user logged in on local server to join IRC.

-spec summon(nick()) ->
  {ok, string()} | {error, term()}.

summon(User) ->
  encode("SUMMON", [User]).

%% @doc Call user logged in on specified server to join IRC.

-spec summon(nick(), server()) ->
  {ok, string()} | {error, term()}.

summon(User, Server) ->
  encode("SUMMON", [User, Server]).

%% @doc Call user logged in on specified server to join IRC channel.

-spec summon(nick(), server(), channel()) ->
  {ok, string()} | {error, term()}.

summon(User, Server, Channel) ->
  encode("SUMMON", [User, Server, Channel]).

%% @doc Request a list of logged in users.

-spec users() ->
  {ok, string()} | {error, term()}.

users() ->
  encode("USERS", []).

%% @doc Request a list of logged in users.

-spec users(server()) ->
  {ok, string()} | {error, term()}.

users(Server) ->
  encode("USERS", [Server]).

%% @doc Send a message to all users who have <i>+w</i> mode.

-spec wallops(string()) ->
  {ok, string()} | {error, term()}.

wallops(Message) ->
  encode("WALLOPS", [Message]).

%% @doc List users' hosts.

-spec userhost([nick()]) ->
  {ok, string()} | {error, term()}.

%% up to 5 users, list is sent space-separated
userhost(Nicks) when is_list(hd(Nicks)), length(Nicks) =< 5 ->
  encode("USERHOST", Nicks).

%% @doc Check if specified users are present on IRC.
%%   The query is processed by local server only and not passed further (but
%%   servers generally know global state, so the response should be accurate).

-spec ison([nick()]) ->
  {ok, string()} | {error, term()}.

%% list is sent space-separated
ison(Nicks) when is_list(hd(Nicks)) ->
  encode("ISON", Nicks).

%% }}}
%%----------------------------------------------------------
%% server replies (numeric) {{{

%% TODO: numeric replies

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
