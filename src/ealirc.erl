%%%---------------------------------------------------------------------------
%%% @doc
%%%   Types and other common stuff.
%%% @end
%%%---------------------------------------------------------------------------

-module(ealirc).

-export_type([prefix/0, command/0, argument/0]).
-export_type([nick/0, channel/0, server/0]).
-export_type([user_mode/0, channel_mode/0]).

%%%---------------------------------------------------------------------------

%% can't contain space
-type prefix() :: server_prefix() | user_prefix() | none.

-type server_prefix() :: {server, string()}.
-type user_prefix() :: {user,
                         Nick :: nick(),
                         User :: string() | undefined,
                         Host :: string() | undefined}.

%% [a-zA-Z]+ or [0-9][0-9][0-9]
-type command() :: string() | integer().

%% anything that doesn't contain NUL, CR, LF, " " or ":"
%% last one can contain " " and/or ":"
-type argument() :: string().

%% [:special:] :: "[", "]", "\", "`", "_", "^", "{", "|", "}"
%% [a-zA-Z[:special:]][a-zA-Z0-9[:special:]-]
-type nick() :: string().

%% [#+!&].+(:.+)?
%% "." means any except NUL, BELL, CR, LF, " ", "," and ":"
-type channel() :: string().

%% [a-z0-9]+[a-z0-9-]*(\.[a-z0-9]+[a-z0-9-]*)* | IPv4
-type server() :: string().

%% [+-][iwoOr]
%% a -- away (cannot be set with +a)
%% i -- invisible
%% w -- receives wallops
%% r -- restricted
%% o -- operator (chanop? IRC op?)
%% O -- local operator
%% s -- receives server notices (cannot be set with +s)
-type user_mode() :: string().

%% "[+-][aimnqpsrt]" | {"[+-][OovklbeI]", Argument}
-type channel_mode() :: string() | {Flag :: string(), Argument :: string()}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
