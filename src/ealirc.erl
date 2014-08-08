%%%---------------------------------------------------------------------------
%%% @doc
%%%   Types and other common stuff.
%%% @end
%%%---------------------------------------------------------------------------

-module(ealirc).

-export_type([prefix/0, command/0, argument/0]).
-export_type([nick/0, channel/0, server/0]).

%%%---------------------------------------------------------------------------

%% can't contain space
-type prefix() :: string() | none.

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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
