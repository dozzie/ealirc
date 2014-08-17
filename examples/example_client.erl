%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example IRC client.
%%%
%%%   Usage (to be simplified):
%%%   ```
%%%   {ok, Pid} = example_client:connect("chat.example.net", 6667, "mynick"),
%%%   gen_ealirc:join(Pid, ["#atled"]),
%%%   % sending raw commands
%%%   {ok, Cmd} = ealirc_proto:privmsg("#atled", "a message"),
%%%   gen_ealirc:quote(Pid, Cmd).
%%%   '''
%%% @end
%%%---------------------------------------------------------------------------

-module(example_client).

-behaviour(gen_ealirc).

%%% public API
-export([connect/3]).

%%% gen_ealirc callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_message/4]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {nick}).

%%%---------------------------------------------------------------------------
%%% public API

connect(Server, Port, Nick) ->
  gen_ealirc:connect(Server, Port, ?MODULE, [Nick], []).

%%%---------------------------------------------------------------------------
%%% gen_ealirc callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_ealirc} state.

init([Nick] = _Args) ->
  gen_ealirc:nick(self(), Nick),
  gen_ealirc:user(self(), Nick, none, Nick),
  {ok, #state{nick = Nick}}.

%% @private
%% @doc Clean up {@link gen_ealirc} state.

terminate(_Reason, _State) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(_Request, _From, State) ->
  {reply, {error, unknown}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

handle_info(_Msg, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming IRC messages.

handle_message(Prefix, "PING" = _Command, Args, State = #state{nick = Nick}) ->
  {ok, PongCmd} = ealirc_proto:pong(Nick),
  gen_ealirc:quote(self(), PongCmd),
  case {Prefix,Args} of
    {none,[From | _]} ->
      io:fwrite("<~s> PING from ~s~n", [Nick, From]);
    {_,[From | _]} ->
      io:fwrite("<~s> PING from ~s (~p)~n", [Nick, From, Prefix])
  end,
  {noreply, State};

handle_message({user, Nick, _, _} = _Prefix,
               "NICK" = _Command, [NewNick] = _Args,
               State = #state{nick = Nick}) ->
  io:fwrite("<~s> Changing nickname from ~s to ~s~n", [Nick, Nick, NewNick]),
  {noreply, State#state{nick = NewNick}};

handle_message(Prefix, Command, Args, State) ->
  io:fwrite("<~s> [~p] ~p ~1024p~n", [Nick, Prefix, Command, Args]),
  {noreply, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
