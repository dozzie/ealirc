%%%---------------------------------------------------------------------------
%%% @doc
%%%   Example IRC client.
%%%
%%%   Usage (to be simplified):
%%%   ```
%%%   {ok, Pid} = example_client:start("chat.example.net", 6667, "mynick"),
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
-export([start/3]).

%%% gen_ealirc callbacks
-export([init/1, terminate/2]).
-export([connected/2, disconnected/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, handle_message/4]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-record(state, {nick}).

%%%---------------------------------------------------------------------------
%%% public API

start(Server, Port, Nick) ->
  gen_ealirc:start(?MODULE, [Nick], {Server, Port}, []).

%%%---------------------------------------------------------------------------
%%% gen_ealirc callbacks

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_ealirc} state.

init([Nick] = _Args) ->
  {ok, #state{nick = Nick}}.

%% @private
%% @doc Clean up {@link gen_ealirc} state.

terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Initialize server connection (e.g. set nickname).

connected(_Socket, State = #state{nick = Nick}) ->
  io:fwrite("<~s> connected to IRC server~n", [Nick]),
  gen_ealirc:nick(self(), Nick),
  gen_ealirc:user(self(), Nick, none, Nick),
  {ok, State}.

%% @private
%% @doc Connection lost handler.

disconnected({connect,_} = _Reason, State = #state{nick = Nick}) ->
  io:fwrite("<~s> couldn't connect to server, retrying in 10s~n", [Nick]),
  {reconnect, 10000, State};

disconnected(Reason, State = #state{nick = Nick}) ->
  io:fwrite("<~s> lost server connection: ~1024p, retrying in 10s~n",
            [Nick, Reason]),
  {reconnect, 10000, State}.

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

handle_message(Prefix,
               "PING" = _Command,
               Args,
               State  = #state{nick = Nick}) ->
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
               "NICK"             = _Command,
               [NewNick]          = _Args,
               State              = #state{nick = Nick}) ->
  io:fwrite("<~s> Changing nickname from ~s to ~s~n", [Nick, Nick, NewNick]),
  {noreply, State#state{nick = NewNick}};

handle_message({user, Nick, _, _} = _Prefix,
               "PRIVMSG"          = _Command,
               [MsgTarget, "!" ++ Request] = _Args,
               State) ->
  [ReqCmd | _] = string:tokens(Request, " "),
  Reply = "sorry, command " ++ ReqCmd ++ " is not implemented yet",
  case MsgTarget of
    "#" ++ _ -> % other channel indicators: "+", "!", "&"
      gen_ealirc:privmsg(self(), MsgTarget, Nick ++ ": " ++ Reply);
    _ ->
      gen_ealirc:privmsg(self(), Nick, Reply)
  end,
  {noreply, State};

handle_message({user, Nick, _, _} = _Prefix,
               "MODE"             = _Command,
               [Channel, "+o", SelfNick] = _Args,
               State              = #state{nick = SelfNick}) ->
  gen_ealirc:privmsg(self(), Channel, [Nick ++ ": thank you"]),
  {noreply, State};

handle_message(Prefix, Command, Args, State = #state{nick = Nick}) ->
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
