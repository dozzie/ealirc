%%%---------------------------------------------------------------------------
%%% @doc
%%%   IRC client behaviour.
%%%
%%%   <b>NOTE</b>: Any IRC commands (e.g. <i>JOIN</i>) sent to `self()' within
%%%   callbacks are processed <i>after</i> the callback has returned. This
%%%   could be a limitation.
%%%
%%%   == Expected callbacks ==
%%%
%%%   The callbacks are the same as for {@link gen_server} behaviour, with
%%%   small addition.
%%%
%%%   `Module:handle_message(Prefix, Command, Args, State)'
%%% @end
%%%---------------------------------------------------------------------------

-module(gen_ealirc).

-behaviour(gen_server).

%%% public API
-export([start/4, start/5, start_link/4, start_link/5]). % start
-export([connect/5, connect_link/5]). % connect+start
-export([call/2, call/3, cast/2]).
-export([quote/2]). % send a message to IRC server
-export([nick/2, user/4]).
-export([mode/3, topic/3]).
-export([ping/2, pong/2]).
-export([invite/3, kick/3, kick/4]).
-export([join/2, part/2, part/3, quit/1, quit/2]).
-export([privmsg/3, notice/3]).

%%% behaviour definition
-export([behaviour_info/1]).

%%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

%% {@link gen_server} recognized names
-type process_name() :: {local, atom()} | {global, term()} | {via, term()}.

-record(state, {sock, mod, state}).

%%%---------------------------------------------------------------------------
%%% public API

%% @TODO message() (send an IRC message, prefixed or not)
%% @TODO option for not closing the socket on stop
%% @TODO connect/6, connect_link/6 (registering names)
%% @TODO channel handlers

%%----------------------------------------------------------
%% start() {{{

%% @doc Start process using already prepared socket.
%%
%%   `Options' is a proplist suitable for {@link gen_server:start/3}.
%%
%%   <b>NOTE</b>: The function doesn't close TCP socket on error. It's the
%%   caller's responsibility.

-spec start(module(), term(), gen_tcp:socket(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

start(Module, Args, Socket, Options) ->
  State = pre_start(Socket, Module),
  InitArgs = {Module, Args, State},
  case gen_server:start(?MODULE, InitArgs, Options) of
    {ok, Pid} ->
      post_start(State, Pid),
      {ok, Pid};
    Any ->
      Any
  end.

%% @doc Start registered process using already prepared socket.
%%
%%   `ServerName' is the same as for {@link gen_server:start/4}.
%%
%%   `Options' is a proplist suitable for {@link gen_server:start/4}.
%%
%%   <b>NOTE</b>: The function doesn't close TCP socket on error. It's the
%%   caller's responsibility.

-spec start(process_name(), module(), term(), gen_tcp:socket(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

start(ServerName, Module, Args, Socket, Options) ->
  State = pre_start(Socket, Module),
  InitArgs = {Module, Args, State},
  case gen_server:start(ServerName, ?MODULE, InitArgs, Options) of
    {ok, Pid} ->
      post_start(State, Pid),
      {ok, Pid};
    Any ->
      Any
  end.

%% @doc Start (linked) process using already prepared socket.
%%
%%   `Options' is a proplist suitable for {@link gen_server:start_link/4}.
%%
%%   <b>NOTE</b>: The function doesn't close TCP socket on error. It's the
%%   caller's responsibility.

-spec start_link(module(), term(), gen_tcp:socket(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

start_link(Module, Args, Socket, Options) ->
  State = pre_start(Socket, Module),
  InitArgs = {Module, Args, State},
  case gen_server:start_link(?MODULE, InitArgs, Options) of
    {ok, Pid} ->
      post_start(State, Pid),
      {ok, Pid};
    Any ->
      Any
  end.

%% @doc Start registered (and linked) process using already prepared socket.
%%
%%   `ServerName' is the same as for {@link gen_server:start_link/4}.
%%
%%   `Options' is a proplist suitable for {@link gen_server:start_link/4}.
%%
%%   <b>NOTE</b>: The function doesn't close TCP socket on error. It's the
%%   caller's responsibility.

-spec start_link(process_name(), module(), term(), gen_tcp:socket(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

start_link(ServerName, Module, Args, Socket, Options) ->
  State = pre_start(Socket, Module),
  InitArgs = {Module, Args, State},
  case gen_server:start_link(ServerName, ?MODULE, InitArgs, Options) of
    {ok, Pid} ->
      post_start(State, Pid),
      {ok, Pid};
    Any ->
      Any
  end.

%% pre and post start {{{

pre_start(Socket, Module) ->
  ok = inet:setopts(Socket, [
    list,
    {packet, line}, {packet_size, 512},
    {active, false} % for now
  ]),
  #state{sock = Socket, mod = Module}.

post_start(_State = #state{sock = Socket}, Child) ->
  gen_tcp:controlling_process(Socket, Child),
  ok = inet:setopts(Socket, [{active, true}]),
  ok.

%% }}}

%% }}}
%%----------------------------------------------------------
%% connect() {{{

%% @doc Connect to specified IRC server.

-spec connect(inet:hostname() | inet:ip_address(), integer(),
              module(), term(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

connect(Server, Port, Module, Args, Options) ->
  case gen_tcp:connect(Server, Port, [{active, false}]) of
    {ok, Sock} ->
      case start(Module, Args, Sock, Options) of
        {ok, Pid} ->
          {ok, Pid};
        {error, Reason} ->
          gen_tcp:close(Sock),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Connect (and link the connection handler) to specified IRC server.

-spec connect_link(inet:hostname() | inet:ip_address(), integer(),
                   module(), term(), list()) ->
  {ok, pid()} | ignore | {error, term()}.

connect_link(Server, Port, Module, Args, Options) ->
  case gen_tcp:connect(Server, Port, [{active, false}]) of
    {ok, Sock} ->
      case start_link(Module, Args, Sock, Options) of
        {ok, Pid} ->
          {ok, Pid};
        {error, Reason} ->
          gen_tcp:close(Sock),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------
%% gen_server-like commands {{{

%% @doc Call {@link gen_ealirc} synchronously.

-spec call(pid(), term()) ->
  term().

call(Pid, Request) ->
  gen_server:call(Pid, Request).

%% @doc Call {@link gen_ealirc} synchronously with timeout.

-spec call(pid(), term(), timeout()) ->
  term().

call(Pid, Request, Timeout) ->
  gen_server:call(Pid, Request, Timeout).

%% @doc Send an asynchronous request to {@link gen_ealirc}.

-spec cast(pid(), term()) ->
  ok.

cast(Pid, Request) ->
  gen_server:cast(Pid, Request).

%% }}}
%%----------------------------------------------------------
%% IRC commands {{{

%% @doc Send raw IRC line to server.
%%
%%   The line will be terminated with CR+LF automatically. You don't need to
%%   include it.

-spec quote(pid(), string()) ->
  ok.

quote(Pid, Line) ->
  cast(Pid, {'$gen_irc_quote', Line}).

%% @doc Nick change/set request.
%%
%% @see ealirc_proto:nick/1

-spec nick(pid(), ealirc_proto:nick()) ->
  ok | {error, term()}.

nick(Pid, Nick) ->
  case ealirc_proto:nick(Nick) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Passing username and realname.
%%
%% @see ealirc_proto:user/3

-spec user(pid(), ealirc_proto:nick(), none | invisible | wallops | invisible_wallops,
           string()) ->
  ok | {error, term()}.

user(Pid, User, Mode, RealName) ->
  case ealirc_proto:user(User, Mode, RealName) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Set user or channel mode.
%%
%%   <b>WARNING</b>: be careful when mixing modes with and without arguments
%%   (channel modes only). There's a risk of passing argument to wrong mode if
%%   you incidentally omitted argument to, for example, `"+o"' or `"+I"'.
%%
%% @see ealirc_proto:mode/2
%%
%% @spec mode(pid(), ealirc_proto:nick() | ealirc_proto:channel(),
%%            [ealirc_proto:user_mode()] | [ealirc_proto:channel_mode()]) ->
%%   ok

-spec mode(pid(), ealirc_proto:nick(),    [ealirc_proto:user_mode()])    -> ok;
          (pid(), ealirc_proto:channel(), [ealirc_proto:channel_mode()]) -> ok.

mode(Pid, NickOrChannel, Modes) ->
  case ealirc_proto:mode(NickOrChannel, Modes) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Quit message.
%%
%% @see ealirc_proto:quit/0

-spec quit(pid()) ->
  ok | {error, term()}.

quit(Pid) ->
  case ealirc_proto:quit() of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Quit message.
%%
%% @see ealirc_proto:quit/1

-spec quit(pid(), string()) ->
  ok | {error, term()}.

quit(Pid, Message) ->
  case ealirc_proto:quit(Message) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Join provided channels.
%%
%%   Channel may be provided as a name or a tuple of `{Chan,Key}', containing
%%   channel key.
%%
%%   Special case of integer 0 is for <i>JOIN 0</i> IRC command, which leaves
%%   all the channels user is in.
%%
%% @see ealirc_proto:join/1

-spec join(pid(), [ealirc_proto:channel()]
           | [{ealirc_proto:channel(), string()}]
           | 0) ->
  ok | {error, term()}.

join(Pid, Channels) ->
  case ealirc_proto:join(Channels) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Leave specified channels.
%%
%% @see ealirc_proto:part/1

-spec part(pid(), [ealirc_proto:channel()]) ->
  ok | {error, term()}.

part(Pid, Channels) ->
  case ealirc_proto:part(Channels) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Leave specified channels.
%%
%% @see ealirc_proto:part/2

-spec part(pid(), [ealirc_proto:channel()], string()) ->
  ok | {error, term()}.

part(Pid, Channels, Message) ->
  case ealirc_proto:part(Channels, Message) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Set topic for specified channel.
%%
%% @see ealirc_proto:topic/2

-spec topic(pid(), ealirc_proto:channel(), string()) ->
  ok | {error, term()}.

topic(Pid, Channel, Topic) ->
  case ealirc_proto:topic(Channel, Topic) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Invite user to a channel.
%%
%% @see ealirc_proto:invite/2

-spec invite(pid(), ealirc_proto:nick(), ealirc_proto:channel()) ->
  ok | {error, term()}.

invite(Pid, Nick, Channel) ->
  case ealirc_proto:invite(Nick, Channel) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Kick users from channels.
%%
%% @see ealirc_proto:kick/2

-spec kick(pid(), [ealirc_proto:channel()], [ealirc_proto:nick()]) ->
  ok | {error, term()}.

kick(Pid, Channels, Nicks) ->
  case ealirc_proto:kick(Channels, Nicks) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Kick users from channels.
%%
%% @see ealirc_proto:kick/3

-spec kick(pid(), [ealirc_proto:channel()], [ealirc_proto:nick()], string()) ->
  ok | {error, term()}.

kick(Pid, Channels, Nicks, Comment) ->
  case ealirc_proto:kick(Channels, Nicks, Comment) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Send a message to user or channel.
%%
%% @see ealirc_proto:privmsg/2

-spec privmsg(pid(), ealirc_proto:nick() | ealirc_proto:channel(), string()) ->
  ok | {error, term()}.

privmsg(Pid, Target, Message) ->
  case ealirc_proto:privmsg(Target, Message) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Send a message to user or channel.
%%
%%   Messages sent with <i>NOTICE</i> are never getting any automated
%%   response (e.g. delivery errors).
%%
%% @see privmsg/3

-spec notice(pid(), ealirc_proto:nick() | ealirc_proto:channel(), string()) ->
  ok | {error, term()}.

notice(Pid, Target, Message) ->
  case ealirc_proto:notice(Target, Message) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Send <i>ping</i> message to server.
%%
%% @see ealirc_proto:ping/1

-spec ping(pid(), ealirc_proto:server()) ->
  ok | {error, term()}.

ping(Pid, Target) ->
  case ealirc_proto:ping(Target) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Send <i>pong</i> reply to server.
%%
%% @see ealirc_proto:pong/1

-spec pong(pid(), ealirc_proto:server()) ->
  ok | {error, term()}.

pong(Pid, Responder) ->
  case ealirc_proto:pong(Responder) of
    {ok, Msg} -> quote(Pid, Msg);
    {error, Reason} -> {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% behaviour definition {{{

%% @doc Define callbacks.

behaviour_info(callbacks) ->
  GenServerCallbacks = [
    {init, 1}, {terminate, 2},
    {handle_call, 3}, {handle_cast, 2}, {handle_info, 2},
    {code_change, 3}
  ],
  CustomCallbacks = [
    {handle_message, 4}
  ],
  GenServerCallbacks ++ CustomCallbacks;

behaviour_info(_Any) ->
  undefined.

%%% }}}
%%%---------------------------------------------------------------------------
%%% gen_server callbacks {{{

%%----------------------------------------------------------
%% initialization and cleanup {{{

%% @private
%% @doc Initialize {@link gen_server} state.

init({Module, ModArgs, InitState} = _Args) ->
  case Module:init(ModArgs) of
    {ok, ModState} ->
      {ok, InitState#state{state = ModState}};
    {ok, ModState, hibernate} ->
      {ok, InitState#state{state = ModState}, hibernate};
    {ok, ModState, Timeout} ->
      {ok, InitState#state{state = ModState}, Timeout};
    Any ->
      Any
  end.

%% @private
%% @doc Clean up {@link gen_server} state.

terminate(Reason, _State = #state{sock = Sock, mod = Mod, state = MState}) ->
  Result = Mod:terminate(Reason, MState),
  gen_tcp:close(Sock),
  Result.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(Request, From, State = #state{mod = Mod, state = MState}) ->
  case Mod:handle_call(Request, From, MState) of
    {reply, Reply, NewMState} ->
      {reply, Reply, State#state{state = NewMState}};
    {reply, Reply, NewMState, hibernate} ->
      {reply, Reply, State#state{state = NewMState}, hibernate};
    {reply, Reply, NewMState, Timeout} ->
      {reply, Reply, State#state{state = NewMState}, Timeout};
    {noreply, NewMState} ->
      {noreply, State#state{state = NewMState}};
    {noreply, NewMState, hibernate} ->
      {noreply, State#state{state = NewMState}, hibernate};
    {noreply, NewMState, Timeout} ->
      {noreply, State#state{state = NewMState}, Timeout};
    {stop, Reason, Reply, NewMState} ->
      {stop, Reason, Reply, State#state{state = NewMState}};
    {stop, Reason, NewMState} ->
      {stop, Reason, State#state{state = NewMState}}
  end.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% generic command
handle_cast({'$gen_irc_quote', CommandLine} = _Request,
            State = #state{sock = Sock}) ->
  case gen_tcp:send(Sock, [CommandLine, "\r\n"]) of
    ok ->
      {noreply, State}; % FIXME: and what if there was a timeout set?
    {error, Reason} ->
      {stop, Reason, State}
  end;

handle_cast(Request, State = #state{mod = Mod, state = MState}) ->
  case Mod:handle_cast(Request, MState) of
    {noreply, NewMState} ->
      {noreply, State#state{state = NewMState}};
    {noreply, NewMState, hibernate} ->
      {noreply, State#state{state = NewMState}, hibernate};
    {noreply, NewMState, Timeout} ->
      {noreply, State#state{state = NewMState}, Timeout};
    {stop, Reason, NewMState} ->
      {stop, Reason, State#state{state = NewMState}}
  end.

%% @private
%% @doc Handle incoming messages.

%% line from socket
handle_info({tcp, Sock, Line} = _Msg,
            State = #state{sock = Sock, mod = Mod, state = MState}) ->
  case ealirc_proto:decode(Line) of
    {ok, {Prefix, Command, Args}} ->
      case Mod:handle_message(Prefix, Command, Args, MState) of
        {noreply, NewMState} ->
          {noreply, State#state{state = NewMState}};
        {noreply, NewMState, hibernate} ->
          {noreply, State#state{state = NewMState}, hibernate};
        {noreply, NewMState, Timeout} ->
          {noreply, State#state{state = NewMState}, Timeout};
        {stop, Reason, NewMState} ->
          {stop, Reason, State#state{state = NewMState}}
      end;
    {error, _Reason} ->
      % server should never pass an invalid line
      % TODO: log the event
      {stop, badserver, State}
  end;

%% EOF
handle_info({tcp_closed, Sock} = _Msg, State = #state{sock = Sock}) ->
  % TODO: log the event
  % FIXME: or pass this to `handle_message/4'?
  {stop, normal, State};

%% connection error
handle_info({tcp_error, Sock, Reason} = _Msg, State = #state{sock = Sock}) ->
  % TODO: log the event
  {stop, Reason, State};

%% unknown message
handle_info(Msg, State = #state{mod = Mod, state = MState}) ->
  case Mod:handle_info(Msg, MState) of
    {noreply, NewMState} ->
      {noreply, State#state{state = NewMState}};
    {noreply, NewMState, hibernate} ->
      {noreply, State#state{state = NewMState}, hibernate};
    {noreply, NewMState, Timeout} ->
      {noreply, State#state{state = NewMState}, Timeout};
    {stop, Reason, NewMState} ->
      {stop, Reason, State#state{state = NewMState}}
  end.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(OldVsn, State = #state{mod = Mod, state = MState}, Extra) ->
  case Mod:code_change(OldVsn, MState, Extra) of
    {ok, NewMState} ->
      {ok, State#state{state = NewMState}};
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------

%%% }}}
%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
