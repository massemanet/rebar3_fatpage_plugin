-module(rebar3_fatpage_prv_compile).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).
-define(SHORT_DESC, "Compile ABNF (.abnf) files using the fatpage compiler").
-define(DESC,"").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, abnf},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {opts, 
             [{out, $o, "out_dir", string, "dir"},
              {module, $m, "mod", atom, "mod"},
              {verbose, $v, "dbg", boolean, "dbg"}]},
            {example, "rebar3 abnf compile"},
            {short_desc, ?SHORT_DESC},
            {desc, ?DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {Args, []} = rebar_state:command_parsed_args(State),
    compile(Args),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================

compile(Args) ->
    rebar3_fatpage_compiler:compile(Args).
