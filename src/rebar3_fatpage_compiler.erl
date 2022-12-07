-module(rebar3_fatpage_compiler).

-export([compile/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(proplists:proplist()) -> ok.
compile(CommandLineArgs) ->
    add_paths([fatpage, abnfc]),
    Opts = fatpage:command_line_args(CommandLineArgs),
    rebar_api:debug("abnf files found:~n~p~n", [Opts]),
    compile1(Opts).

%% ===================================================================

add_paths(Deps) ->
    Base = dir(dir(dir(code:which(?MODULE)))),
    [code:add_patha(filename:join([Base, Dep, ebin])) || Dep <- Deps].

dir(F) ->
    filename:dirname(F).

compile1(Opts) ->
    #{out := Target, in := ABNF} = Opts,
    TargetTime = filelib:last_modified(Target),
    AbnfTime = filelib:last_modified(ABNF),
    rebar_api:debug("(~s, ~s):~n(~p, ~p)~n", [Target, ABNF, TargetTime, AbnfTime]),
    [cmp(ABNF, Opts) || TargetTime < AbnfTime].

-spec cmp(string(), map()) -> ok.
cmp(Source, Opts) ->
    rebar_api:debug("compiling ~s: ~p", [Source, Opts]),
    try fatpage:beam(Opts)
    catch C:R -> rebar_utils:abort("failed to compile ~s: ~s~n", [Source, {C, R}])
    end.
