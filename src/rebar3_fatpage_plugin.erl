-module(rebar3_fatpage_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    rebar3_fatpage_prv_compile:init(State0).
