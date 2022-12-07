Rebar3 fatpage plugin
=====

A rebar3 plugin for automatically compiling .abnf files using the fatpage compiler.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{erl_opts, [{i, "./_build/default/plugins/fatpage/include"}]}.

{plugins, [
    {rebar3_fatpage_plugin, "0.0.0"}
]}.
```
