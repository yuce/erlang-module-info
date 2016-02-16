#! /usr/bin/env escript
%% -*- erlang
%%! -pa ./deps

-define(RE_ERL, "\\.erl$").
-define(RE_EXPORTS, "^-export\\(\\[([^\\]]+)\\]\\)\\.").
-define(RE_FUNS, "(\\w+)/").
-define(MOD_WHITELIST, ["lists", "array", "base64", "beam_lib",
                        "binary", "c", "calendar", "dets",
                        "dict", "digraph", "digraph_utils", "epp",
                        "erl_anno", "erl_eval", "erl_expand_records", "erl_id_trans",
                        "erl_internal", "erl_lint", "erl_parse", "erl_pp",
                        "erl_scan", "erl_tar", "ets", "file_sorter",
                        "filelib", "filename", "gb_sets", "gb_trees",
                        "gen_event", "gen_fsm", "gen_server", "io",
                        "io_lib", "lib", "lists", "log_mf_h",
                        "maps", "math", "ms_transform", "orddict",
                        "ordsets", "pool", "proc_lib", "proplists",
                        "qlc", "queue", "rand", "random",
                        "re", "sets", "shell", "slave",
                        "sofs", "string", "supervisor", "supervisor_bridge",
                        "sys", "timer", "unicode", "win32reg", "zip"]).

main([]) ->
    io:format("Usage: module_info base_dir~n");

main([BaseDir]) ->
    {ok, ReExports} = re:compile(?RE_EXPORTS, [multiline]),
    {ok, ReFun} = re:compile(?RE_FUNS, [multiline]),
    Modules = get_modules(BaseDir),
    F = fun(M) ->
        Set = sets:from_list(extract_exports(M, ReExports, ReFun)),
        Funs = lists:sort(sets:to_list(Set)),
        {module_name(list_to_binary(M)), Funs}
    end,
    ModuleFuns = maps:from_list(lists:map(F, Modules)),
    io:format("~s~n", [jsx:encode(ModuleFuns)]).

get_modules(BaseDir) ->
    WhiteList = sets:from_list(?MOD_WHITELIST),
    Modules = filelib:wildcard("**/src/*.erl", BaseDir),
    [filename:join(BaseDir, Fn) || Fn <- Modules,
                                sets:is_element(module_name(Fn), WhiteList)].

module_name(Filename) ->
    filename:basename(Filename, ".erl").

extract_exports(Filename, ReExports, ReFun) ->
    {ok, Text} = file:read_file(Filename),
    case re:run(Text, ReExports, [global, {capture, all_but_first, binary}]) of
        nomatch ->
            none;
        {match, Captured} ->
            F = fun([C]) ->
                extract_functions(C, ReFun)
            end,
            lists:flatten(lists:map(F, Captured))
    end.

extract_functions(ExportBin, ReFun) ->
    case re:run(ExportBin, ReFun, [global, {capture, all_but_first, binary}]) of
        nomatch ->
            [];
        {match, Captured} ->
            Captured
    end.
