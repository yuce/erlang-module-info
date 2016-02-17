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
                        "sys", "timer", "unicode", "win32reg",
                        "zip", "compile", "erl_prim_loader", "erlang",
                        "init", "zlib", "erl_driver", "erl_nif",
                        "application", "auth", "code", "disk_log",
                        "erl_boot_server", "erl_ddll", "error_handler", "error_logger",
                        "file", "gen_tcp", "gen_udp", "gen_sctp",
                        "global", "global_group", "heart", "inet",
                        "inet_res", "net_adm", "net_kernel", "os",
                        "pg2", "rpc", "seq_trace", "wrap_log_reader",
                        "alarm_handler", "overload", "rb", "release_handler",
                        "systools", "mnesia", "mnesia_frag_hash", "mnesia_registry",
                        "odbc", "cpu_sup", "disksup", "memsup",
                        "os_mon_mib", "os_sup", "nteventlog", "otp_mib",
                        "asn1ct", "asn1rt", "crypto", "diameter",
                        "diameter_app", "diameter_codec", "diameter_make", "diameter_transport",
                        "diameter_tcp", "diametr_sctp", "eldap", "ei",
                        "ei_connect", "registry", "erl_connect", "erl_error",
                        "erl_eterm", "erl_format", "erl_global", "erl_malloc",
                        "erl_marshal", "gs", "inets", "ftp",
                        "tftp", "httpc", "httpd", "httpd_conf",
                        "httpd_custom_api", "httpd_socket", "httpd_util", "mod_alias",
                        "mod_auth", "mod_esi", "mod_security", "http_uri",
                        "public_key", "ssh", "ssh_channel", "ssh_connection",
                        "ssh_client_key_api", "ssh_server_key_api", "ssh_sftp", "ssh_sftpd",
                        "ssl", "ssl_crl_cache", "ssl_crl_cache_api", "ssl_session_cache_api"]).

main([]) ->
    io:format("Usage: module_info base_dir~n");

main([BaseDir]) ->
    {ok, ReExports} = re:compile(?RE_EXPORTS, [multiline]),
    {ok, ReFun} = re:compile(?RE_FUNS, [multiline]),
    Modules = get_modules(BaseDir),
    F = fun(M, Acc) ->
        case extract_exports(M, ReExports, ReFun) of
            none ->
                Acc;
            Ls ->
                Set = sets:from_list(Ls),
                Funs = lists:sort(sets:to_list(Set)),
                [{module_name(list_to_binary(M)), Funs} | Acc]
        end
    end,
    ModuleFuns = maps:from_list(lists:foldl(F, [], Modules)),
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
