-module(rebar3_vsn_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_vsn_plugin).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_vsn_plugin"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Options      = rebar_state:get(State, rebar3_vsn_plugin, []),
    VersionsOpts = proplists:get_value(versions, Options, []),
    Versions     = rebar3_vsn_plugin_utils:handle_versions(VersionsOpts),
    rebar_api:info("Versions ~p",[Versions]),
    OverrideVsnForApps = proplists:get_value(override_vsn_for_apps, Options, []),
    State1 = rewrite_versions_for_apps(State, Versions, OverrideVsnForApps),
    OverrideVsnForRelease = proplists:get_value(override_vsn_for_release, Options),
    State2 = rewrite_version_for_release(State1, Versions, OverrideVsnForRelease),
    {ok, State2}.

rewrite_versions_for_apps(State, Versions, OverrideVsnForApps) ->
    Apps = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    FlatOverrideVsnForApps =
        [{OvAppName, VsnRef} || {OvAppsNames, VsnRef} <- OverrideVsnForApps,
                                OvAppName <- OvAppsNames],
    lists:foldl(
      fun(AppInfo, St) ->
              AppName = binary_to_atom(rebar_app_info:name(AppInfo), utf8),
              case lists:keyfind(AppName, 1, FlatOverrideVsnForApps) of
                  {AppName, VsnRef} ->
                      NewAppVsn = proplists:get_value(VsnRef, Versions),
                      AppFile = rebar_app_info:app_file(AppInfo),
                      ok = set_appfile_version(AppFile, NewAppVsn),
                      St;
                  false -> St
              end
      end, State, Apps),
    State.

set_appfile_version(Filepath, NewVsn) when is_list(Filepath) ->
    rebar_api:info("Rewriting vsn in ~s", [filename:basename(Filepath)]),
    {ok, [{application, AppName, Sections}]} = file:consult(Filepath),
    Vsn = proplists:get_value(vsn, Sections),
    NewSections = [{vsn, NewVsn} | proplists:delete(vsn, Sections)],
    Contents = io_lib:format("~p.~n",[{application, AppName, NewSections}]),
    ok = file:write_file(Filepath, Contents),
    rebar_api:info("Vsn changed from ~s --> ~s in: ~s",[Vsn, NewVsn, Filepath]),
    ok.

rewrite_version_for_release(State, _Versions, undefined) -> State;
rewrite_version_for_release(State, Versions, VsnRef) ->
    NewRelVsn = proplists:get_value(VsnRef, Versions),
    rebar_api:info("Setting release vsn in rebar.config to ~s", [NewRelVsn]),
    set_rebar_relx_version("rebar.config", NewRelVsn),
    State.

set_rebar_relx_version(Filepath, NewVsn) ->
    {ok, Bin} = file:read_file(Filepath),
    Lines = lists:map(fun binary_to_list/1, binary:split(Bin, <<"\n">>, [global])),
    case set_rebar_relx_version_1(NewVsn, Lines, false, []) of
        {error, _} = E ->
            E;
        Contents ->
            ok = file:write_file(Filepath, [strip(Contents), <<"\n">>])
    end.

set_rebar_relx_version_1(NewVsn, [Line | Lines], Found, Acc) ->
    case string:str(Line, "release-version-marker") > 0 of
        true ->
            NewLine = io_lib:format("    \"~s\" %% release-version-marker", [NewVsn]),
            set_rebar_relx_version_1(NewVsn, Lines, true, [NewLine|Acc]);
        false ->
            set_rebar_relx_version_1(NewVsn, Lines, Found, [Line|Acc])
    end;
set_rebar_relx_version_1(_, [], false, _Acc) ->
    {error, relflow_marker_missing};
set_rebar_relx_version_1(_, [], true, Acc) ->
    [ [Line, <<"\n">>] || Line <- lists:reverse(Acc) ].

strip(<<>>) ->
    <<>>;
strip(B) when is_list(B) ->
    strip(iolist_to_binary(B));
strip(B) when is_binary(B) ->
    LastChar = binary:last(B),
    case whitespace(LastChar) of
        false ->
            B;
        true ->
            strip(binary:part(B, 0, size(B)-1))
    end.

whitespace($ )  -> true;
whitespace($\t) -> true;
whitespace($\r) -> true;
whitespace($\n) -> true;
whitespace(_)   -> false.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
