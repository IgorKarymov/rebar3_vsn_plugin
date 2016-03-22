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
    {ok, State1}.

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

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
