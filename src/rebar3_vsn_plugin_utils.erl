-module(rebar3_vsn_plugin_utils).

-export([
         handle_versions/1
        ]).

handle_versions(Versions) ->
    [{VsnRef, handle_version(VsnOpts)} || {VsnRef, VsnOpts} <- Versions].

handle_version(Version) ->
    binary_to_list(iolist_to_binary(lists:map(fun process_vsn_component/1, Version))).

process_vsn_component({env_var, VariableName}) ->
    case os:getenv(VariableName) of
        false ->
            "";
                Value -> Value
    end;
process_vsn_component(epoch) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    integer_to_list((MegaSecs * 1000000) + Secs);
process_vsn_component(git_ref) ->
    {_TagVsn, RawRef, _RawCount} =
        rebar3_vsn_plugin_git:collect_default_refcount(),
    RawRef;
process_vsn_component(git_tag) ->
    {TagVsn, _RawRef, _RawCount} =
        rebar3_vsn_plugin_git:collect_default_refcount(),
    TagVsn;
process_vsn_component(git_commits_number) ->
    {_TagVsn, _RawRef, RawCount} =
        rebar3_vsn_plugin_git:collect_default_refcount(),
    RawCount;
process_vsn_component(ConstString) -> ConstString.
