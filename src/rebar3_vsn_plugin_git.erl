-module(rebar3_vsn_plugin_git).

-export([
         collect_default_refcount/0
        ]).

collect_default_refcount() ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    RawRef = get_ref(),

    {Tag, TagVsn} = parse_tags(),
        RawCount =
        case Tag of
            undefined -> get_ref_count();
            _         -> get_patch_count(RawRef)
        end,
    {TagVsn, RawRef, RawCount}.

get_ref() ->
    re:replace(os:cmd("git log -n 1 --pretty=format:'%h\n' "),  "\\s", "", [global]).

parse_tags() ->
    first_valid_tag(os:cmd("git log --oneline --decorate  | fgrep \"tag: \" -1000")).

first_valid_tag(Line) ->
    case re:run(Line, "(\\(|\\s)tag:\\s(v?([^,\\)]+))", [{capture, [2, 3], list}]) of
        {match,[Tag, Vsn]} ->
            {Tag, Vsn};
        nomatch -> {undefined, "0.0.0"}
    end.

get_ref_count() ->
    re:replace(os:cmd("git rev-list HEAD | wc -l"), "\\s", "", [global]).

get_patch_count(RawRef) ->
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list ~s..HEAD | wc -l",
                        [Ref]),
    os:cmd(Cmd).
