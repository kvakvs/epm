-ifndef(EPM_PRIVATE_RECORDS).
-define(EPM_PRIVATE_RECORDS, true).

%% Package: Does not specify branches/tags as this info is queried from package
%% repository online.
-record(pkg, { id=erlang:error(id_not_set) :: pkgid:pkgid()
             , deps=[] :: [pkgid:pkgid()]
             , args=[] :: list()
             , repo=erlang:error(repo_not_set) :: repoid()
             }).

%% Unified global identifier for package
-record(pkgid, { author     = ?any_author     :: string()    | ?any_author
               , pkg_name   = ?any_name       :: string()    | ?any_name
               , platform   = ?any_platform   :: platform()
               , vsn        = ?any_vsn        :: string()    | ?any_vsn
               , erlang_vsn = ?any_erlang_vsn :: erlangvsn() | ?any_erlang_vsn
  %% arguments passed from command line, like: source, {tag|branch|hash, x}
               , args=[] :: [pkg_arg()]
               }).

-endif.