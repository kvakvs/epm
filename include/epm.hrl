-define(epm_version, "1.0.0-dev").
-define(epm_year, 2014).
-define(DEFAULT_API_MODULES, [github_api]).

-define(any_name,       any_n).
-define(any_author,     any_a).
-define(any_platform,   any_p).
-define(any_vsn,        any_v).
-define(any_erlang_vsn, any_e).

-define(EPM_FAIL(Format, Args), begin exit({error, epm:s(Format, Args)}) end).
-define(EPM_EXIT(Format, Args), begin exit({ok, epm:s(Format, Args)}) end).

-type platform() :: x86 | x64 | ?any_platform.
-type erlangvsn() :: string().
-type api_module() :: epm_vcs % autodetect
                    | epm_vcs_git | epm_vcs_github.
-type pkg_arg() :: source | {tag|branch|hash, string()}.

-record(repoid, { name=erlang:error(name_not_set) :: nonempty_string()
                }).
-type repoid() :: #repoid{}.

-define(IS_PKGID(X), is_tuple(X) andalso element(1, X) =:= pkgid).
-define(IS_PKG(X), is_tuple(X) andalso element(1, X) =:= pkg).

%% Repository: Specifies only title and base URL for api_module to use.
-record(repo, { id=erlang:error(id_not_set) :: repoid()
              , description :: string()
              , url :: string()
              , short_name=erlang:error(short_name_not_set) :: nonempty_string()
              , api_module=epm_vcs :: api_module()
              }).
-type repo() :: #repo{}.

-record(installed_pkg, { id=erlang:error(id_not_set) :: pkgid:pkgid()
                       , repository=erlang:error(repo_not_set) :: repoid()
                       , platform :: platform()
                       }).
-type installed_pkg() :: #installed_pkg{}.

%% State of the application, contains loaded local index, installed apps and
%% remote index
-record(epm_state, {%installed=[] :: [installed_pkg()]
                   %, global_index=[] :: [pkg()]
                   %, local_index=[] :: [pkg()]
                   %, repos=[] :: [repo()]
                   }).
