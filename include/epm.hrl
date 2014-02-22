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

-record(repoid, { name :: string()
                }).
-type repoid() :: #repoid{}.

%% Unified global identifier for package
-record(pkgid, { author     = ?any_author     :: string()    | ?any_author
               , pkg_name   = ?any_name       :: string()    | ?any_name
               , platform   = ?any_platform   :: platform()
               , vsn        = ?any_vsn        :: string()    | ?any_vsn
               , erlang_vsn = ?any_erlang_vsn :: erlangvsn() | ?any_erlang_vsn
  %% arguments passed from command line, like: source, {tag|branch|hash, x}
               , args=[] :: [pkg_arg()]
               }).
-type pkgid() :: #pkgid{}.

%% Repository: Specifies only title and base URL for api_module to use.
-record(repo, { id :: repoid()
              , description :: string()
              , url :: string()
              %, followers
              %, pushed
              , api_module=epm_vcs :: api_module()
              }).
-type repo() :: #repo{}.

%% Package: Does not specify branches/tags as this info is queried from package
%% repository online.
-record(pkg, { id :: pkgid()
             , install_dir :: string()
             , deps=[] :: [pkgid()]
             , args=[] :: list()
             , repo :: repoid()
             }).
-type pkg() :: #pkg{}.

-record(installed_pkg, { id :: pkgid()
                       , repository :: repoid()
                       , platform :: platform()
                       }).
-type installed_pkg() :: #installed_pkg{}.

%% State of the application, contains loaded local index, installed apps and
%% remote index
-record(epm_state, { installed=[] :: [installed_pkg()]
                   , global_index=[] :: [pkg()]
                   %, local_index=[] :: [pkg()]
                   %, repos=[] :: [repo()]
                   }).
