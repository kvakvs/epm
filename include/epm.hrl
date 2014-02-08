-define(epm_version, "1.0.0-dev").
-define(epm_year, 2014).
-define(DEFAULT_API_MODULES, [github_api]).

-define(any_author, any_author).
-define(any_vsn, any_vsn).
-define(any_platform, any_platform).

-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format(Format, Args)))).
-type platform() :: x86 | x64 | ?any_platform.
-type erlangvsn() :: binary().

-record(repoid, { name :: string()
                }).
-type repoid() :: #repoid{}.

%% Unified global identifier for package
-record(pkgid, { author=?any_author :: string() | ?any_author
              , pkg_name :: string()
              , platform=?any_platform :: platform()
              , vsn=?any_vsn :: string() | ?any_vsn
              , erlang_vsn=?any_vsn :: erlangvsn() | ?any_vsn
              }).
-type pkgid() :: #pkgid{}.

-record(repo, { id :: repoid()
              , description :: string()
              , url :: string()
              %, followers
              %, pushed
              , api_module
              }).
-type repo() :: #repo{}.

-record(pkg, { id :: pkgid()
             %, vsn=undefined :: string()
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
-record(epm_state, { local_available=[] :: [pkg()]
                   , installed=[] :: [installed_pkg()]
                   , remote_available=[] :: [pkg()]
                   , repositories=[] :: [repo()]
                   }).
