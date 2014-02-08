-define(epm_version, "1.0.0-dev").
-define(epm_year, 2014).
-define(DEFAULT_API_MODULES, [github_api]).
-define(epm_index_filename, "epm_index").

-define(EXIT(Format, Args), exit(lists:flatten(io_lib:format(Format, Args)))).

-record(repository, { name
                    , owner
                    , description
                    , homepage
                    , followers
                    , pushed
                    , api_module
                    }).
-record(package, {user
                 , name
                 , vsn=undefined
                 , app_vsn
                 , install_dir
                 , deps=[]
                 , args=[]
                 , repo=#repository{}
                 }).