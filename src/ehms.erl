-module(ehms).
-export([get_access_token_info/0, get_access_token_info/2]).
-include_lib("eutil/include/eutil.hrl").

get_access_token_info() ->
    {ok, AppId} = application:get_env(ehms, app_id),
    {ok, AppSecret} = application:get_env(ehms, app_secret),
    get_access_token_info(AppId, AppSecret).

get_access_token_info(AppId, AppSecret) ->
    URL = "https://login.cloud.huawei.com/oauth2/v2/token",
    Data = #{grant_type => <<"client_credentials">>, client_id => AppId,
             client_secret => AppSecret},
    eutil:http_post(URL, ?URLENCEDED_HEADS, Data, [{pool, hms}]).
