-module(ehms_push).
-export([general_notification/3, general_notification/6,
         general_app_msg/2, general_app_msg/4
        ]).

-include_lib("eutil/include/eutil.hrl").

general_notification(DeviceToken, Title, Content) ->
    {ok, AppId} = application:get_env(ehms, app_id),
    {ok, PkgName} = application:get_env(ehms, pkg_name),
    TokenInfo = ehms:get_access_token_info(),
    AccessToken = maps:get(<<"access_token">>, TokenInfo),
    general_notification(AppId, PkgName, AccessToken, DeviceToken, Title, Content).

general_notification(AppId, PkgName, AccessToken, DeviceToken, Title, Content) ->
    Payload = #{hps =>
                #{msg =>
                  #{type => 3,
                    body => #{title => Title, content => Content},
                    action => #{type => 3, param => #{appPkgName => PkgName}}
                   }
                 }
               },
    send(AppId, AccessToken, [DeviceToken], Payload).

general_app_msg(DeviceToken, Content) ->
    {ok, AppId} = application:get_env(ehms, app_id),
    TokenInfo = ehms:get_access_token_info(),
    AccessToken = maps:get(<<"access_token">>, TokenInfo),
    general_app_msg(AppId, AccessToken, DeviceToken, Content).

general_app_msg(AppId, AccessToken, DeviceToken, Content) ->
    Payload = #{hps =>
                #{msg =>
                  #{type => 1,
                    body => Content
                   }
                 }
               },
    send(AppId, AccessToken, [DeviceToken], Payload).



send(AppId, AccessToken, DeviceTokens, Payload) ->
    URLTail = hackney_url:urlencode(eutil:json_encode(#{ver => <<"1">>, appId => integer_to_binary(AppId)})),
    URL = <<"https://api.push.hicloud.com/pushsend.do?nsp_ctx=", URLTail/binary>>,
    Data = #{access_token => AccessToken, nsp_ts => erlang:system_time(second),
             nsp_svc => <<"openpush.message.api.send">>, device_token_list => eutil:json_encode(DeviceTokens),
             payload => eutil:json_encode(Payload)},
    Result = eutil:http_post(URL, ?URLENCEDED_HEADS, Data, [{pool, hms}]),
    case Result of
        #{<<"code">> := <<"80000000">>} ->
            {ok, Result};
        _ ->
            ?ERROR_MSG("ehms_push error, DeviceTokens: ~p, Result: ~p", [DeviceTokens, Result]),
            {error, Result}
    end.
