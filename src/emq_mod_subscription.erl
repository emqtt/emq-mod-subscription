%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emq_mod_subscription).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("emqttd/include/emqttd_protocol.hrl").

%% Mnesia Callbacks
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

-export([load/1, on_client_connected/3, unload/0]).

%% Static Subscription API
-export([add/1, lookup/1, del/1, del/2]).

-define(TAB, ?MODULE).

%%--------------------------------------------------------------------
%% Mnesia callbacks
%%--------------------------------------------------------------------

mnesia(boot) ->
    ok = emqttd_mnesia:create_table(?TAB, [
                {type, bag},
                {disc_copies, [node()]},
                {record_name, mqtt_subscription},
                {attributes, record_info(fields, mqtt_subscription)},
                {storage_properties, [{ets, [compressed]},
                                      {dets, [{auto_save, 5000}]}]}]);

mnesia(copy) ->
    ok = emqttd_mnesia:copy_table(?TAB).

%%--------------------------------------------------------------------
%% Load/Unload Hook
%%--------------------------------------------------------------------

load(Topics) ->
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Topics]).

on_client_connected(?CONNACK_ACCEPT, Client = #mqtt_client{client_id  = ClientId,
                                                           client_pid = ClientPid,
                                                           username   = Username}, Topics) ->

    Replace = fun(Topic) -> rep(<<"%u">>, Username, rep(<<"%c">>, ClientId, Topic)) end,
    TopicTable = [{Replace(Topic), Qos} || {Topic, Qos} <- Topics],
    emqttd_client:subscribe(ClientPid, TopicTable),
    {ok, Client};

on_client_connected(_ConnAck, _Client, _State) ->
    ok.

unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3).

%%--------------------------------------------------------------------
%% Add/Del static subscriptions
%%--------------------------------------------------------------------

%% @doc Add a static subscription manually.
-spec(add(mqtt_subscription()) -> ok | {error, already_existed}).
add(Subscription = #mqtt_subscription{subid = SubId, topic = Topic}) ->
    Pattern = match_pattern(SubId, Topic),
    return(mnesia:transaction(fun() ->
                    case mnesia:match_object(?TAB, Pattern, write) of
                        [] ->
                            mnesia:write(?TAB, Subscription, write);
                        [Subscription] ->
                            mnesia:abort(already_existed);
                        [Subscription1] -> %% QoS is different
                            mnesia:delete_object(?TAB, Subscription1, write),
                            mnesia:write(?TAB, Subscription, write)
                    end
            end)).

%% @doc Lookup static subscription
-spec(lookup(binary()) -> list(mqtt_subscription())).
lookup(ClientId) when is_binary(ClientId) ->
    mnesia:dirty_read(?TAB, ClientId).

%% @doc Delete static subscriptions by ClientId manually.
-spec(del(binary()) -> ok).
del(ClientId) when is_binary(ClientId) ->
    return(mnesia:transaction(fun mnesia:delete/1, [{?TAB, ClientId}])).

%% @doc Delete a static subscription manually.
-spec(del(binary(), binary()) -> ok).
del(ClientId, Topic) when is_binary(ClientId) andalso is_binary(Topic) ->
    return(mnesia:transaction(fun del_/1, [match_pattern(ClientId, Topic)])).

del_(Pattern) ->
    lists:foreach(fun(Subscription) ->
                mnesia:delete_object(?TAB, Subscription, write)
        end, mnesia:match_object(?TAB, Pattern, write)).

match_pattern(SubId, Topic) ->
    #mqtt_subscription{subid = SubId, topic = Topic, qos = '_'}.

return({atomic, ok})      -> ok;
return({aborted, Reason}) -> {error, Reason}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

rep(<<"%c">>, ClientId, Topic) ->
    emqttd_topic:feed_var(<<"%c">>, ClientId, Topic);
rep(<<"%u">>, undefined, Topic) ->
    Topic;
rep(<<"%u">>, Username, Topic) ->
    emqttd_topic:feed_var(<<"%u">>, Username, Topic).

