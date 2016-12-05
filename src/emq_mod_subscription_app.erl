%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
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

-module(emq_mod_subscription_app).

-include_lib("emqttd/include/emqttd_protocol.hrl").

-behaviour(application).

-export([start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

-define(APP, emq_mod_subscription).

start(_Type, _Args) ->
    ?APP:load(application:get_env(?APP, topics, [])),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ?APP:unload(),
    ok.

%% Dummy supervisor
init([]) ->
	{ok, {{one_for_one, 1, 5}, []}}.

