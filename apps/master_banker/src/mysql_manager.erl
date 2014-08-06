-module(mysql_manager).

-include("campaign.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([load_campaign_data/0]).

%%-----------------------------------------------------------------------------
%% API Function Implementations
%%-----------------------------------------------------------------------------

load_campaign_data() ->
  emysql:add_pool(hello_pool, [{size,2},
             {user,"root"},
             {password,""},
             {database,"attalon_production"},
             {encoding,utf8}]).
  % Result = emysql:execute(hello_pool, 
  %   <<"SELECT id, monetary_budget, action_budget, currency_id from campaigns">>),
  % Recs = emysql_util:as_record(
  %   Result, campaign, record_info(fields, hello_record)).

