-module(mysql_manager).

-include("campaign.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([load_campaign_data/3]).

%%-----------------------------------------------------------------------------
%% API Function Implementations
%%-----------------------------------------------------------------------------

load_campaign_data(User, Password, Database) ->
  connect_to_mysql(User, Password, Database),
  get_campaign_rows().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_campaign_rows() ->
  Result = emysql:execute(hello_pool,
    <<"SELECT id, monetary_budget, action_budget, currency_id, start_date, end_date from campaigns">>),
  emysql_util:as_record(Result, campaign, record_info(fields, campaign)).

connect_to_mysql(User, Password, Database) ->
  application:start(emysql),
  emysql:add_pool(hello_pool, [
            {size,1},
            {user, User},
            {password, Password},
            {database, Database},
            {encoding,utf8}]).
