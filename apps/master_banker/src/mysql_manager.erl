-module(mysql_manager).

-include("campaign.hrl").
-include("currency.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([load_campaign_data/4, load_currency_data/4]).

%%-----------------------------------------------------------------------------
%% API Function Implementations
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Function: load_campaign_data/1
%% Purpose: Connects to MySQL and returns all running campaign data.
%% Args:
%%    User: The username to use in the credentials.
%%    Password: The password to use in the credentials.
%%    Database: The database to connect in the server.
%%      The default one is attalon_production.
%% Returns: A record set with all the running campaigns in the system.
%%-----------------------------------------------------------------------------
load_campaign_data(Host, User, Password, Database) ->
  connect_to_mysql(Host, User, Password, Database),
  get_campaigns().


%%-----------------------------------------------------------------------------
%% Function: load_currency_data/1
%% Purpose: Connects to MySQL and returns all currency data.
%% Args:
%%    User: The username to use in the credentials.
%%    Password: The password to use in the credentials.
%%    Database: The database to connect in the server.
%%      The default one is attalon_production.
%% Returns: A record set with all  the currencies entered in the system.
%%-----------------------------------------------------------------------------
load_currency_data(Host, User, Password, Database) ->
  connect_to_mysql(Host, User, Password, Database),
  get_currencies().


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%% Function: get_currencies/1
%% Purpose: Retrieves the data for all currencies in the system.
%% Args: -
%% Returns: A record set with all the currencies entered in the system.
%%-----------------------------------------------------------------------------
get_currencies() ->
  Result = emysql:execute(banker_pool,
    <<"SELECT id, symbol, rate_to_euro FROM currencies">>),
  emysql_util:as_record(Result, currency, record_info(fields, currency)).


%%-----------------------------------------------------------------------------
%% Function: get_campaigns/1
%% Purpose: Retrieves the data for all running campaigns in the system.
%% Args: -
%% Returns: A record set with all the running campaigns.
%%-----------------------------------------------------------------------------
get_campaigns() ->
  Result = emysql:execute(banker_pool,
    <<"SELECT id, monetary_budget, action_budget, currency_id, start_date, end_date from campaigns">>),
  emysql_util:as_record(Result, campaign, record_info(fields, campaign)).


%%-----------------------------------------------------------------------------
%% Function: connect_to_mysql/1
%% Purpose: Connects to MySQL server.
%% Args:
%%    User: The username to use in the credentials.
%%    Password: The password to use in the credentials.
%%    Database: The database to connect in the server.
%%      The default one is attalon_production.
%% Returns: A tuple with a success or failure code.
%%-----------------------------------------------------------------------------
connect_to_mysql(Host, User, Password, Database) ->
  application:start(emysql),
  emysql:add_pool(banker_pool, [
            {size,1},
            {host, Host}
            {user, User},
            {password, Password},
            {database, Database},
            {encoding,utf8}]).
