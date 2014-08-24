%%-----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%-----------------------------------------------------------------------------

-module(master_banker_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {bidders_count, bidders}).

-include("campaign.hrl").
-include("currency.hrl").
-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  bidder_announce/1,             % - used by the bidders to announce themselves 
                                 %   and allocate budget
  bidder_retire/1                % - used by the bidders before they die to move
                                 %   return their remaining budget back 
                                 %   (the budgets are read from mnesia)
  ]).

%%-----------------------------------------------------------------------------
%% gen_server Function Exports
%%-----------------------------------------------------------------------------

-export([                      % The behaviour callbacks
  init/1,                      % - initializes our process
  handle_call/3,               % - handles synchronous calls (with response)
  handle_cast/2,               % - handles asynchronous calls  (no response)
  % handle_cast/3,
  handle_info/2,               % - handles out of band messages (sent with !)
  terminate/2,                 % - is called on shut-down
  code_change/3                % - called to handle code changes
  ]).

%%-----------------------------------------------------------------------------
%% API Function Definitions
%%-----------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

bidder_announce(ID) ->
  gen_server:call(?SERVER, {bidder_announce, ID}).

bidder_retire(ID) ->
  gen_server:call(?SERVER, {bidder_retire, ID}).

%%-----------------------------------------------------------------------------
%% gen_server Function Definitions
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: init/0
%% Purpose: Initialize the master_banker node. Reads the campaign data
%%    from MySQL and loads them in CampaignBudget records in mnesia.
%%    It also initializes the server's state with no bidder connected.
%% Args:   -.
%% Returns: A fresh #state record with bidders_count = 0 and an empty
%%    bidders list, bidders = []
%%-----------------------------------------------------------------------------
init([]) ->
  load_currencies_in_mnesia("root", "", "attalon_production"),
  CampaignRecords = mysql_manager:load_campaign_data("root", "", "attalon_production"),
  Campaigns = update_campaigns(CampaignRecords),
  CampaignBudgets = get_banker_campaign_budget(Campaigns),
  mnesia_manager:save_campaign_budgets(CampaignBudgets),
  crone:start([
  {
    {daily, {0, 1, am}},
    {master_banker_worker, calculate_daily_budget, []}
  }]),
  {ok, #state{bidders_count=0, bidders=[]}}.


%%-----------------------------------------------------------------------------
%% Function: handle_call/2 - bidder_announce
%% Purpose: Handles the registration of a new bidder. Bidders call this
%%    function to register themselves. It triggers the aggregation of the
%%    budget distributed among bidders and its resuffling to the existing
%%    bidders and the newly registered one.
%%    Additionaly it updates the server's state to include the bidder node
%%    ID and the bidder counter.
%% Args:
%%    {bidder_announce, ID}: A tuple triggering the aggregation and
%%      redistribution of budget among nodes and updating the
%%      server's state.
%%    _From: The caller ID, not used for now.
%%    #state: Contains the number of registered bidders and a list with
%%      their node IDs.
%% Returns: The updated #state record with an increased counter and the new
%%    nodes ID aggregated in the bidders list.
%%-----------------------------------------------------------------------------
handle_call({bidder_announce, ID}, _From, #state{ bidders_count=Count, bidders=Bidders }) ->
  NodeCampaignBudgets = mnesia_manager:get_node_campaign_budgets(),

  CampaignBudgets = aggregate_node_campaign_budgets(NodeCampaignBudgets),

  mnesia_manager:create_node_campaign_budget(ID),

  NewNodeCampaignBudgets = calculate_node_campaign_budgets(CampaignBudgets, [ID] ++ Bidders),

  mnesia_manager:save_node_campaign_budgets(NewNodeCampaignBudgets),

  {reply, ok, #state{ bidders_count=Count+1, bidders=[ID] ++ Bidders }};


%%-----------------------------------------------------------------------------
%% Function: handle_call/2 - bidder_retire
%% Purpose: Handles the removal of a bidder. Bidders call this
%%    function to unregister themselves. It triggers the aggregation of the
%%    budget distributed among bidders and its resuffling to the existing
%%    bidders excluding the one leaving the cluster.
%%    Additionaly it updates the server's state to remove the bidder node
%%    ID and decrease the bidder counter.
%% Args:
%%    {bidder_announce, ID}: A tuple triggering the aggregation and
%%      redistribution of budget among nodes and updating the
%%      server's state.
%%    _From: The caller ID, not used for now.
%%    #state: Contains the number of registered bidders and a list with
%%      their node IDs.
%% Returns: The updated #state record with an increased counter and the new
%%    nodes ID aggregated in the bidders list.
%%-----------------------------------------------------------------------------
handle_call({bidder_retire, ID}, _From, #state{ bidders_count=Count, bidders=Bidders }) ->
  NodeCampaignBudgets = mnesia_manager:get_node_campaign_budgets(),

  CampaignBudgets = aggregate_node_campaign_budgets(NodeCampaignBudgets),

  NewNodeCampaignBudgets = calculate_node_campaign_budgets(CampaignBudgets, Bidders -- [ID]),

  mnesia_manager:remove_node_campaign_budget(ID),

  mnesia_manager:save_node_campaign_budgets(NewNodeCampaignBudgets),

  {reply, ok, #state{ bidders_count=Count - 1, bidders = Bidders -- [ID] }}.



handle_cast( _From, State) ->
  {ok, _From, State}.

handle_info(Info, State) ->      
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->  
  error_logger:info_msg("terminating~n"),
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%%-----------------------------------------------------------------------------
%% Internal Function Definitions
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: calculate_daily_budget/0
%% Purpose: Aggregates the finishing day's remaining budget from all nodes,
%%    calculates the budget for the next days as:
%%      REMAINING_CAMPAIGN_BUDGET / REMAINING_CAMPAIGN_DAYS = DAILY_BUDGET
%%    it adds last day's remaining budget as:
%%      LAST_DAY_REMAINING_BUDGET + DAILY_BUDGET = NEXT_DAY_BUDGET
%%    distributes the budget among the servers by dividing it and then
%%    persisting a slice to each bidder's entry in node_campaign_budget
%%    table in mnesia:
%%      NEXT_DAY_BUDGET / #state.bidders_count = BUDGET_PER_NODE
%% Args:   -
%% Returns: A fresh #state record with bidders_count = 0 and an empty
%%    bidders list, bidders = []
%%-----------------------------------------------------------------------------
calculate_daily_budget() ->
  NodeCampaignBudgets = mnesia_manager:get_node_campaign_budgets(),

  CampaignBudgets = aggregate_node_campaign_budgets(NodeCampaignBudgets),

  Bidders = mnesia_manager:get_bidders(),

  NextDayCampaignBudgets = calculate_next_day_campaign_budgets(CampaignBudgets),

  NewNodeCampaignBudgets = calculate_node_campaign_budgets(
    NextDayCampaignBudgets,
    Bidders),

  mnesia_manager:save_node_campaign_budgets(NewNodeCampaignBudgets),
  ok.


%%-----------------------------------------------------------------------------
%% Function: aggregate_node_campaign_budgets/1
%% Purpose: Aggregates the node budget for every campaign.
%% Args: NodeCampaignBudgets: A list of #node_campaign_budgets for the
%%    running campaigns.
%% Returns: The aggregated campaign budgets.
%%-----------------------------------------------------------------------------
aggregate_node_campaign_budgets(NodeCampaignBudgets) ->
  ets:new(campaign_budgets, [ named_table,
    {keypos, #banker_campaign_budget.campaign_id}]
  ),
  [aggregate_node_budget(NodeCampaignBudget) ||
    NodeCampaignBudget <- NodeCampaignBudgets],
  NewCampaignBudgets = ets:tab2list(campaign_budgets),
  ets:delete(campaign_budgets),
  NewCampaignBudgets.


%%-----------------------------------------------------------------------------
%% Function: aggregate_node_budget/1
%% Purpose: Aggregates the node budget for every campaign. It creates
%%    an entry in the campaign_budgets ETS table if there isn't one
%%    for the campaign_id in the parameter or it will update an existing one.
%% Args: NodeCampaignBudget: A record of #node_campaign_budgets.
%% Returns: It updates the ets table, doesn't return a meaningful value.
%%-----------------------------------------------------------------------------
aggregate_node_budget(NodeCampaignBudget) ->
  CampaignID = NodeCampaignBudget#node_campaign_budget.campaign_id,
  NodeRemainingBudget = NodeCampaignBudget#node_campaign_budget.remaining_budget,
  case ets:lookup(campaign_budgets, CampaignID) of
    [] ->
      ets:insert(campaign_budgets, #banker_campaign_budget{
        campaign_id = CampaignID,
        remaining_budget = NodeRemainingBudget
      });
    [Campaign | _] ->
      CampaignBudget = Campaign#banker_campaign_budget.remaining_budget,
      ets:insert(campaign_budgets, #banker_campaign_budget{
        campaign_id = CampaignID,
        remaining_budget = CampaignBudget + NodeRemainingBudget
      })
  end.


%%-----------------------------------------------------------------------------
%% Function: calculate_node_campaign_budgets/1
%% Purpose: Aggregates the node budget for every campaign. It creates
%%    an entry in the campaign_budgets ETS table if there isn't one
%%    for the campaign_id in the parameter or it will update an existing one.
%% Args: NodeCampaignBudget: A record of #node_campaign_budgets.
%% Returns: It updates the ets table, doesn't return a meaningful value.
%%-----------------------------------------------------------------------------
calculate_node_campaign_budgets(CampaignBudgets, Bidders) ->
  case CampaignBudgets of
    [] ->
      NewCampaignBudgets = calculate_next_day_campaign_budgets(CampaignBudgets)
  end,
  NewCampaignBudgets.


%%-----------------------------------------------------------------------------
%% Function: calculate_next_day_campaign_budgets/1
%% Purpose: Calculates the daily budget for all campaigns for the new day.
%%    For a new run without any residual budgets:
%%      REMAINING_CAMPAIGN_BUDGET / REMAINING_CAMPAIGN_DAYS = DAILY_BUDGET
%%    For runs with remaining budget from last day:
%%      REMAINING_CAMPAIGN_BUDGET / REMAINING_CAMPAIGN_DAYS =
%%          DAILY_BUDGET + LAST_DAY_REMAINING_BUDGET
%% Args:
%%    CampaignBudgets: The remaining budgets for all running campaigns in
%%    the end of the day.
%% Returns: The budget for the new day for all campaigns.
%%-----------------------------------------------------------------------------
calculate_next_day_campaign_budgets(CampaignBudgets) ->
  case CampaignBudgets of 
    [] ->
      NewCampaignBudgets = [#banker_campaign_budget {
        campaign_id = CampaignBudget#banker_campaign_budget.campaign_id,
        remaining_days = CampaignBudget#banker_campaign_budget.remaining_days,
        remaining_budget = CampaignBudget#banker_campaign_budget.remaining_budget
          - allocate_daily_budget(CampaignBudget),
        daily_budget = allocate_daily_budget(CampaignBudget)
      }
      || CampaignBudget <- mnesia_manager:get_campaign_budgets()]
  end,
  NewCampaignBudgets.

%%-----------------------------------------------------------------------------
%% Function: allocate_daily_budget/1
%% Purpose: Calculates the daily budget for a given campaign.
%% Args:
%%    CampaignBudget: The #banker_campaign_budget record for a single campaign.
%% Returns: The daily budget for the given campaign.
%%-----------------------------------------------------------------------------
allocate_daily_budget(CampaignBudget) ->
  CampaignBudget#banker_campaign_budget.remaining_budget /
    CampaignBudget#banker_campaign_budget.remaining_days.


%%----------------------------------------------------------------------
%% Function: get_banker_campaign_budget/1
%% Purpose: Creates a list of banker_campaign_budget records retrieved
%%    from MySQL.
%% Args: Campaigns: The active campaign records retrieved from MySQL.
%% Returns: A list of #banker_campaign_budget records for all active
%%    campaigns.
%%----------------------------------------------------------------------
get_banker_campaign_budget(Campaigns) ->
  [#banker_campaign_budget{
      campaign_id = Campaign#campaign.id,
      remaining_days = get_campaign_remaining_days(Campaign),
      remaining_budget = calculate_remaining_budget(Campaign),
      daily_budget = calculate_daily_budget(Campaign)
    } || Campaign <- Campaigns].


%%-----------------------------------------------------------------------------
%% Function: calculate_remaining_budget/1
%% Purpose: Calculates the remaining budget for the given campaign after
%%    the daily budget has been deducted as:
%%      REMAINING_CAMPAIGN_BUDGET - DAILY_BUDGET = REMAINING_CAMPAIGN_BUDGET
%% Args: Campaign: A Campaign record.
%% Returns: The remaining campaign budget.
%%-----------------------------------------------------------------------------
calculate_remaining_budget(Campaign) ->
  Campaign#campaign.monetary_budget - calculate_daily_budget(Campaign).


%%-----------------------------------------------------------------------------
%% Function: calculate_daily_budget/1
%% Purpose: Calculates the daily budget:
%%      REMAINING_CAMPAIGN_BUDGET - DAILY_BUDGET = REMAINING_CAMPAIGN_BUDGET
%% Args: Campaign: A Campaign record.
%% Returns: The remaining campaign budget.
%%-----------------------------------------------------------------------------
calculate_daily_budget(Campaign) ->
  Duration = Campaign#campaign.duration,
  if Duration == 0 -> Campaign#campaign.monetary_budget; % Campaigns expiring today
    Duration < 0 -> 0; % Expired campaigns
    Duration > 0 -> Campaign#campaign.monetary_budget / Duration % Campaign to start campaign
  end.


%%-----------------------------------------------------------------------------
%% Function: get_campaign_remaining_days/1
%% Purpose: Calculates the remaining days until a campaign's end date.
%%      CAMPAIGN_END_DATE - TODAY
%% Args: Campaign: A Campaign record from where the end date is read.
%% Returns: The remaining days until the campaign's end.
%%-----------------------------------------------------------------------------
get_campaign_remaining_days(Campaign) ->
  { { Y, M, D }, {_,_,_}} = calendar:universal_time(),
  { _, EndDate } =  Campaign#campaign.end_date,
  calendar:date_to_gregorian_days(EndDate) - calendar:date_to_gregorian_days(Y, M, D).


%%-----------------------------------------------------------------------------
%% Function: update_campaigns/1
%% Purpose: Creates a list of #campaign records retrieved
%%    from MySQL.
%% Args: Campaigns: The active campaign records retrieved from MySQL.
%% Returns: A list of #banker_campaign_budget records for all active
%%    campaigns with updated data.
%%-----------------------------------------------------------------------------
update_campaigns(Campaigns) ->
  [#campaign{
      id = Campaign#campaign.id, 
      monetary_budget = Campaign#campaign.monetary_budget,
      action_budget = Campaign#campaign.action_budget,
      currency_id = Campaign#campaign.currency_id,
      currency = mnesia_manager:find_currency_symbol(Campaign),
      start_date = Campaign#campaign.start_date,
      end_date = Campaign#campaign.end_date,
      duration = calculate_campaign_duration(Campaign)
    } || Campaign <- Campaigns].


%%-----------------------------------------------------------------------------
%% Function: calculate_campaign_duration/1
%% Purpose: Calculates the total campaign duration in days.
%%      CAMPAIGN_END_DATE - CAMPAIGN_START_DATE
%% Args: Campaign: A Campaign record from where the end and start
%%      dates is read.
%% Returns: The remaining days until the campaign's end.
%%-----------------------------------------------------------------------------
calculate_campaign_duration(Campaign) ->
  { _, StartDate } =  Campaign#campaign.start_date,
  { _, EndDate } =  Campaign#campaign.end_date,
  calendar:date_to_gregorian_days(EndDate) - calendar:date_to_gregorian_days(StartDate).


%%-----------------------------------------------------------------------------
%% Function: load_currencies_in_mnesia/3
%% Purpose: Loads all currency data from MySQL into mnesia.
%% Args:
%%    User: The MySQL username.
%%    Password: The MySQL user's password.
%%    Database: The database from where to draw the currencies
%%      (the default is attalon_production).
%% Returns: A list of #currency records with all currencies loaded
%%    from MySQL.
%%-----------------------------------------------------------------------------
load_currencies_in_mnesia(User, Password, Database) ->
  Currencies = mysql_manager:load_currency_data(User, Password, Database),
  [mnesia_manager:save_currency(Currency) || Currency <- Currencies].
