-module(mnesia_manager).


-include_lib("stdlib/include/qlc.hrl").

-include("campaign.hrl").
-include("currency.hrl").
-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").

%%% -----------------------------------------------------------------------------
%% API Function Exports
%%% -----------------------------------------------------------------------------

-export([
  init_db/0,
  save_currency/1,
  find_currency_symbol/1,
  save_campaign_budgets/1,
  get_campaign_budgets/0,
  get_node_campaign_budgets/0,
  get_bidders/0,
  create_node_campaign_budget/1,
  save_node_campaign_budgets/1,
  remove_node_campaign_budget/1
  ]).

%%% -----------------------------------------------------------------------------
%%% API Function Definitions
%%% -----------------------------------------------------------------------------

%%% -----------------------------------------------------------------------------
%%% API Calls for master_banker_app
%%% -----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: init_db/0
%% Purpose: Initialized mnesia and creates the tables to the local node.
%% Args:   -
%% Returns: -
%%-----------------------------------------------------------------------------
init_db() ->
  application:stop(mnesia),
  mnesia:create_schema(Nodes = [node()]),
  ok = mnesia:start(),
  create_campaign_budgets(Nodes),
  create_node_budgets(Nodes),
  create_currencies(Nodes).

%%-----------------------------------------------------------------------------
%% API Calls for master_banker_worker
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: create_node_campaign_budget/1
%% Purpose: Creates a series of #node_campaign_budget records in mnesia for
%%    a new node. It will create a record for every running campaign.
%% Args: 
%%    NodeID: The new bidder node's ID.
%% Returns: A list with the #node_campaign_budget records for the new node.
%%-----------------------------------------------------------------------------
create_node_campaign_budget(NodeID) ->
  CampaignsBudgets = get_campaign_budgets(),
  [create_node_record(NodeID, CampaignsBudget) || CampaignsBudget <- CampaignsBudgets].


%%-----------------------------------------------------------------------------
%% Function: remove_node_campaign_budget/1
%% Purpose: Deletes all #node_campaign_budget records from mnesia related with
%%    the given bidder node.
%% Args:
%%    NodeID: The new bidder node's ID.
%% Returns: Removed all records related with the given bidder node ID. 
%%-----------------------------------------------------------------------------
remove_node_campaign_budget(NodeID) ->
  mnesia:activity(transaction, fun() ->
    mnesia:delete(node_campaign_budget, NodeID)
  end).


%%-----------------------------------------------------------------------------
%% Function: save_node_campaign_budgets/1
%% Purpose: Saves all given #banker_campaign_budget records in mnesia.
%% Args:
%%    BankerCampaignBudgets: The list of records to be persisted.
%% Returns: A list of mnesia query results.
%%-----------------------------------------------------------------------------
save_node_campaign_budgets(BankerCampaignBudgets) ->
  [save_node_campaign_budget(BankerCampaignBudget) ||
    BankerCampaignBudget <- BankerCampaignBudgets].


%%-----------------------------------------------------------------------------
%% Function: get_bidders/1
%% Purpose: Returns a list of IDs of the registered bidders.
%% Args: -
%% Returns: A list of node IDs of the registered bidders
%%-----------------------------------------------------------------------------
get_bidders() ->
  [Node#node_campaign_budget.node_id || Node <- get_node_campaign_budgets()].


%%-----------------------------------------------------------------------------
%% Function: get_campaign_budgets/1
%% Purpose: Returns a list of #banker_campaign_budgets from mnesia.
%% Args: -
%% Returns: A list of #banker_campaign_budgets records from mnesia.
%%-----------------------------------------------------------------------------
get_campaign_budgets() ->
  ets:tab2list(banker_campaign_budget).


%%-----------------------------------------------------------------------------
%% Function: node_campaign_budget/1
%% Purpose: Returns a list of #node_campaign_budget from mnesia.
%% Args: -
%% Returns: A list of #node_campaign_budget records from mnesia.
%%-----------------------------------------------------------------------------
get_node_campaign_budgets() ->
  ets:tab2list(node_campaign_budget).


%%-----------------------------------------------------------------------------
%% Function: save_currency/1
%% Purpose: Saves a #currency record in mnesia
%% Args:
%%    Currency: The #currency record to be saved.
%% Returns: An mnesia query result.
%%-----------------------------------------------------------------------------
save_currency(Currency) ->
  mnesia:activity(transaction, fun() ->
    mnesia:write(Currency)
  end).


%%-----------------------------------------------------------------------------
%% Function: find_currency_symbol/1
%% Purpose: Returns the currency symbol for a given currency id.
%% Args:
%%    Campaign: Provides the currency id.
%% Returns: The currency symbol.
%%-----------------------------------------------------------------------------
find_currency_symbol(Campaign) ->
  Results = mnesia:activity(transaction, fun() ->
    mnesia:read({currency, Campaign#campaign.currency_id})
  end),
  case hd(Results) of
    {currency, _, _, Symbol, _} ->
      Symbol;
    _ ->
      <<"Unknown">>
  end.


%%-----------------------------------------------------------------------------
%% Function: save_campaign_budgets/1
%% Purpose: Returns the currency symbol for a given currency id.
%% Args:
%%    Campaign: Provides the currency id.
%% Returns: The currency symbol.
%%-----------------------------------------------------------------------------
save_campaign_budgets(CampaignBudgets) ->
  [ mnesia:activity(transaction, fun() ->
      mnesia:write(CampaignBudget)
    end) || CampaignBudget <- CampaignBudgets].


%%% -----------------------------------------------------------------------------
%%% Internal Function Definitions
%%% -----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function: save_node_campaign_budget/1
%% Purpose: Saves a given #banker_campaign_budget record in mnesia.
%% Args:
%%    BankerCampaignBudget: The record to be persisted.
%% Returns: A mnesia query result.
%%-----------------------------------------------------------------------------
save_node_campaign_budget(BankerCampaignBudget) ->
  mnesia:activity(transaction, fun() ->
    mnesia:write(BankerCampaignBudget)
  end).


%%-----------------------------------------------------------------------------
%% Function: create_node_record/2
%% Purpose: Creates a #node_campaign_budget record and stores it in mnesia.
%% Args:
%%    NodeID: The bidder node's Id.
%%    CampaignsBudget: The campaign's #banker_campaign_budget record to which
%%        the new record will be related.
%% Returns: A new #node_campaign_budget record in mnesia.
%%-----------------------------------------------------------------------------
create_node_record(NodeID, CampaignsBudget) ->
  NodeCampaignBudget = #node_campaign_budget{
    campaign_id = CampaignsBudget#banker_campaign_budget.campaign_id,
    node_id = NodeID
  },
  mnesia:activity(transaction, fun() ->
    mnesia:write(NodeCampaignBudget)
  end).


%%-----------------------------------------------------------------------------
%% Function: create_campaign_budgets/1
%% Purpose: Creates the banker_campaign_budget table in mnesia in the given nodes.
%% Args:
%%    Nodes: The list of nodes where the table will be created.
%% Returns: A mnesia query result.
%%-----------------------------------------------------------------------------
create_campaign_budgets(Nodes) ->
  mnesia:create_table(banker_campaign_budget,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, banker_campaign_budget)}]).


%%-----------------------------------------------------------------------------
%% Function: create_node_budgets/1
%% Purpose: Creates the node_campaign_budget table in mnesia in the given nodes.
%% Args:
%%    Nodes: The list of nodes where the table will be created.
%% Returns: A mnesia query result.
%%-----------------------------------------------------------------------------
create_node_budgets(Nodes) ->
  mnesia:create_table(node_campaign_budget,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, node_campaign_budget)}]).


%%-----------------------------------------------------------------------------
%% Function: create_currencies/1
%% Purpose: Creates the currency table in mnesia in the given nodes.
%% Args:
%%    Nodes: The list of nodes where the table will be created.
%% Returns: A mnesia query result.
%%-----------------------------------------------------------------------------
create_currencies(Nodes) ->
  mnesia:create_table(currency,
    [{ram_copies, Nodes},
    {attributes, record_info(fields, currency)}]).
