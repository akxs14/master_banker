-module(mnesia_manager).


-include_lib("stdlib/include/qlc.hrl").

-include("campaign.hrl").
-include("currency.hrl").
-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  create_mnesia_schema/1,
  save_currency/1,
  find_currency_symbol/1
  ]).

%%-----------------------------------------------------------------------------
%% API Function Definitions
%%-----------------------------------------------------------------------------

create_mnesia_schema(Nodes) ->
  create_campaign_budgets(Nodes),
  create_node_budgets(Nodes),
  create_currencies(Nodes).

save_currency(Currency) ->
  mnesia:activity(transaction, fun() ->
    mnesia:write(Currency)
  end).

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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

create_campaign_budgets(Nodes) ->
  mnesia:create_table(banker_campaign_budgets, 
    [{attributes, record_info(fields, banker_campaign_budget)},
    {index, [#banker_campaign_budget.campaign_id]},
    {ram_copies, Nodes}]).

create_node_budgets(Nodes) ->
  mnesia:create_table(node_campaign_budget, 
    [{attributes, record_info(fields, node_campaign_budget)},
    {index, [#node_campaign_budget.node_id]},
    {ram_copies, Nodes}]).

create_currencies(Nodes) ->
  mnesia:create_table(currency,
    [{attributes, record_info(fields, currency)},
    {index, [#currency.id]},
    {ram_copies, Nodes}]).
