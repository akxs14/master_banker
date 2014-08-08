-module(mnesia_manager).


-include_lib("stdlib/include/qlc.hrl").

-include("node_campaign_budget.hrl").
-include("banker_campaign_budget.hrl").
-include("currency.hrl").

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([create_mnesia_schema/0]).

%%-----------------------------------------------------------------------------
%% API Function Definitions
%%-----------------------------------------------------------------------------

create_mnesia_schema() ->
  CreateCampaignBudgets = fun() ->
    create_campaign_budgets()
  end,
  create_table(CreateCampaignBudgets),
  CreateNodeBudgets = fun() ->
    create_node_budgets()
  end,
  create_table(CreateNodeBudgets),
  CreateCurrencies = fun() ->
    create_node_budgets()
  end,
  create_table(CreateCurrencies).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


create_table(Create) ->
  case catch Create() of
    {'EXIT',_} ->
      Create();
    _ -> 
      ok
  end.

create_campaign_budgets() ->
  io:format("create_campaign_budgets()~n~n"),
  mnesia:create_table(banker_campaign_budgets, 
    [{attributes, record_info(fields, banker_campaign_budget)},
    {index, [#banker_campaign_budget.campaign_id]},
    {ram_copies, [node()]}]
  ).

create_node_budgets() ->
  io:format("create_node_budgets()~n~n"),
  mnesia:create_table(node_campaign_budget, 
    [{attributes, record_info(fields, node_campaign_budget)},
    {index, [#node_campaign_budget.node_id]},
    {ram_copies, [node()]}]
  ).

create_currencies() ->
  io:format("create_currencies()~n~n"),
  mnesia:create_table(currencies,
    [{attributes, record_info(fields, currency)},
    {index, [#currency.id]},
    {ram_copies, [node()]}]
  ).
