%%%----------------------------------------------------------------------------
%%% @doc The master banker server, that allocates campaign budget.
%%% @author Angelos Kapsimanis <angelos at reto.io>
%%% @end
%%%----------------------------------------------------------------------------
-module(master_banker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(HOST, "localhost").
-define(USERNAME, "makis").
-define(PASSWORD, "akxs14").
-define(DATABASE, "makis").
-define(PORT, 5432).

%% ------------------------------------------------------------------
%% Required header files and external record definitions
%% ------------------------------------------------------------------

-include_lib("deps/epgsql/include/pgsql.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("../include/campaign.hrl").
-include_lib("../include/campaign_daily_budget.hrl").

%% ------------------------------------------------------------------
%% Record definitions
%% ------------------------------------------------------------------

-record(state, {count, dbconn}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  start_link/0,
  stop/0,
  say_hello/0,
  get_count/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

say_hello() ->
  gen_server:cast(?SERVER, say_hello).

get_count() ->
  gen_server:call(?SERVER, get_count).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  create_schema(),
  load_campaign_budgets(),
  calculate_campaign_budgets(),
  {ok, #state{count=0}}.

handle_call(get_count, _From, #state{count=Count}) ->
  {reply, Count, #state{count=Count+1}}.
% handle_call(_Request, _From, State) ->
%   {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(say_hello, State) ->
  io:format("Hello~n"),
  {noreply, #state{count=State#state.count+1}}.
% handle_cast(_Msg, State) ->
%   {noreply, State}.

handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.
% handle_info(_Info, State) ->
%   {noreply, State}.

terminate(_Reason, _State) ->
  error_logger:info_msg("terminating~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% creates an in-memory schema in local mnesia
create_schema() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(campaign,[
    {attributes, record_info(fields, campaign)}
  ]),
  mnesia:create_table(campaign_daily_budget,[
    {attributes, record_info(fields, campaign_daily_budget)}
  ]).


load_campaign_budgets() ->
  {ok, C} = pgsql:connect(?HOST, ?USERNAME, ?PASSWORD,[{database,?DATABASE}, {port,?PORT}]),
  {ok, _, Rows} = pgsql:equery(
    C,
    "select id, start_date, end_date, monetary_budget from campaigns where status = $1",
    ["active"]),
  [save_campaign_data(Row) || Row <- Rows],
  {ok, C}.


save_campaign_data(Row) ->
  {Id, Start_date, End_date, Budget} = Row,
  mnesia:transaction(
    fun() ->
      mnesia:write(#campaign{
                    id = Id,
                    start_date = Start_date,
                    end_date = End_date,
                    budget = binary_to_float(Budget)
      })
    end).


calculate_campaign_budgets() ->
  mnesia:transaction(
    fun() ->
      qlc:eval( qlc:q(
        [ calculate_daily_budget(X) || X <- mnesia:table(campaign)]
      ))
    end).


calculate_daily_budget(Campaign) ->
  Duration = date_util:day_difference(
                            Campaign#campaign.end_date,
                            Campaign#campaign.start_date),
  DailyBudget = Campaign#campaign.budget / Duration,
  save_distributed_budget(
                          Campaign#campaign.id,
                          Campaign#campaign.start_date,
                          Campaign#campaign.end_date,
                          DailyBudget).


save_distributed_budget(Id, Start_date, End_date, DailyBudget) ->
  save_day_budget(Id, Start_date, date_util:subtract(End_date, {days,1}), DailyBudget).


save_day_budget(Id, End_date, End_date, Budget) ->
  insert_daily_budget(Id, End_date, Budget);

save_day_budget(Id, Date, End_date, Budget) ->
  insert_daily_budget(Id, Date, Budget),
  save_day_budget(Id, date_util:add(Date,{days,1}), End_date, Budget).


insert_daily_budget(Id, Date, Budget) ->
  mnesia:transaction(
    fun() ->
      mnesia:write(#campaign_daily_budget{
        id = Id,
        date = Date,
        budget = binary_to_float(Budget)
      })
    end).
