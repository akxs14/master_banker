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
%% Record definitions
%% ------------------------------------------------------------------

-record(state, {count}).

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
  {ok, C} = load_campaign_budgets(),
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

load_campaign_budgets() ->
  {ok, C} = pgsql:connect(?HOST, ?USERNAME, ?PASSWORD,[{database,?DATABASE}, {port,?PORT}]),
  {ok, Columns, Rows} = pgsql:squery(
    C,
    "SELECT id, start_date, end_date, monetary_budget FROM CAMPAIGNS WHERE status=$1",
    ["active"]),
  io:format("Rows: ~p~n", Rows),
  {ok, C}.
