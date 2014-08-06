%%%----------------------------------------------------------------------------
%%% @doc Master banker worker
%%% @author angelos
%%% @end
%%%----------------------------------------------------------------------------

-module(master_banker_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {bidders_count}).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------

-export([
  start_link/0,                  % - starts and links the process in one step
  stop/0,                        % - stops it
  bidder_announce/1,             % - used by the bidders to announce themselves 
                                 %   and allocate budget
  bidder_retire/1,               % - used by the bidders before they die to move
                                 %   return their remaining budget back 
                                 %   (the budgets are read from mnesia)
  hello/0
  ]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------

-export([                      % The behaviour callbacks
  init/1,                      % - initializes our process
  handle_call/3,               % - handles synchronous calls (with response)
  handle_cast/2,               % - handles asynchronous calls  (no response)
  handle_cast/3,
  handle_info/2,               % - handles out of band messages (sent with !)
  terminate/2,                 % - is called on shut-down
  code_change/3                % - called to handle code changes
  ]).

%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

bidder_announce(ID) ->
  gen_server:call(?SERVER, {bidder_announce, ID}).

bidder_retire(ID) ->
  gen_server:cast(?SERVER, {bidder_retire, ID}).

hello() ->
  gen_server:cast(?SERVER, say_hello).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

init([]) ->
  mnesia_manager:create_mnesia_schema(),
  % read campaign id and budgets from mysql
  % calculate campaign duration
  % calculate daily budget per campaign
  % calculate budget per bidder
  % write budget per bidder in mnesia (and set fresh_budget=true)
  {ok, #state{bidders_count=0}}.

handle_call({bidder_announce, _ID}, _From, #state{bidders_count=Count}) ->
  % read the remaining daily budget from all nodes and for all campaigns
  % calculate the remaining daily budget for N+1 bidders
  % write budget per bidder in mnesia (and set fresh_budget=true)
  % add new bidder to ets table with bidders
  {reply, ok, #state{bidders_count=Count+1}}.

handle_cast({bidder_retire, _ID}, _From, #state{bidders_count=Count}) ->
  % read the remaining daily budget from all nodes and for all campaigns
  % calculate the remaining daily budget for N-1 bidders
  % write budget per bidder in mnesia (and set fresh_budget=true)
  % remove bidder to ets table with bidders
  {reply, ok, #state{bidders_count=Count-1}}.

handle_cast(say_hello, State) ->
  io:format("Hallo!~n"),
  {noreply, State}.

handle_info(Info, State) ->      
    error_logger:info_msg("~p~n", [Info]), 
    {noreply, State}.          

terminate(_Reason, _State) ->  
    error_logger:info_msg("terminating~n"),
    ok.                        

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.               

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------





