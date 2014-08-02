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

bidder_announce(bidder_id) ->
  gen_server:call(?SERVER, {bidder_announce, bidder_id}).

bidder_retire(bidder_retire) ->
  gen_server:cast(?SERVER, {bidder_retire, bidder_id}).

hello() ->
  gen_server:cast(?SERVER, say_hello).

%% ---------------------------------------------------------------------------
%% gen_server Function Definitions
%% ---------------------------------------------------------------------------

