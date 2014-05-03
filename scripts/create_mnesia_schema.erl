-module(create_mnesia_schema).

-include_lib("../include/campaigns.hrl").

-export([create_database/0]).

create_database() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(campaigns,[
    {disc_copies, [node()]},
    {attributes, record_info(fields, campaign)}
    ]
  ),
  mnesia:stop().



