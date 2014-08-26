%%-----------------------------------------------------------------------------
%% Description: It contains the campaign information for a running campaign.
%% Fields: 
%%    campaign_id: The campaign's ID.
%%    remaining_days: The number of remaining days until the campaign's end date.
%%    remaining_budget: The remaining budget to spend.
%%    daily_budget: The budget to spend for the current day.
%%-----------------------------------------------------------------------------
-record(banker_campaign_budget,{
  campaign_id,
  remaining_days,
  remaining_budget,
  daily_budget
  }).
