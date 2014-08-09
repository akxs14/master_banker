-record(campaign, {
  id,
  monetary_budget,
  action_budget,
  currency_id,
  currency,
  start_date,
  end_date,
  duration = 0,
  remaining_overall_budget = 0,
  todays_remaining_budget = 0
  }).
