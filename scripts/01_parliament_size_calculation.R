
size_calculation <- function(df_name) {
  
  data_frame <- get(df_name)
  
  # minus 0.5
  data_frame <- data_frame %>%
    mutate(mandate_minus_05 = mandate_minimum - 0.5)
  
  # Get divisor by party: second_votes divided by minimum - 0.5
  data_frame <- data_frame %>%
    mutate(party_divisor_1 = second_votes / mandate_minus_05)
  
  # get minimum divisor for parties (but it's effectively the maximum divisor
  # as it maximizes size of the parliament, hence the name)
  data_frame <- data_frame %>%
    mutate(max_divisor = min(party_divisor_1))
  
  # new mandates
  data_frame <- data_frame %>%
    mutate(new_mandates = round(second_votes / max_divisor))
  
  # get new divisor (+0.5)
  data_frame <- data_frame %>%
    mutate(mandate_plus_05 = new_mandates + 0.5)
  
  # min divisor
  data_frame <- data_frame %>%
    mutate(party_divisor_2 = second_votes / mandate_plus_05)
  
  # max from previous step
  data_frame <- data_frame %>%
    mutate(min_divisor = max(party_divisor_2))
  
  # votes divided by new divisor
  data_frame <- data_frame %>%
    mutate(new_mandates2 = round(second_votes / min_divisor))
  
  # size parliament with both divisors
  data_frame <- data_frame %>%
    mutate(
      min_size = sum(data_frame$new_mandates),
      max_size = sum(data_frame$new_mandates2)
    )
  
  
  # Rounding the denominator:
  # create functions:
  round_to_next_tousand <- function(x) {
    round(x + 500, -3)
  }
  
  round_to_next_hundred <- function(x) {
    round(x + 50, -2)
  }
  
  round_to_next_ten <- function(x) {
    round(x + 1, -1)
  }
  
  # apply functions:
  data_frame <- data_frame %>%
    mutate(selected_divisor = round_to_next_tousand(min_divisor)) %>%
    mutate(selected_divisor = replace(
      selected_divisor,
      selected_divisor > max_divisor,
      round_to_next_hundred(min_divisor)
    )) %>%
    mutate(selected_divisor = replace(
      selected_divisor,
      selected_divisor > max_divisor,
      round_to_next_ten(min_divisor)
    )) %>%
    mutate(selected_divisor = replace(
      selected_divisor,
      selected_divisor > max_divisor,
      ceiling(min_divisor)
    )) %>%
    mutate(seats_party = round(second_votes / selected_divisor)) %>%
    mutate(final_size = sum(seats_party))
  
}