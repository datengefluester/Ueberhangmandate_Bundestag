





source("./scripts/mandate_allocation_state.R")

mandate_allocation_state("parties_state")



mandate_allocation_state <- function(percentage_types) {
  
  
  test <- c()
  
  for (state in 1:16) {
    
    # create loop dummy data sets so coding gets easier
    loop_state <- get(percentage_types[state])
    
    # obtain votes to mandates per party
    loop_state <- loop_state %>% mutate(
      mandates =
        round(zweitstimmen / divisor)
    )
    # get sum mandates
    number <- sum(loop_state$mandates)
    
    # get mandates allocated to state
    allocated_seats <- mandates_state[state]
    
    # too few allocated mandates:
    # decrease denominator for convergence of votes to mandates
    # Note as required by law: This uses simple rounding for decimal places
    while (allocated_seats > number) {
      loop_state$divisor <- loop_state$divisor - 10
      loop_state <- loop_state %>% mutate(
        mandates =
          round(zweitstimmen / divisor)
      )
      number <- sum(loop_state$mandates)
    }
    
    # too many allocated seats:
    # increase denominator for convergence of votes to seats
    while (number > allocated_seats) {
      loop_state$divisor <- loop_state$divisor + 10
      loop_state <- loop_state %>% mutate(
        mandates =
          round(zweitstimmen / divisor)
      )
      number <- sum(loop_state$mandates)
    }
    
    # add number for checking
    test[state] <- sum(loop_state$mandates)
    
    # save to data frame
    assign(paste0(percentage_types[state]), loop_state, )
    
    # clean up
    rm(loop_state, number, allocated_seats, state)
  }