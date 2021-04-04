###############################################################################
## Packages
###############################################################################

library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(rgdal) # for reading in shape file
library(lpSolveAPI) # optimization
library(rvest) # web scrapping
library(stringr) # structure replacement Überhang to Ueberhang etc.
library(knitr) # package for tables
library(janitor) # for better variable names

###############################################################################
## read in size calculation functions
###############################################################################

source("./scripts/parliament_size_calculation.R")

###############################################################################
## calculate changes in votes obtained compared to the actual result of the election
###############################################################################

survey_percent <- data.frame(
  party = c("cdu", "csu", "spd", "greens", "fdp", "linke", "afd"),
  election = c(32.8, 32.8, 20.5, 8.9, 10.7, 9.2, 12.7)
)

# add survey values (INSA 29.03.2021)
survey_percent <- survey_percent %>%
  mutate(survey = NA) %>%
  mutate(
    survey = replace(survey, party == "cdu", 26),
    survey = replace(survey, party == "csu", 26),
    survey = replace(survey, party == "spd", 18),
    survey = replace(survey, party == "greens", 21),
    survey = replace(survey, party == "fdp", 10.5),
    survey = replace(survey, party == "linke", 7),
    survey = replace(survey, party == "afd", 11)
  ) %>%
  mutate(change_factor = survey / election)

###############################################################################
## Historic Size Bundestag Data Set
###############################################################################

# read in from source
url <- "https://www.bundeswahlleiter.de/service/glossar/u/ueberhangmandate.html"

# as the data is presented in two tables on the web page, we need to read them
# in separately. First table [until 2009](before current election reform):
# no adjustments for Überhangmandate
table <- url %>%
  read_html() %>%
  html_node(xpath = "/html/body/div/div/main/table[1]") %>%
  html_table(fill = TRUE)
# clean up:
# First, the first row contains names needed in the variable names.
table[] <- lapply(table, as.character)
names(table) <- paste(names(table), table[1, ], sep = "_")
# Second, apply consistent and more intuitive names
# and keep only relevant variables
graph_historic_size <- table %>%
  rename(
    `year` = `Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
    `counties` = `Zahl der Wahlkreise_Zahl der Wahlkreise`,
    `mandates` = `Sitze insgesamt 1_Sitze insgesamt 1`
  ) %>%
  slice(2:49) %>%
  select(c(-5)) %>%
  rename_all(funs(str_replace(
    ., "Überhangmandate",
    "ueberhangmandates"
  )))

# Second table [2013,2017](after current election reform):
# adjustments for Überhangmandate
table <- url %>%
  read_html() %>%
  html_node(xpath = "/html/body/div/div/main/table[2]") %>%
  html_table(fill = TRUE)
# clean up (see above) and merge with first data frame
table[] <- lapply(table, as.character)
names(table) <- paste(names(table), table[1, ], sep = "_")
table <- table %>%
  rename(
    `year` = `Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
    `counties` = `Zahl der Wahlkreise_Zahl der Wahlkreise`,
    `mandates` = `Sitze insgesamt_Sitze insgesamt`
  ) %>%
  rename_all(funs(str_replace(., "Überhänge", "ueberhangmandates"))) %>%
  slice(-c(1)) %>%
  slice(c(1:7)) %>%
  select(c(1:6))

# merge tables
graph_historic_size <- rbind(graph_historic_size, table)


# drop data frames and url, which are not needed anymore
rm(table, url)


# merge data frames and make everything numeric as it's needed for the graph
graph_historic_size[, 1:5] <- sapply(graph_historic_size[, 1:5], as.numeric)

# replace "-" for zero Ueberhangmandate for a party in a given year with NA
graph_historic_size <- graph_historic_size %>% mutate(ueberhangmandates_Partei = replace(
  ueberhangmandates_Partei,
  ueberhangmandates_Partei == "–", NA
))

# as the data frame contains every Ueberhangmandat per party,
# I only need one observation per year. However, the remaining years also need
# to have numbers to achieve abrupt changes in the graph. Hence, the fill
# commands.

years <- data.frame("year" = 1949:2020, stringsAsFactors = FALSE)

graph_historic_size <- graph_historic_size %>%
  distinct(`year`, .keep_all = TRUE) %>%
  select(c(`year`, `mandates`)) %>%
  left_join(years, ., by = "year") %>%
  fill(`mandates`)

rm(years)

###############################################################################
## 2017 Election Data Preparation
###############################################################################

# get data from the Bundeswahlleiter (Federal Returning Officer)
raw <- read.csv("https://www.bundeswahlleiter.de/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv",
  header = F,
  sep = ";",
  skip = 1,
  stringsAsFactors = F,
  na.strings = ""
)

# keep raw data as backup
write.csv(raw, "./data/raw.csv", row.names = TRUE)




###############################################################################
### clean up raw data
###############################################################################

# For now the data frame has no proper column names. Hence, I obtain party names
# and have all names in row 6 so I can name variables in one line. Idea:
# paste party names in front of second vote ("Zweitstimme");
# Logic: if cell contains "Zweitstimme" grab the party name from row 5 and then
# 2 cells to the left of the cell and paste content it in front.
# Similar logic for 'Erststimme'.
cleaned <- raw

for (i in 1:ncol(cleaned)) {
  if (cleaned[6, i] %in% "Zweitstimmen") {
    cleaned[6, i] <- paste(cleaned[5, i - 2], cleaned[6, i])
  }
  if (cleaned[6, i] %in% "Erststimmen") {
    cleaned[6, i] <- paste(cleaned[5, i], cleaned[6, i])
  }
  rm(i)
}

# set new column names to names in sixth row
colnames(cleaned) <- cleaned[6, ]

# additional clean up:
# rename first to third column manually for more intuitive names
colnames(cleaned)[1:3] <- c("Wahlkreisnummer", "Wahlkreisname", "Bundesland")

# remove extra rows
cleaned <- cleaned[-(1:7), ]

# Drop numbers from previous election (unnamed columns);
# logic: keep all columns whose name contain the letter "n"
cleaned <- cleaned[, grepl("*n", names(cleaned))]

# remove rows with missing values for 'Bundesland' (state) variable,
# as these have NAs for all columns.
cleaned <- cleaned[!(is.na(cleaned$Bundesland)), ]

# Convert every row expect the first two as numeric
# (we're counting votes after all, right?)
cleaned[, 3:97] <- sapply(cleaned[, 3:97], as.numeric)

# drop aggregate values for each state as we care about counties primarily
cleaned <- cleaned[!(cleaned$`Bundesland` == 99), ]


# rename variables
cleaned <- cleaned %>% 
  rename(district_number = Wahlkreisnummer,
         district_name = Wahlkreisname,
         state = Bundesland) %>% 
  rename_all(
    funs(str_replace(., "Erststimmen", "first_vote"))
  ) %>% 
  rename_all(
    funs(str_replace(., "Zweitstimmen", "second_vote"))
  ) %>% 
  rename_all(
    funs(str_replace(., "Wahlberechtigte", "eligible_"))
  ) %>% 
  rename_all(
    funs(str_replace(., "Wähler", "voters_"))
  ) %>% 
  rename_all(
    funs(str_replace(., "Ungültige", "invalid_"))
  ) %>% 
  rename_all(
    funs(str_replace(., "Gültige", "valid_"))
  ) %>% 
  janitor::clean_names()

# rename the seven parties represented in German Parliament with short forms
# for better readability.
cleaned <- cleaned %>%
  rename_all(
    funs(str_replace(., "christlich_demokratische_union_deutschlands", "cdu"))
  ) %>%
  rename_all(
    funs(str_replace(., "sozialdemokratische_partei_deutschlands", "spd"))
  ) %>%
  rename_all(
    funs(str_replace(., "die_linke", "linke"))
  ) %>%
  rename_all(
    funs(str_replace(., "bundnis_90_die_grunen", "greens"))
  ) %>%
  rename_all(
    funs(str_replace(., "christlich_soziale_union_in_bayern_e_v", "csu"))
  ) %>%
  rename_all(
    funs(str_replace(., "freie_demokratische_partei", "fdp"))
  ) %>%
  rename_all(
    funs(str_replace(., "alternative_fur_deutschland", "afd"))
  )


### Check to see if all counties included
# At the time of the 2017 election there were 299 counties in Germany.
nrow(cleaned)

# export clean data set as backup
write.csv(cleaned, "./data/cleaned.csv", row.names = TRUE)

###############################################################################
## Election Map: Direct Mandates Data Frame
###############################################################################

# Get the highest number of votes for each voting district, as the party, which
# received most votes gets the mandate. Then rename the parties (I only include
# the ones included in the Bundestag to save space). Additionally, add id for
# matching with the shape files needed for the map in the next step.
map_actual_election <- cleaned %>%
  select(starts_with("district") | contains("first_vote")) %>%
  select(-c(3:6)) %>%
  gather(
    "party", "votes",
    cdu_first_vote: ubrige_first_vote
  ) %>%
  group_by(district_number) %>%
  top_n(1, votes) %>%
  mutate(district_number = as.numeric(district_number)) %>%
  mutate(party = str_replace(party, "_first_vote", "")) %>%
  select(starts_with("district") | contains("party")) %>%
  mutate(id = district_number - 1, id = as.character(id))

# check if results like actual election results:
# https://w.wiki/m6G (German Wikipedia entry for the election)
map_actual_election %>%
  group_by(party) %>%
  tally() %>%
  rename("direct mandates" = "n") %>%
  kable()

##########################
### Add shape files
#########################

# all election maps borrow heavily from
# https://interaktiv.morgenpost.de/analyse-bundestagswahl-2017/data/btw17_analysis.html
# load state borders and shapes of the constituencies, which are needed for the
# maps. Additionally, use broom to the get data to work with ggplot2
shp_bund <- readOGR("data/btw17-shapes/bundeslaender_small.shp",
  "bundeslaender_small",
  stringsAsFactors = FALSE,
  encoding = "latin1"
) %>%
  broom::tidy()
shp_wahlkreise <- readOGR("data/btw17-shapes/wahlkreise_small.shp",
  "wahlkreise_small",
  stringsAsFactors = FALSE,
  encoding = "latin1"
) %>%
  broom::tidy()


# merge shapes of counties with party, which received most votes by id
map_actual_election <- merge(shp_wahlkreise,
  map_actual_election,
  by = "id",
  all.y = T
)


###############################################################################
# Create State Data Frames
###############################################################################

# Replace state numbers with names create a data set for each state
# (Land=number to differentiate between states).
# Also only keep the parties in the parliament

# Create a vector for the number of seats allocated for each state.
# The reason for doing this by hand stems from the fact that it has already been
# calculated by the Bundeswahlleiter in the past. Moreover, the formal rule of
# 2 * counties = seats does not always apply due to rounding.
# source:
# https://www.bundeswahlleiter.de/dam/jcr/dd81856b-7711-4d9f-98dd-91631ddbc37f/btw17_sitzberechnung.pdf
# The order of seats follows the order of states used in the following steps.
mandates_state <- c(22, 12, 59, 5, 128, 43, 30, 76, 93, 7, 24, 20, 13, 32, 17, 17)


#------------
### Split the cleaned data frame into data frames for each state.
#------------

# We need this as later on each state is its own maximization problem in terms of
# mandates when avoiding Ueberhangmandates.

# Create a list of short names of each states; lists names for of counties-state
# and state-party data frames. This allows for running loops over these lists.
# Additionally replace numbers with names for states for easier understanding
# in the cleaned data frame.
state_short <- c(
  "sch", "ham", "nds", "bre", "nrw", "hes", "rhn", "bad", "bay",
  "sar", "ber", "bra", "mec", "sac", "saa", "thu"
)
counties_state <- paste("counties_", state_short, sep = "")
parties_state <- paste("parties_", state_short, sep = "")

# splitting into individual state data frames and replace numbers
for (state_number in 1:16) {
  cleaned <- cleaned %>% mutate(state = replace(
    state,
    state == state_number,
    state_short[state_number]
  ))
  assign(
    paste0(counties_state[state_number]),
    cleaned[cleaned$state == state_short[state_number], ]
  )
  rm(state_number)
}


#------------
### Create party-state data frames
#------------

# This is needed for the calculation of the amount of Ueberhangmandates and the
# mandates per party as both are determined on the state level for the initial
# allocation.

for (state_number in 1:16) {
  # create loop a dummy data frame so coding gets easier
  loop <- data.frame(
    party = c("cdu", "spd", "linke", "greens", "csu", "fdp", "afd"),
    stringsAsFactors = FALSE
  )

  # Obtain mandates reserved in parliament in state
  loop$mandates_state <- mandates_state[state_number]

  # select state data frame
  loop_state <- get(counties_state[state_number])

  # sum Zweitstimmen (second vote)
  loop <- loop %>% mutate(
    sum_valid_second_vote =
      sum(loop_state$valid_second_vote)
  )

  # denominator: votes needed per seat
  loop <- loop %>% mutate(divisor = sum_valid_second_vote / mandates_state)

  # Votes per party in the individual states
  loop$second_votes_party <- 0
  loop <- loop %>%
    mutate(
      second_votes_party = replace(
        second_votes_party, party == "cdu",
        sum(loop_state$cdu_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "spd",
        sum(loop_state$spd_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "linke",
        sum(loop_state$linke_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "greens",
        sum(loop_state$greens_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "csu",
        sum(loop_state$csu_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "fdp",
        sum(loop_state$fdp_second_vote,
          na.rm = TRUE
        )
      ),
      second_votes_party = replace(
        second_votes_party, party == "afd",
        sum(loop_state$afd_second_vote,
          na.rm = TRUE
        )
      )
    )

  # save to data frame
  assign(paste0(parties_state[state_number]), loop, )

  # clean up
  rm(loop, loop_state, state_number)
}


# ---- adjust survey data frame numbers by polling numbers and create data frames
survey_parties_state <- paste0("survey_", parties_state)

for (state_number in 1:16) {

  # create loop dummy data sets so coding gets easier
  loop_state <- get(parties_state[state_number])

  loop_state <- loop_state %>%
    left_join(., survey_percent, by = "party") %>%
    mutate(second_votes_party = round(second_votes_party * change_factor)) %>%
    select(-c(election, survey, change_factor))

  # save to data frame
  assign(paste0(survey_parties_state[state_number]), loop_state, )

  # clean up
  rm(loop_state, state_number)
}


###############################################################################
### Get right mandate allocation per party per state for 2017 election.
###############################################################################

# Note: for the method described by law you would only add or subtract 1
# (instead of 10). However, this will make the code run significantly slower.
# If you have time to spare, feel free to modify the code.

# mandates per party
# create vector so you can test later on if seats allocated matches what
# the Bundeswahlleiter calculated.

# create lists for both survey results and actual results
combined_states <- c(parties_state, survey_parties_state)
repeated_mandates <- c(mandates_state, mandates_state)

test <- c()

for (state_number in 1:length(combined_states)) {

  # create loop dummy data sets so coding gets easier
  loop_state <- get(combined_states[state_number])

  # obtain votes to mandates per party
  loop_state <- loop_state %>% mutate(
    mandates =
      round(second_votes_party / divisor)
  )
  # get sum mandates
  number <- sum(loop_state$mandates)

  # get mandates allocated to state
  allocated_seats <- repeated_mandates[state_number]

  # too few allocated mandates:
  # decrease denominator for convergence of votes to mandates
  # Note as required by law: This uses simple rounding for decimal places
  while (allocated_seats > number) {
    loop_state$divisor <- loop_state$divisor - 10
    loop_state <- loop_state %>% mutate(
      mandates =
        round(second_votes_party / divisor)
    )
    number <- sum(loop_state$mandates)
  }

  # too many allocated seats:
  # increase denominator for convergence of votes to seats
  while (number > allocated_seats) {
    loop_state$divisor <- loop_state$divisor + 10
    loop_state <- loop_state %>% mutate(
      mandates =
        round(second_votes_party / divisor)
    )
    number <- sum(loop_state$mandates)
  }

  # add number for checking
  test[state_number] <- sum(loop_state$mandates)

  # save to data frame
  assign(paste0(combined_states[state_number]), loop_state, )

  # clean up
  rm(loop_state, number, allocated_seats, state_number)
}

##########################
### Checks and clean ups
##########################

# check whether amount of seats as calculated by the Bundeswahlleiter matches
# with the calculated numbers per state.
# (delete last 16 as they are the same as the first 16 by construction - as survey included)
mandates_state_2 <- c(mandates_state,mandates_state)
all.equal(mandates_state_2, test)

# double check: check if seats assigned to party in state NRW (most populous state)
# equal:
# https://www.bundeswahlleiter.de/dam/jcr/dd81856b-7711-4d9f-98dd-91631ddbc37f/btw17_sitzberechnung.pdf
# (p.6)
nrw <- data.frame(
  party = c("cdu", "spd", "linke", "greens", "csu", "fdp", "afd"),
  mandates = c(43, 35, 10, 10, 0, 17, 13)
)
nrw$party <- as.character(nrw$party)

parties_nrw %>%
  select(1, 6) %>%
  all.equal(nrw)



# clean up
rm(nrw, test)

# tripple check: whether mandates by party add up to numbers from
# Bundeswahlleiter: get all mandates per party from each state
# This is also needed for minimum amount of mandates per party, when calculating
# the seats in parliament

# create dummy data frame
party_second_vote_mandates <- parties_sch %>% select(party, mandates)

# loop over the remaining states
for (state_number in 2:16) {
  loop_state <- get(parties_state[state_number])
  loop_state <- loop_state %>% select(party, mandates)
  party_second_vote_mandates <- rbind(
    party_second_vote_mandates,
    loop_state
  )
  rm(state_number)
}

# get sum and see if they add up to 598
sum(party_second_vote_mandates$mandate)

# get sum of mandates for each party (this should equal final distribution)
party_second_vote_mandates <- party_second_vote_mandates %>%
  group_by(party) %>%
  summarise(mandates_second_vote = sum(mandates))

# create output for markdown file
party_second_vote_mandates %>%
  arrange(-mandates_second_vote) %>%
  rename("Partei" = party, "Mandate nach Zweitstimmen" = mandates_second_vote) %>%
  kable()



###############################################################################
# Optimal Allocation preparation
###############################################################################

for (state_number in 1:16) {

  # create loop dummy data sets so coding gets easier
  loop_state <- get(counties_state[state_number])

  # drop all variables for 'Zweitstimme' (second vote), as we are only
  # concerned with 'Erststimme' (first/direct vote) from now on.
  # (The sum of second votes is saved in the "state_XXX" data frames
  # to be able to double check)
  loop_state <- loop_state[, !grepl("second_vote", names(loop_state))]

  # Only keep parties represented in the parliament as only mandates for these
  # parties will decrease the overall size Additionally, rename them for easier
  # readability.
  loop_state <- loop_state %>%
    select(c(1:3, 8:14)) %>%
    rename(
      cdu = cdu_first_vote,
      spd = spd_first_vote,
      linke = linke_first_vote,
      greens = greens_first_vote,
      csu = csu_first_vote,
      fdp = fdp_first_vote,
      afd = afd_first_vote
    )

  # Get the highest amount of 'Erststimmen' for each 'Wahlkreis':

  # Replace NA wit -1 - this means -1 for parties which had no candidate.
  # A negative number is required here as later on when calculating the votes
  # needed, negative numbers result in higher numbers, which means they're not
  # selected. (see. next step)
  loop_state[is.na(loop_state)] <- -1

  # get highest number of votes for each 'Wahlkreis' so we can calculate
  # differences in votes for each party to optimize later on.
  loop_state$max <- apply(loop_state[, 4:10], 1, max)

  # get difference from max of votes for each 'Wahlkreis' per party:
  # we need this so we can apply the minimization optimization in the next step.
  for (x in 1:nrow(loop_state)) {
    max <- loop_state[x, 11]
    for (y in 4:10) {
      loop_state[x, y] <- max - loop_state[x, y]
    }
  }

  # obtain the margin from highest to second highest in a variable, so it can
  # easily be used for plotting
  loop_state$margin <- apply(
    loop_state[, 4:10],
    1,
    function(x) names(sort(x)[2])
  )

  # sort by 'wahlkreisnummer' - so you can match by row number later on, as matrix
  # from minimization has no 'wahlkreisnummer' but it sorted by rows.
  # Also drop max of votes received as no longer needed, as we only care
  # about changes.
  loop_state <- loop_state %>%
    mutate(district_number = as.numeric(district_number)) %>%
    arrange(district_number) %>%
    mutate(matching = 1:nrow(loop_state)) %>%
    select(-c(max))

  # save to data frame
  assign(paste0(counties_state[state_number]), loop_state, )

  # cleanup
  rm(loop_state, state_number, x, y, max)
}



###############################################################################
# Actual Optimization
###############################################################################

# !!! Important: we only do this for actual election results
# - however, survey results can easily be included  !!!

# Note: we have two constraints:
#  1. The amount of mandates in total awarded to a party should not exceed the
# amount of mandates a party is entitled to according to the Zweitstimmen
# (second votes).
# 2. Each county (wahlkreis) should only have one direct candidate.

# for each state
for (state_number in 1:16) {
  # create loop dummy datasets so coding gets easier
  loop_state <- get(parties_state[state_number])

  # 1. constrain: Zweitstimmenmandate per party
  max_seats <- loop_state$mandates


  # Assign the counties data frame to a dummy data frame, as we need like to match
  # the optimization results with the votes per party later on.
  loop_state <- get(counties_state[state_number])

  # keep only variables needed for calculation (difference from maximum
  # votes per party) as this is the only number that matters for the optimization.
  # Keep in mind we want to minimize the votes, which need to be shifted to
  # achieve the minimum size of the parliament. The change to a matrix is needed
  # for the following operations to work.
  calculation <- loop_state %>%
    select(4:10) %>%
    as.matrix()

  # the follow steps borrow heavily from:
  # https://stackoverflow.com/questions/31813686/lpsolve-in-r-with-character-and-column-sum-contraints

  # obtain dimensions of the calculation matrix
  # (wahlkreis=counties; parteien=parties)
  number_districts <- nrow(calculation)
  number_parties <- ncol(calculation)
  ncol <- number_districts * number_parties

  lp_matching <- make.lp(ncol = ncol)

  # we want integer assignments and minimize the votes changed for the optimal
  # solution with no Ueberhangmandaten
  set.type(lp_matching, columns = 1:ncol, type = c("integer"))
  set.objfn(lp_matching, calculation)
  lp.control(lp_matching, sense = "min")

  # 2. constrain: one mandate per county (wahlkreis)

  # solution vector which contains 1s with length equal to number of 'wahlkreisen'
  max_one_candidate_constraint <- replicate(number_districts, 1)

  add_max_district_constraint <- function(district_index) {
    # relevant columns for a wahlkreis
    district_cols <- (0:(number_parties - 1)) * number_districts + district_index
    add.constraint(lp_matching,
      rep(1, number_parties),
      indices = district_cols,
      type = c("="),
      rhs = 1
    )
  }

  # Add a max_number constraint for each wahlkreis
  lapply(1:number_districts, add_max_district_constraint)

  # Max values for each party (first constraint)
  mandate.value <- rep(1, number_districts) # Add that each mandate is worth on mandate

  add_max_mandates_constraint <- function(party_index) {
    # relevant columns for a party
    party_cols <- (party_index - 1) * number_districts + (1:number_districts)
    add.constraint(lp_matching,
      xt = mandate.value,
      indices = party_cols,
      rhs = max_seats[party_index]
    )
  }


  # Add a max_number constraint for each party
  lapply(1:number_parties, add_max_mandates_constraint)

  # Actual calculation and obtain dummies indicating, to which party a
  # wahlkreis goes to
  solve(lp_matching)
  get.variables(lp_matching)

  # create dataset (ncol=7  as seven parties in parliament)
  calculation <- get.variables(lp_matching) %>%
    matrix(ncol = 7) %>%
    as.data.frame() %>%
    rename(
      cdu_optimized = 1,
      spd_optimized = 2,
      linke_optimized = 3,
      greens_optimized = 4,
      csu_optimized = 5,
      fdp_optimized = 6,
      afd_optimized = 7
    )

  # create a  variable for matching
  calculation$matching <- 1:nrow(calculation)

  # matching
  calculation <- merge(loop_state, calculation, by = "matching")

  # save to data frame
  assign(paste0(counties_state[state_number]), calculation, )

  # clean up
  rm(
    state_number, lp_matching, mandate.value, max_one_candidate_constraint, max_seats,
    number_parties, number_districts, ncol, add_max_mandates_constraint,
    add_max_district_constraint, calculation, loop_state
  )
}


###############################################################################
# Creating Variables
###############################################################################
# create empty data frame, which are used to fill in values from all the
# counties:
# difference in votes first and second party in a county
graph_margins <- data.frame(district_number = numeric(), margin = numeric())

# all relevant information for all counties from all states
counties <- data.frame(
  district_number = numeric(),
  district_name = numeric(),
  state = character(),
  mandate_actual = character(),
  mandate_optimized = character(),
  votes_needed = numeric(),
  votes_halved = numeric()
)


for (state_number in 1:16) {
  clean_up <- get(counties_state[state_number])
  # variable indicating winner when optimized
  clean_up <- clean_up %>%
    mutate(mandate_optimized = "cdu") %>%
    mutate(
      mandate_optimized = replace(
        mandate_optimized,
        spd_optimized == 1,
        "spd"
      ),
      mandate_optimized = replace(
        mandate_optimized,
        linke_optimized == 1,
        "linke"
      ),
      mandate_optimized = replace(
        mandate_optimized,
        greens_optimized == 1,
        "greens"
      ),
      mandate_optimized = replace(
        mandate_optimized,
        csu_optimized == 1,
        "csu"
      ),
      mandate_optimized = replace(
        mandate_optimized,
        fdp_optimized == 1,
        "FPD"
      ),
      mandate_optimized = replace(
        mandate_optimized,
        afd_optimized == 1,
        "afd"
      )
    )
  
  
# get actual direct Mandate Party name for each Wahlkreis (logic: normal winner = 0)
  clean_up <- clean_up %>%
    mutate(mandate_actual = "cdu") %>%
    mutate(
      mandate_actual = replace(mandate_actual, spd == 0, "spd"),
      mandate_actual = replace(mandate_actual, linke == 0, "linke"),
      mandate_actual = replace(mandate_actual, greens == 0, "greens"),
      mandate_actual = replace(mandate_actual, csu == 0, "csu"),
      mandate_actual = replace(mandate_actual, fdp == 0, "fdp"),
      mandate_actual = replace(mandate_actual, afd == 0, "afd")
    )

  # voter change needed for optimization
  clean_up <- clean_up %>%
    mutate(votes_needed = 0) %>%
    mutate(
      votes_needed = replace(
        votes_needed,
        cdu_optimized == 1,
        cdu[cdu_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        spd_optimized == 1,
        spd[spd_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        linke_optimized == 1,
        linke[linke_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        greens_optimized == 1,
        greens[greens_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        csu_optimized == 1,
        csu[csu_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        afd_optimized == 1,
        afd[afd_optimized == 1]
      ),
      votes_needed = replace(
        votes_needed,
        fdp_optimized == 1,
        fdp[fdp_optimized == 1]
      )
    ) %>%
    as.data.frame()

  #  divide by two as half amount would already amount to flip county
  clean_up <- clean_up %>% mutate(votes_halved = ceiling(votes_needed / 2))

  # margins between first and second as data frame for graph (not optimized!!!)
  clean_up <- clean_up %>%
    mutate(
      margin = replace(
        margin,
        margin == "cdu",
        cdu[margin == "cdu"]
      ),
      margin = replace(
        margin,
        margin == "spd",
        spd[margin == "spd"]
      ),
      margin = replace(
        margin,
        margin == "linke",
        linke[margin == "linke"]
      ),
      margin = replace(
        margin,
        margin == "greens",
        greens[margin == "greens"]
      ),
      margin = replace(
        margin,
        margin == "csu",
        csu[margin == "csu"]
      ),
      margin = replace(
        margin,
        margin == "afd",
        afd[margin == "afd"]
      ),
      margin = replace(
        margin,
        margin == "fdp",
        fdp[margin == "fdp"]
      )
    ) %>%
    as.data.frame()

  margins_placeholder <- clean_up %>% select(district_number, margin)
  graph_margins <- rbind(graph_margins, margins_placeholder)




  # only keep variables needed
  clean_up <- clean_up %>%
    select(-c(1, 5:19)) %>%
    relocate(any_of(c(
      "district_number",
      "district_name",
      "state",
      "mandate_actual",
      "mandate_optimized",
      "votes_needed",
      "votes_halved"
    )))

  # merge with data frame for analysis
  counties <- rbind(counties, clean_up)
  # cleanup
  rm(
    margins_placeholder, clean_up, state_number
  )
}

###############################################################################
# Some clean up
###############################################################################

# drop data frames for counties and states , which are not needed anymore:
rm(list = c(counties_state, parties_state))
# drop lists for counties and parties, number of seats per state
rm(counties_state, parties_state, mandates_state)


###############################################################################
# Some check ups
###############################################################################

# Amount of direct mandates per party
counties %>%
  group_by(mandate_actual) %>%
  tally() %>%
  rename(
    "Anzahl" = "n",
    "Direktmandate" = "mandate_actual"
  ) %>%
  kable()

# Optimized amount of direct mandates per party
counties %>%
  group_by(mandate_optimized) %>%
  tally() %>%
  rename(
    "Anzahl" = "n",
    "Optimierte Direktmandate" = "mandate_optimized"
  ) %>%
  kable()

# Movement direct mandates per party from actual to optimized
counties %>%
  filter(mandate_actual != mandate_optimized) %>%
  group_by(mandate_actual, mandate_optimized) %>%
  tally() %>%
  arrange(-n) %>%
  rename(
    "Anzahl" = "n",
    "Direktmandate von:" = "mandate_actual",
    "nach:" = "mandate_optimized"
  ) %>%
  kable()

# Note: all but the these three are changed from cdu / csu (Bavaria)
counties %>%
  filter(mandate_actual != mandate_optimized & mandate_optimized == "cdu") %>%
  select(1:5) %>%
  rename(
    "Von:" = "mandate_actual",
    "nach:" = "mandate_optimized"
  ) %>%
  kable()


###############################################################################
# Optimizied Election Map
###############################################################################

# number of county as number (needs to be transformed to id for county borders)
counties$district_number <- as.numeric(counties$district_number)
# create id variable and a variable, which indicates if the direct mandates
# changed after optimization
map_optimized_election <- counties %>%
  mutate(change = mandate_actual) %>%
  rowwise() %>%
  mutate(change = replace(
    change,
    mandate_actual != mandate_optimized,
    mandate_optimized
  )) %>%
  mutate(
    id = district_number - 1,
    id = as.character(id)
  )
# add shape files
map_optimized_election <- merge(shp_wahlkreise,
  map_optimized_election,
  by = "id",
  all.y = T
)



###############################################################################
# Distribution of Size of Counties by Erststimmen cast
###############################################################################

graph_distribution <- cleaned %>%
  select(district_name, eligible_first_vote, valid_first_vote) %>%
  arrange(desc(eligible_first_vote)) %>%
  mutate(percent_eligible_first_vote = 1:n() / 299)

graph_margins$margin <- as.numeric(graph_margins$margin)
# arrange numbers and calculate the percent of counties
graph_margins <- graph_margins %>%
  arrange(desc(margin)) %>%
  mutate(percent_ = 1:n() / 299)



###############################################################################
# Create a data frame for Ueberhangmandates only
###############################################################################

# This is needed for calculation of the dynamic size of parliament.

# kick out counties, where no changed occurred after optimization
ueberhangmandate <- counties %>%
  filter(mandate_actual != mandate_optimized) %>%
  arrange(votes_needed) %>%
  mutate(rank = 1:n())
# cumulative votes (sum from all votes, which had a closer margin; meaning less
# votes need to be shifted for the county to flip)
ueberhangmandate$cummulative_votes <- 0
for (row in 1:nrow(ueberhangmandate)) {
  ueberhangmandate[row, 9] <- sum(ueberhangmandate$votes_halved[1:row])
  rm(row)
}


# reform 2020: only keep 3 ueberhangmandates per party per state

# check no state has two parties with ueberhangmandates
tmp <- unique(ueberhangmandate[c("state","mandate_actual")]) 
duplicated(tmp$state)
rm(tmp)

# only keep three 
ueberhangmandate_reform <- ueberhangmandate %>% 
  group_by(state) %>%
  slice_min(order_by = votes_needed, n =3)
  


###############################################################################
# Calculate the Size of German Parliament after 2017 Election
###############################################################################

# recreate table: https://w.wiki/mBZ
# recall we already calculated Zweitstimmen mandates per party earlier as
# a checkup to see whether mandates add up to 598
head(party_second_vote_mandates)

# get ueberhangmandate
ueberhang_parties <- ueberhangmandate %>%
  group_by(mandate_actual) %>%
  tally() %>%
  rename(
    party = mandate_actual,
    ueberhang_mandate = n
  )

party_second_vote_mandates <- left_join(
  party_second_vote_mandates,
  ueberhang_parties,
  by = "party"
) %>%
  mutate(ueberhang_mandate = replace(
    ueberhang_mandate,
    is.na(ueberhang_mandate),
    0
  ))
# clean up
rm(ueberhang_parties)

# minimum amount of seats in parliament
party_second_vote_mandates <- party_second_vote_mandates %>%
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate)

# get votes per party country wide
second_votes <- cleaned %>%
  select(seq(13, 25, 2)) %>%
  pivot_longer(c(1:7), names_to = "party", values_to = "second_votes") %>%
  group_by(party) %>%
  summarise(second_votes = sum(second_votes, na.rm = TRUE)) %>%
  mutate( party = str_replace(party, "_second_vote","" ))
    
     
# merge
party_second_vote_mandates <- left_join(
  party_second_vote_mandates,
  second_votes,
  by = "party"
)
rm(second_votes)


# add reform ueberhang mandates as own data frame : replace number of ueberhangmandates
reform_party_second_vote_mandates <- ueberhangmandate_reform %>% 
  group_by(mandate_actual) %>% 
  tally() %>% 
  rename(party = mandate_actual) %>% 
  left_join(party_second_vote_mandates, ., by = "party")

reform_party_second_vote_mandates <- reform_party_second_vote_mandates %>% 
  mutate(ueberhang_mandate = n) %>% 
  select(-n) %>% 
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate)


#-------
# create data frame for surveys (add survey data frame and adjust votes accordingly)
#------

# get second votes from each state
survey_mandates <- data.frame()

for (state_number in 1:16) {
  loop_state <- get(survey_parties_state[state_number])

  loop_state <- loop_state %>%
    mutate(state = state_short[state_number]) %>%
    select(state, party, mandates, second_votes_party) %>% 
    rename(mandates_second_vote = mandates)
  
  survey_mandates <- rbind(
    survey_mandates,
    loop_state
  )
  rm(loop_state, state_number)
}

# add direct mandates
# (note: I assume that changes in direct mandates are unrelated to changes in 
# second vote mandates)
survey_mandates <- counties %>%
  group_by(state, mandate_actual) %>%
  tally() %>%
  rename(direct_mandates = n, party = mandate_actual) %>%
  left_join(survey_mandates, ., by = c("state", "party")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  mutate(ueberhangmandates = direct_mandates - mandates_second_vote) %>% 
  mutate(ueberhangmandates = replace(ueberhangmandates, ueberhangmandates <0, 0))

# create survey reform data frame
reform_survey_mandates <- survey_mandates %>% 
  mutate(ueberhangmandates = replace (ueberhangmandates, ueberhangmandates>3, 3))

# get mandate and vote numbers
survey_mandates <- survey_mandates %>%
  group_by(party) %>%
  summarise(
    mandates_second_vote = sum(mandates_second_vote),
    second_votes = sum(second_votes_party),
    ueberhang_mandate = sum(ueberhangmandates)
  ) %>%
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate) %>%
  select(party, mandates_second_vote, ueberhang_mandate, mandate_minimum, second_votes)

reform_survey_mandates <- reform_survey_mandates %>% 
  group_by(party) %>%
  summarise(
    mandates_second_vote = sum(mandates_second_vote),
    second_votes = sum(second_votes_party),
    ueberhang_mandate = sum(ueberhangmandates)
  ) %>%
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate) %>%
  select(party, mandates_second_vote, ueberhang_mandate, mandate_minimum, second_votes)


# Some clean up
rm(list = c(survey_parties_state))
rm(survey_parties_state)


# Reform: 3 Ueberhangmandate rest is balanced with mandates from second vote from
# other states

# survey result:
reform_survey_mandates <- survey_mandates %>%
  mutate(ueberhang_mandate = replace(ueberhang_mandate, ueberhang_mandate>3, 3)) %>% 
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate)

# election result:
reform_party_second_vote_mandates <- party_second_vote_mandates %>% 
  mutate(ueberhang_mandate = replace(ueberhang_mandate, ueberhang_mandate>3, 3)) %>% 
  mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate)



# ----------------
# calculation size
# ----------------
survey_mandates <- size_calculation("survey_mandates")
reform_survey_mandates <- size_calculation("reform_survey_mandates")
party_second_vote_mandates <- size_calculation("party_second_vote_mandates")
reform_party_second_vote_mandates <- size_calculation("reform_party_second_vote_mandates")

# different sizes for the different scenarios:

parliament_sizes_scenarios <- data.frame()

scenarios <- c("survey_mandates","reform_survey_mandates","party_second_vote_mandates","reform_party_second_vote_mandates")

for (scenario_df in scenarios){
loop_df <- get(scenario_df)
parliament_sizes_scenarios <- loop_df %>% 
  slice(1) %>% 
  select(final_size) %>% 
  rename(parliament_size = final_size) %>% 
  mutate(scenario = scenario_df) %>% 
  rbind(parliament_sizes_scenarios) %>% 
  arrange(parliament_size)
}


# paste size parliament into ueberhang mandate date frame
# note again this is not done for the survey results
ueberhangmandate <- ueberhangmandate %>%
  mutate(
    min_size = sum(party_second_vote_mandates$new_mandates),
    max_divisor = mean(party_second_vote_mandates$max_divisor),
    max_size = sum(party_second_vote_mandates$new_mandates2),
    min_divisor = mean(party_second_vote_mandates$min_divisor),
    selected_divisor = mean(party_second_vote_mandates$selected_divisor),
    final_size = mean(party_second_vote_mandates$final_size)
  )

# save values actual election to allow comparison
actual_election <- ueberhangmandate %>%
  select(10:15) %>%
  slice(1)



###############################################################################
# Dynamic Size Parliament
###############################################################################

# for each Ueberhangmandat:
for (row in 1:nrow(ueberhangmandate)) {
  # get party, from which a mandate has to be substracted
  party_subtract <- ueberhangmandate$mandate_actual[row]
  # get amount of mandates per party
  cdu <- party_second_vote_mandates[[2, 3]]
  csu <- party_second_vote_mandates[[3, 3]]
  spd <- party_second_vote_mandates[[7, 3]]

  # subtract a seat from the corresponding party
  # (note only cdu,csu and spd have Ueberhangmandate)
  party_second_vote_mandates <- party_second_vote_mandates %>%
    mutate(
      ueberhang_mandate =
        replace(
          ueberhang_mandate,
          party_subtract == "cdu" & party == "cdu",
          cdu - 1
        ),
      ueberhang_mandate =
        replace(
          ueberhang_mandate,
          party_subtract == "csu" & party == "csu",
          csu - 1
        ),
      ueberhang_mandate = replace(
        ueberhang_mandate,
        party_subtract == "spd" & party == "spd",
        spd - 1
      )
    ) %>%
    as.data.frame()

  # minimum amount of seats in parliament
  party_second_vote_mandates <- party_second_vote_mandates %>%
    mutate(mandate_minimum = mandates_second_vote + ueberhang_mandate)

  # calculation size
  party_second_vote_mandates <- size_calculation("party_second_vote_mandates")

  # paste size parliament into ueberhang mandate date frame
  ueberhangmandate[[row, 10]] <- party_second_vote_mandates[[1, 14]]
  ueberhangmandate[[row, 11]] <- party_second_vote_mandates[[1, 8]]
  ueberhangmandate[[row, 12]] <- party_second_vote_mandates[[1, 15]]
  ueberhangmandate[[row, 13]] <- party_second_vote_mandates[[1, 12]]
  ueberhangmandate[[row, 14]] <- party_second_vote_mandates[[1, 16]]
  ueberhangmandate[[row, 15]] <- party_second_vote_mandates[[1, 18]]

  rm(row, party_subtract)
}

# clean up values:
rm(cdu, csu, spd)

# for better readability: omit decimal numbers
ueberhangmandate$max_divisor <- as.integer(ueberhangmandate$max_divisor)
ueberhangmandate$min_divisor <- as.integer(ueberhangmandate$min_divisor)

# add original result and drop not needed variable rank
# values can be look up in acutal_election data frame
ueberhangmandate <- ueberhangmandate %>%
  add_row(
    votes_needed = 0,
    votes_halved = 0,
    cummulative_votes = 0,
    min_size = 709,
    max_size = 709,
    final_size = 709,
    max_divisor = 62394.27,
    min_divisor = 62202.28
  ) %>%
  replace(is.na(.), "Election") %>%
  arrange(cummulative_votes) %>%
  select(-c(rank))




###############################################################################
### Exporting all data frames for graphs
###############################################################################

 graph_dynamic_size <- ueberhangmandate
 map_gif <- counties
 
 path <- "./data/"
 exports <- c(
   "graph_dynamic_size", "graph_historic_size", "graph_margins",
   "graph_distribution", "graph_historic_size", "map_actual_election",
   "map_optimized_election", "map_gif", "parliament_sizes_scenarios"
 )
 lapply(exports, function(x) {
   write.csv(get(x),
     paste(path, paste(x, "csv", sep = "."), sep = ""),
     row.names = FALSE
   )
 })
 rm(path, exports)


###############################################################################
### Finalising the data set for Shiny Dashboard
###############################################################################
 dashboard <- map_optimized_election %>%
   mutate(district_number = as.character(district_number)) %>%
   left_join(., ueberhangmandate, by = c("district_number", "district_name", "state", "mandate_actual", "mandate_optimized", "votes_needed", "votes_halved")) %>%
   mutate(Ueberhang = ifelse(votes_needed == 0, 1, 0))
 write.csv(dashboard, "./data/dashboard.csv", row.names = TRUE)


###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))
