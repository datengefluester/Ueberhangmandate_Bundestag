###############################################################################
## Packages
###############################################################################

# for creating the gif data frame as otherwise >50mb:
library(dplyr)
library(tidyr)
library(stringr)
library(grid) # plots
library(ggplot2) # plots
library(ggpubr) # plots next to each other
library(rgdal) # for maps
library(rgeos) # for maps
library(mapproj) # for maps
library(gganimate) # for gif
library(transformr) # for gif


###############################################################################
# Theme
###############################################################################

source("./scripts/02_themes.R")



###############################################################################
# Shape Files
###############################################################################

shp_federal <- readOGR("data/btw17-shapes/bundeslaender_small.shp",
  "bundeslaender_small",
  stringsAsFactors = FALSE,
  encoding = "latin1"
) %>%
  broom::tidy()


shp_constituencies <- readOGR("data/btw17-shapes/wahlkreise_small.shp",
  "wahlkreise_small",
  stringsAsFactors = FALSE,
  encoding = "latin1"
) %>%
  broom::tidy()


###############################################################################
## Historic Size Bundestag Graph
###############################################################################

# read in data
graph_historic_size <- read.csv("./data/graph_data/graph_historic_size.csv")


# graph
graph_historic_size %>%
  ggplot(aes(x = year, y = size, group = 1)) +
  geom_line(aes(group = 1), color = "#009E73", size = 0.65) +
  geom_hline(yintercept = 598, size = 0.2, linetype='longdash', color = "black") + 
  geom_vline(xintercept = 1990, size = 0.075, linetype='dotted', color = "black") +
  annotate("text",
           x = 1990.5, y = 790, label = "Wiedervereinigung",
           size = 1.5, color = "black", hjust = 0
  ) +
  scale_y_continuous(
    position = "right",
    limits = c(300, 820),
    breaks = c(300, 400, 500, 700, 800),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = sort(c(seq(1960, 2022, by = 10), 1949)),
    limits = c(1945, 2021),
    labels = c(
      "1949" = "1949", "1960" = "1960", "1970" = "1970",
      "1980" = "1980", "1990" = "1990", "2000" = "2000",
      "2010" = "2010", "2020" = "2020"
    ),
    expand = c(0, 0)
  ) +
  labs(
    title = "Historische Entwicklung der Größe des Bundestages",
    caption = "*Seit 2003\n Quelle: Bundeswahlleiter"
  ) +
  annotate("text",
    x = 1945, y = 810, label = "800",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
           x = 1945, y = 790, label = "Sitze",
           size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 710, label = "700",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 608, label = "598",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
           x = 1945, y = 588, label = "(Normgröße*)",
           size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 510, label = "500",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 410, label = "400",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945.5, y = 350, label = "\u03df",
    size = 2, color = "black", hjust = 0
  ) +
  line_chart_theme()

# save graph
ggsave("./images/graph_historic_size.jpg", width = 4, height = 3)


###############################################################################
## Dynamic Size Bundestag Graph
###############################################################################

# read in data
graph_dynamic_size <- read.csv("./data/graph_data/graph_dynamic_size.csv")

# graph
graph_dynamic_size %>%
  ggplot(aes(x = cummulative_votes, y = final_size, group = 1)) +
  geom_line(aes(group = 1), color = "#009E73", size = 0.65) +
  geom_hline(yintercept = 598, size = 0.2, linetype='longdash', color = "black") + 
  scale_y_continuous(
    position = "right",
    limits = c(570, 735),
    breaks = c(625, 650, 675, 700, 725),
    expand = c(0, 0)
  ) +
  annotate("text",
    x = 260000, y = 729, label = "725",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 721, label = "Sitze im Bundestag",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 704, label = "700",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 679, label = "675",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 653, label = "650",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 629, label = "625",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 602, label = "598",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 593, label = "(Normgröße)",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 256000, y = 580, label = "\u03df",
    size = 2, color = "black", hjust = 1
  ) +
  scale_x_continuous(
    breaks = c(0,60000,120000,180000,240000),
    limits = c(-5000, 260000),
    expand = c(0, 0),
    labels = c(
      "0" = "0", "60000" = "60.000",
      "120000" = "120.000",
      "180000" = "180.000",
      "240000" = "240.000"
    )
  ) +
  labs(
    title = "Größe des Bundestages nach Erststimmenverschiebung",
    subtitle = "in verschobenen Stimmen.",
    caption = "Quelle: Bundeswahlleiter, \n Eigene Berechnungen",
    x = "Verschobene Erststimmen") +
  line_chart_theme() +
  theme(axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(margin = unit(c(0, 0, -0.5, 0), "line")))

# save graph
ggsave("./images/dynamic_size_graph_pic.jpg", width = 4, height = 3)


###############################################################################
## Size Parliament Different Scenarios
###############################################################################

graph_parliament_sizes_scenarios <- read.csv("./data/graph_data/graph_parliament_sizes_scenarios.csv")

graph_parliament_sizes_scenarios %>%  
  ggplot(aes(x = reorder(scenario, -scenario_number), y = parliament_size)) +
  geom_bar(stat = "identity", fill = "#1b9e77") +
  coord_flip() +
  scale_x_discrete(labels = c('Umfrage INSA vom 29.3.2021 \nReform 2020',
                              'Umfrage INSA vom 29.3.2021',
                              'Bundestagswahl 2017 \nReform 2020',
                              'Bundestagswahl 2017 ')) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "left",
    breaks = c(300,600,900),
    limits = c(0,950)
  ) +
  labs(
    title = "Größe Bundestag unter verschiedenen Szenarien",
    caption = "Quelle: Bundeswahlleiter \nEigene Berechnungen"
  ) +
  bar_chart_theme() +
  theme(axis.title = element_blank())
  
ggsave("./images/graph_parliament_sizes_scenarios.jpg", width = 4, height = 3)



###############################################################################
## Election Map 2017
###############################################################################

# read in data
map_actual_election <- read.csv("./data/graph_data/map_actual_election.csv")

# Relevel group factor
map_actual_election <- map_actual_election %>% 
  mutate(party = str_trim(party)) %>%
  mutate(party = replace(party, party=="greens", "grüne")) %>% 
  mutate(party = toupper(party)) %>% 
  mutate(party = factor(party, levels = c("CDU", "LINKE", "CSU", "GRÜNE", "SPD", "AFD")))


# whole Germany map
map_actual_election %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = party),
               show.legend = T
  ) +
  geom_polygon(
    data = shp_constituencies,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "white",
    size = 0.025
  ) +
  geom_polygon(
    data = shp_federal,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "white", size = 0.1
  ) +
  
  scale_fill_manual(values = c(
    "#32302e", "magenta1", "royalblue1",
    "#46962b", "#E3000F", "blue4"
  )) +
  labs(
    title = "Direktmandate Bundestagswahl 2017",
    caption = "Quelle: Bundeswahlleiter"
  ) +
  coord_map() + # apply projection
  map_theme() 

# save map
ggsave("./images/map_actual_election.jpg", width = 2, height = 2.7)





###############################################################################
# Optimized Election Map
###############################################################################

# read in data
map_optimized_election <- read.csv("./data/graph_data/map_optimized_election.csv")

# Relevel group factor
map_optimized_election <- map_optimized_election %>% 
  mutate(change = str_trim(change)) %>% 
  mutate(change = replace(change, change=="greens", "grüne")) %>% 
  mutate(change = toupper(change)) %>% 
  mutate(change = factor(change, levels = c("CDU", "LINKE", "CSU", "GRÜNE", "SPD", "AFD")))


# map
ggplot(data = map_optimized_election, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = change), show.legend = T) +
  geom_polygon(
    data = shp_constituencies,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "white",
    size = 0.025
  ) +
  geom_polygon(
    data = shp_federal,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "white", size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "#32302e", "magenta1", "royalblue1",
      "#46962b", "#E3000F", "blue4"
    ),
    name = "Neues Direktmandat",
    na.translate = F
  ) +
  labs(
    title = "Direktmandate nach Stimmenverschiebung",
    caption = "Quelle: Bundeswahlleiter \nEigene Berechnungen"
  ) +
  coord_map() + # apply projection
  map_theme()

# save map
ggsave("./images/optimized_election_map_pic.jpg", width = 2, height = 2.7)



###############################################################################
### Creating GIF Data Frame
### Note: this part relies on the running 'graph dynamic size' first
###############################################################################

# read in data
map_gif <- read.csv("./data/graph_data/map_gif.csv")
map_optimized_election <- read.csv("./data/graph_data/map_optimized_election.csv")
graph_dynamic_size <- read.csv("./data/graph_data/graph_dynamic_size.csv")

# create combination for each county with each number of votes ('states')
# of the gif
gif <- map_gif %>%
  select(constituency_number) %>%
  mutate(
    "1" = "0",
    "2" = 20000,
    "3" = 40000,
    "4" = 60000,
    "5" = 80000,
    "6" = 100000,
    "7" = 120000,
    "8" = 140000,
    "9" = 160000,
    "10" = 180000,
    "11" = 200000,
    "12" = 220000,
    "13" = 240000
  ) %>%
  gather(2:14, key = tmp, value = Votes) %>%
  mutate(Votes = as.numeric(Votes)) %>%
  select(-c("tmp")) %>%
  mutate(size = 709) %>%
  mutate(
    size = replace(size, Votes == "20000", 700),
    size = replace(size, Votes == "40000", 700),
    size = replace(size, Votes == "60000", 685),
    size = replace(size, Votes == "80000", 685),
    size = replace(size, Votes == "100000", 671),
    size = replace(size, Votes == "120000", 654),
    size = replace(size, Votes == "140000", 625),
    size = replace(size, Votes == "160000", 625),
    size = replace(size, Votes == "180000", 625),
    size = replace(size, Votes == "200000", 619),
    size = replace(size, Votes == "220000", 619),
    size = replace(size, Votes == "240000", 619)
  ) %>%
  full_join(., map_optimized_election, by = "constituency_number")


# create data frame with all counties. Actual and optimized vote, cumulative
# votes and size parliament
gif <- graph_dynamic_size %>%
  filter(constituency_number != "Election") %>%
  mutate(constituency_number = as.numeric(constituency_number)) %>%
  select(c(1, 8, 14)) %>%
  right_join(gif, by = "constituency_number") %>%
  mutate(cummulative_votes = replace(
    cummulative_votes,
    is.na(cummulative_votes),
    0
  ))

# replace to NA if not a überhangmandate for given number of votes
# and also create text for the dynamic subtitle
gif <- gif %>%
  mutate(mandate_map = mandate_actual) %>%
  rowwise() %>%
  mutate(mandate_map = replace(
    mandate_map,
    cummulative_votes <= Votes,
    NA
  )) %>%
  mutate(mandate_map = replace(mandate_map, is.na(mandate_map), "white")) %>%
  mutate(mandate_map = as.factor(mandate_map)) %>%
  mutate(Votes = format(as.numeric(Votes), scientific = FALSE)) %>%
  mutate(States = as.character(Votes)) %>%
  mutate(States = paste(States, size, sep = ". Größe Bundestag: ")) %>%
  mutate(States = str_squish(States)) %>%
  mutate(States = str_replace(States, "0000", "0.000")) %>%
  mutate(States = str_replace(States, "10.0000", "100.000")) %>%
  mutate(States = str_replace(States, "20.0000", "200.000")) %>%
  mutate(States = replace(
    States,
    States == "100.000. Größe Bundestag: 709",
    "100.000. Größe Bundestag: 671"
  )) %>%
  mutate(States = replace(
    States,
    States == "200.000. Größe Bundestag: 709",
    "200.000. Größe Bundestag: 619"
  ))

# modify states as factors
level <- unique(gif$States)
gif$States <- factor(gif$States, levels = level)

# define colors for parties
Colors <- c(
  "csu" = "royalblue1",
  "cdu" = "#32302e",
  "greens" = "#46962b",
  "linke" = "magenta1",
  "spd" = "#E3000F",
  "afd" = "blue4",
  "fdp" = "#ffed00",
  "white" = "white"
)


# gif graph layout
p_gif <- gif %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mandate_map), show.legend = T) +
  geom_polygon(
    data = shp_constituencies,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "black", size = 0.025
  ) +
  geom_polygon(
    data = shp_federal,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "black", size = 0.1
  ) +
  scale_fill_manual(
    values = Colors,
    name = "Neues Direktmandat",
    na.translate = F
  ) +
  labs(
    title = "Dynamische Veränderung der Überhangmandate\n2017",
    subtitle = "Wählerstimmen verschoben: {closest_state}",
    caption = "Quelle: Bundeswahlleiter \n Eigene Berechnungen"
  ) +
  map_theme() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = rel(2.5),
                              face = "bold",
                              margin = unit(c(0, 0, 1.5, 0), "line")),
    plot.subtitle = element_text(size = rel(1.5),
                                 hjust = 0.5,
                                 margin = unit(c(0, 0, -1, 0), "line")),
    plot.caption = element_text(size = rel(1))
  )

# add transition states
p_gif <- p_gif + transition_states(States,
                                   transition_length = 5,
                                   state_length = 80
)

# render gif
animate(p_gif, height = 800, width = 800)

# save gif
anim_save("./images/dynamic_map.gif", width = 2, height = 2.7)

###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))
