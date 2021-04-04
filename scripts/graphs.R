###############################################################################
## Packages
###############################################################################

# for creating the gif data frame as otherwise >50mb
library(dplyr)
library(tidyr)
library(stringr)
# plots
library(grid)
library(ggplot2)
# plots next to each other
library(ggpubr)
# packages for maps
library(rgdal)
library(rgeos)
library(mapproj)
# for gif
library(gganimate)
library(transformr)


###############################################################################
# Theme
###############################################################################

source("./scripts/themes.R")



###############################################################################
# Shape Files
###############################################################################

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


###############################################################################
## Historic Size Bundestag Graph
###############################################################################

# read in data
graph_historic_size <- read.csv("./data/graph_historic_size.csv")

# graph
graph_historic_size %>%
  ggplot(aes(x = year, y = mandates, group = 1)) +
  geom_line(aes(group = 1), color = "#009E73") +
  geom_hline(aes(yintercept = 598)) +
  scale_y_continuous(
    position = "right",
    limits = c(300, 820),
    breaks = c(300, 400, 500, 598, 700, 800),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = sort(c(seq(1960, 2022, by = 10), 1949)),
    limits = c(1945, 2021),
    labels = c(
      "1949" = "1949", "1960" = "60", "1970" = "70",
      "1980" = "80", "1990" = "90", "2000" = "2000",
      "2010" = "10", "2020" = "20"
    ),
    expand = c(0, 0)
  ) +
  labs(
    title = "Historische Entwicklung der Größe des Bundestages",
    caption = "*Seit 2003\n Quelle: Bundeswahlleiter"
  ) +
  annotate("text",
    x = 1945, y = 800, label = "800 \nSitze",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 710, label = "700",
    size = 2, color = "black", hjust = 0
  ) +
  annotate("text",
    x = 1945, y = 598, label = "598 \n(Normgröße*)",
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
ggsave("./pictures/graph_historic_size.jpg", width = 4, height = 3)




###############################################################################
## Distribution of size of counties by Erststimmen cast
###############################################################################

# Distribution Graph  ---

# read in data
graph_distribution <- read.csv("./data/graph_distribution.csv")

# graph
distribution <- graph_distribution %>%
  ggplot(aes(y = percent_eligible_first_vote, x = eligible_first_vote)) +
  geom_smooth(span = 0.1, color = "#009E73") +
  scale_x_continuous(
    breaks = c(seq(160000, 260000, 25000)),
    labels = c(
      "160000" = "160", "185000" = "185", "210000" = "210",
      "235000" = "235", "260000" = "260.000"
    )
  ) +
  scale_y_continuous(
    position = "right",
    limits = c(0, 1.05),
    breaks = c(seq(0.25, 1, 0.25)),
    expand = c(0, 0)
  ) +
  annotate("text",
    x = 260000, y = 1.025, label = "100%",
    size = 2, color = "black", hjust = 0.6
  ) +
  annotate("text",
    x = 260000, y = 0.975, label = "der Wahlkreise",
    size = 2, color = "black", hjust = 0.85
  ) +
  annotate("text",
    x = 260000, y = 0.775, label = "75",
    size = 2, color = "black", hjust = 0.2
  ) +
  annotate("text",
    x = 260000, y = 0.525, label = "50",
    size = 2, color = "black", hjust = 0.2
  ) +
  annotate("text",
    x = 260000, y = 0.275, label = "25",
    size = 2, color = "black", hjust = 0.2
  ) +
  labs(
    title = "Größe der Wahlkreise",
    subtitle = "nach wahlberechtigten Erststimmen oder weniger.*"
  ) +
  coord_cartesian(clip = "off") +
  line_chart_theme() +
  theme(
    axis.text = element_text(size = 5)
  )


# Distribution Graph  ---

# read in data
graph_margins <- read.csv("./data/graph_margins.csv")

# graph
margins <- graph_margins %>%
  ggplot(aes(y = percent_, x = margin)) +
  geom_smooth(span = 0.1, color = "#009E73") +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = c(seq(0.25, 1, 0.25)),
    expand = c(0, 0)
  ) +
  annotate("text",
    x = 73500, y = 1.025, label = "100%",
    size = 2, color = "black", hjust = 0.65
  ) +
  annotate("text",
    x = 73500, y = 0.975, label = "der Wahlkreise",
    size = 2, color = "black", hjust = 0.85
  ) +
  annotate("text",
    x = 73500, y = 0.775, label = "75",
    size = 2, color = "black", hjust = 0.2
  ) +
  annotate("text",
    x = 73500, y = 0.525, label = "50",
    size = 2, color = "black", hjust = 0.2
  ) +
  annotate("text",
    x = 73500, y = 0.275, label = "25",
    size = 2, color = "black", hjust = 0.2
  ) +
  scale_x_continuous(
    breaks = c(seq(0, 70000, 17000)),
    labels = c(
      "0" = "0", "17000" = "17", "34000" = "34",
      "51000" = "51", "70000" = "70.000"
    )
  ) +
  labs(
    title = "Abstand Erst- und Zweitwahl nach Erststimme",
    subtitle = "kummulierten Abstand oder weniger.*"
  ) +
  coord_cartesian(clip = "off") +
  line_chart_theme() +
  theme(
    axis.text = element_text(size = 5)
  )


# combine graphs margin and distribution size ---

figure <- ggarrange(margins, distribution,
  ncol = 2, nrow = 1,
  common.legend = FALSE
)

figure <- annotate_figure(figure,
  bottom = text_grob("* in 1.000. \nQuelle: Bundeswahlleiter, \nEigene Berechnungen",
    color = "#3B3B3B",
    hjust = 1, x = 0.98,
    size = 4
  ),
)

figure <- figure + bgcolor("#F0F0F0") + border("#F0F0F0")
figure

# save graph
ggsave("./pictures/graph_margins_distribution.jpg", width = 4, height = 3)




###############################################################################
## Dynamic Size Bundestag Graph
###############################################################################

# read in data
graph_dynamic_size <- read.csv("./data/graph_dynamic_size.csv")

# graph
graph_dynamic_size %>%
  ggplot(aes(x = cummulative_votes, y = final_size, group = 1)) +
  geom_line(aes(group = 1), color = "#009E73") +
  scale_y_continuous(
    position = "right",
    limits = c(570, 735),
    breaks = c(625, 650, 675, 700, 725),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = 598, linetype='dashed', size = 0.2, color = "#656565") + 
  annotate("text",
    x = 260000, y = 729, label = "725",
    size = 2, color = "black", hjust = 1
  ) +
  annotate("text",
    x = 260000, y = 721, label = "Sitze",
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
    breaks = sort(c(seq(0, 240000,
      by = 60000
    ))),
    limits = c(-5000, 260000),
    expand = c(0, 0),
    labels = c(
      "0" = "0", "60000" = "60",
      "120000" = "120",
      "180000" = "180",
      "240000" = "240.000"
    )
  ) +
  labs(
    title = "Größe des Bundestages nach Ersttimmenverschiebung",
    subtitle = "in 1.000 verschobenen Stimmen.",
    caption = "Quelle: Bundeswahlleiter, \n Eigene Berechnungen"
  ) +
  line_chart_theme()

# save graph
ggsave("./pictures/dynamic_size_graph_pic.jpg", width = 4, height = 3)



###############################################################################
## Election Map 2017
###############################################################################

# read in data
map_actual_election <- read.csv("./data/map_actual_election.csv")

# Relevel group factor
map_actual_election <- map_actual_election %>% 
  mutate(party = str_trim(party)) %>%
  mutate(party = replace(party, party=="greens", "grüne")) %>% 
  mutate(party = toupper(party)) %>% 
  mutate(party = factor(party, levels = c("CDU", "LINKE", "CSU", "GRÜNE", "SPD", "AFD")))

# map
map_actual_election %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = party),
               show.legend = T
  ) +
  geom_polygon(
    data = shp_wahlkreise,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "white",
    size = 0.025
  ) +
  geom_polygon(
    data = shp_bund,
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
ggsave("./pictures/map_actual_election.jpg", width = 2, height = 2.7)


###############################################################################
# Optimized Election Map
###############################################################################

# read in data
map_optimized_election <- read.csv("./data/map_optimized_election.csv")

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
    data = shp_wahlkreise,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "white",
    size = 0.025
  ) +
  geom_polygon(
    data = shp_bund,
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
ggsave("./pictures/optimized_election_map_pic.jpg", width = 2, height = 2.7)



###############################################################################
### Creating GIF Data Frame
### Note: this part relies on the running 'graph dynamic size' first
###############################################################################

# read in data
map_gif <- read.csv("./data/map_gif.csv")

# create combination for each county with each number of votes ('states')
# of the gif
gif <- map_gif %>%
  select(district_number) %>%
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
  full_join(., map_optimized_election, by = "district_number")


# create data frame with all counties. Actual and optimized vote, cumulative
# votes and size parliament
gif <- graph_dynamic_size %>%
  filter(district_number != "Election") %>%
  mutate(district_number = as.numeric(district_number)) %>%
  select(c(1, 8, 14)) %>%
  right_join(gif, by = "district_number") %>%
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
    data = shp_wahlkreise,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "black", size = 0.025
  ) +
  geom_polygon(
    data = shp_bund,
    aes(x = long, y = lat, group = group),
    fill = NA, color = "black", size = 0.1
  ) +
  scale_fill_manual(
    values = Colors,
    name = "Neues Direktmandat",
    na.translate = F
  ) +
  labs(
    title = "Überhangmandate",
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
anim_save("./pictures/dynamic_map.gif", width = 2, height = 2.7)


###############################################################################
# clear console
###############################################################################
rm(list = ls(all.names = TRUE))
