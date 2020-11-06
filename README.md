Ueberhangmandate
================

## Packages

``` r
library(tidyr)
library(dplyr)
library(lpSolveAPI)
library(rvest)
library(stringr)
library(grid)
library(ggplot2)
# packages for maps
library(rgdal)
library(rgeos)
library(mapproj)
# package for table
library(knitr)
```

## Theme

``` r
hp_theme <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      
      # Base elements which are not used directly but inherited by others
      line =              element_line(colour = '#DADADA', size = 0.75,
                                       linetype = 1, lineend = "butt"),
      rect =              element_rect(fill = "#F0F0F0", colour = "#F0F0F0",
                                       size = 0.5, linetype = 1),
      text =              element_text(family = base_family, face = "plain",
                                       colour = "#656565", size = base_size,
                                       hjust = 0.5, vjust = 0.5, angle = 0,
                                       lineheight = 0.9, margin = margin(), debug = FALSE),
      plot.margin =       margin(12,10,5,10),
      # Modified inheritance structure of text element
      plot.title =        element_text(size = rel(0.75), family = '' ,
                                       face = 'bold', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B'),
      plot.subtitle =     element_text(size = rel(0.4), family = '' ,
                                       face = 'plain', hjust = 0,
                                       vjust = 2.5, colour = '#3B3B3B', margin = margin(0,0,15,0)),
      axis.title.x =      element_blank(),
      axis.title.y =      element_blank(),
      axis.text =         element_text(),
      # Modified inheritance structure of line element
      axis.ticks =        element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      
      # Modified inheritance structure of rect element
      plot.background =   element_rect(),
      panel.background =  element_rect(),
      legend.key =        element_rect(colour = '#DADADA'),
      
      # Modifiying legend.position
      legend.position = 'none',
      
      complete = TRUE
    )
}
```

## Historic Size Bundestag Data Set

``` r
# read in source
url <- 'https://www.bundeswahlleiter.de/service/glossar/u/ueberhangmandate.html'
# first table [until 2009](before current election reform): no adjustments for Überhangmandate
first_part = url %>%
  read_html() %>%
  html_node(xpath = '/html/body/div/div/main/table[1]') %>%
  html_table(fill = TRUE)
# clean up
# first merge names and first row and clean names up. Second, cut only relevant information
first_part[] <- lapply(first_part, as.character)
names(first_part) <- paste(names(first_part), first_part[1, ], sep = "_")
first_part <- first_part %>%
  rename(`Jahr`=`Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
         `Wahlkreise`=`Zahl der Wahlkreise_Zahl der Wahlkreise`,
         `Mandate`=`Sitze insgesamt 1_Sitze insgesamt 1`) %>%
  slice(2:49) %>%
  select(c(-5)) %>%
  rename_all(funs(str_replace(., "Überhangmandate", "Ueberhangmandate")))

# second table [2013,2017](after current election reform): adjustments for Überhangmandate
second_part = url %>%
  read_html() %>%
  html_node(xpath = '/html/body/div/div/main/table[2]') %>%
  html_table(fill = TRUE)
# clean up
second_part[] <- lapply(second_part, as.character)
names(second_part) <- paste(names(second_part), second_part[1, ], sep = "_")
second_part <- second_part %>%
  rename(`Jahr`=`Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
         `Wahlkreise`=`Zahl der Wahlkreise_Zahl der Wahlkreise`,
         `Mandate`=`Sitze insgesamt_Sitze insgesamt`) %>%
  rename_all(funs(str_replace(., "Überhänge", "Ueberhangmandate"))) %>%
  slice(c(2:8)) %>%
  select(c(1:6))  

# merge data frames  
historic_size <- full_join(first_part,second_part)
historic_size[, 1:5] <- sapply(historic_size[, 1:5], as.numeric)
historic_size <- historic_size%>% mutate(Ueberhangmandate_Partei=replace(Ueberhangmandate_Partei, Ueberhangmandate_Partei=="–", NA)) 
# drop unnecessary data
rm(first_part,second_part, url)
```

## Historic Size Bundestag Graph

``` r
historic_size %>%
  distinct(`Jahr`, .keep_all = TRUE) %>%
  select(c(`Jahr`,`Mandate`)) %>%
  ggplot(aes(x=`Jahr`, y=`Mandate`,group=1)) +
  geom_line(aes(group=1), color="#009E73") +
  scale_y_continuous(limits = c(300, 800.1),breaks = c(seq(300,800,100)), expand = c(0, 0), labels=c("300" = "300", "400"="400", "500"="500", "600"= "600", "700"= "700","800"="800 Sitze")) +
  scale_x_continuous(breaks = seq(1949, 2017, by = 4), limits=c(1949,2017), labels=c("1949"="1949", "1953"="53", "1957"="57", "1961"="61", "1965"="65", "1969"="69", "1973"="73", "1977"="77", "1981"="81", "1985"="85", "1989"="89", "1993"="93", "1997"="97", "2001"="01", "2005"="05", "2009"="09", "2013"="13", "2017"="17")) +
  labs(title = "Historische Entwicklung der Größe des Bundestages", subtitle="",caption = "Quelle: Bundeswahlleiter") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(),plot.title.position = "plot",  axis.title.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.2, color="#656565"), axis.line.x=element_line( size=.3, color="black"), legend.position = "right", legend.key = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x =element_line( size=.3, color="black"), plot.caption=element_text(size=5), axis.text.x=element_text(color="black"))
```

![](README_figs/historic_size_graph-1.png)<!-- -->

``` r
ggsave("./HP_pic/historic_size_graph.jpg",width=4, height=3)
```

## Overview of Überhangmandaten for Parties

``` r
 historic_size %>%
    rename(Partei=`Ueberhangmandate_Partei`,
            `Überhangmandate_Anzahl`=`Ueberhangmandate_Anzahl`) %>%
    group_by(Partei) %>%
    filter(!is.na(Partei)) %>%
    summarise(`Überhangmandate (1949-2017)` = sum(Überhangmandate_Anzahl, rm.na=TRUE)) %>%
    kable()
```

| Partei | Überhangmandate (1949-2017) |
| :----- | --------------------------: |
| CDU    |                         112 |
| CSU    |                          11 |
| DP     |                           2 |
| SPD    |                          38 |

## 2017 Election Data Preparation

### get data

``` r
# get data
raw <- read.csv("https://www.bundeswahlleiter.de/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv", header=F, sep=";", skip=1, stringsAsFactors = F, na.strings="")
# keep raw data
cleaned <- raw
```

### clean up raw data

``` r
# paste party names to second vote ("Zweitstimme"); logic if cell contains "Zweitstimme" grab party name from row 5, 2 cells to the left and paste it in front
# similar logic for 'Erststimme'
for(i in 1:ncol(cleaned)){
  if(cleaned[6,i] %in% "Zweitstimmen"){
    cleaned[6,i] <- paste(cleaned[5,i-2], cleaned[6,i])
  }
  if(cleaned[6,i] %in% "Erststimmen"){
    cleaned[6,i] <- paste(cleaned[5,i], cleaned[6,i])
  }
}

# set new column names to names in sixth row
colnames(cleaned) <- cleaned[6,] 
# remove extra rows
cleaned <- cleaned[-(1:7),] 
# rename first to third column
colnames(cleaned)[1:3] <- c("wahlkreisnummer","wahlkreisname", "Bundesland") 
# remove rows with missing values for 'Bundesland' (state) variable
cleaned <- cleaned[!(is.na(cleaned$Bundesland)),] 

# delete every second row from the third row onwards; logic: keep all columns whose name contain the letter "n"
cleaned = cleaned[,grepl("*n",names(cleaned))] 
# every row expect the first two as numeric
number_col <- ncol(cleaned)
for(i in 3:number_col){
  cleaned[[i]] <- as.numeric(cleaned[[i]])
} 

# drop aggregate values for each state
cleaned<-cleaned[!(cleaned$`Bundesland`==99),]

# all columns as numeric
cleaned[, 3:97] <- sapply(cleaned[, 3:97], as.numeric)

# remove not needed values:
rm(i,number_col)

#export clean and raw data set
write.csv(raw,"raw.csv", row.names = TRUE)
write.csv(cleaned,"cleaned.csv", row.names = TRUE)
```

### double check all counties included:

``` r
nrow(cleaned)
```

    ## [1] 299

## Election Map

Get the highest number of votes for each voting district. Then rename
the parties to only include the ones included in the Bundestag to save
space. Add id for matching with the shape files.

``` r
map_actual_election <- cleaned %>% 
  select(starts_with("wahlkreis") | contains("Erststimme")) %>%
  select(-c(3:6)) %>%
  gather('Partei','Stimmen', `Christlich Demokratische Union Deutschlands Erststimmen`:`Übrige Erststimmen`) %>%
  group_by(wahlkreisnummer) %>%
  top_n(1, Stimmen) %>% 
  mutate(wahlkreisnummer=as.numeric(wahlkreisnummer)) %>%
  mutate(Partei=replace(Partei, Partei=="Christlich Demokratische Union Deutschlands Erststimmen", "CDU")) %>%
  mutate(Partei=replace(Partei, Partei=="Christlich Demokratische Union Deutschlands Erststimmen", "CDU")) %>%
  mutate(Partei=replace(Partei, Partei=="Sozialdemokratische Partei Deutschlands Erststimmen", "SPD")) %>%
  mutate(Partei=replace(Partei, Partei=="DIE LINKE Erststimmen", "LINKE")) %>%
  mutate(Partei=replace(Partei, Partei=="BÜNDNIS 90/DIE GRÜNEN Erststimmen", "GRÜNE")) %>%
  mutate(Partei=replace(Partei, Partei=="Christlich-Soziale Union in Bayern e.V. Erststimmen", "CSU")) %>%
  mutate(Partei=replace(Partei, Partei=="Freie Demokratische Partei Erststimmen", "FDP")) %>%
  mutate(Partei=replace(Partei, Partei=="Alternative für Deutschland Erststimmen", "AFD")) %>%
  mutate(Partei=replace(Partei, is.numeric(Partei), "Andere")) %>%
  select(starts_with("wahlkreis") | contains("Partei")) %>%
  mutate(id=wahlkreisnummer-1,id=as.character(id))
```

Add shape files

``` r
# all election maps borrows heavily from https://interaktiv.morgenpost.de/analyse-bundestagswahl-2017/data/btw17_analysis.html
# load state borders and shapes of the constituencies
shp_bund <- readOGR("btw17-shapes/bundeslaender_small.shp", "bundeslaender_small", stringsAsFactors=FALSE, encoding="latin1") %>% broom::tidy()
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/home/urza/Documents/backup 10.10/Daten/V/Visualisierungen/github/Ueberhangmandate_Bundestag/btw17-shapes/bundeslaender_small.shp", layer: "bundeslaender_small"
    ## with 16 features
    ## It has 4 fields
    ## Integer64 fields read as strings:  WKR_NR

``` r
wahlkreise <- readOGR("btw17-shapes/wahlkreise_small.shp", "wahlkreise_small", stringsAsFactors=FALSE, encoding="latin1") 
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/home/urza/Documents/backup 10.10/Daten/V/Visualisierungen/github/Ueberhangmandate_Bundestag/btw17-shapes/wahlkreise_small.shp", layer: "wahlkreise_small"
    ## with 299 features
    ## It has 4 fields
    ## Integer64 fields read as strings:  WKR_NR

``` r
shp_krs <- wahlkreise %>% broom::tidy() # broom to tidy shape data to make it work smoothly with the graphic package ggplot2
rm(wahlkreise)                                                  
map_actual_election <- merge(shp_krs, map_actual_election, by="id", all.y=T) # merge shapes with vote data by id
```

## Election Map 2017

``` r
# actual graph
ggplot(data=map_actual_election, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=Partei), show.legend = T) +
  geom_polygon(data=shp_krs, aes(x=long, y=lat, group=group), fill=NA, color="white", size=0.4) +
  scale_fill_manual(values=c("royalblue1", "#32302e", "blue4", "#46962b", "magenta1", "#E3000F")) +
  theme_void() + # remove axes
  coord_map() # apply projection
```

![](README_figs/map_actual_election-1.png)<!-- -->

# State Data Frames

Replace state numbers with names create a data set for each state
(Land=number to differentiate between states). Also only keep the
parties in the parliament

``` r
# replace numbers with names for states for easier understanding
state <- c("SCH","HAM","NDS","BRE","NRW","HES","RHN","BAD","BAY","SAR","BER","BRA","MEC","SAC","SAA","THU")
data_state <- paste( "state_",state, sep="")
# data frame / vector for the number of seats allocated for each state. The reason for doing this by hand
# stems from the fact that it has already been calculated by the Bundeswahlleiter in the past. Moreover, the formal rule of
# 2 * counties = seats does not always apply due to rounding. 
# source: https://www.bundeswahlleiter.de/dam/jcr/dd81856b-7711-4d9f-98dd-91631ddbc37f/btw17_sitzberechnung.pdf
seats <- c(22,12,59,5,128,43,30,76,93,7,24,20,13,32,17,17)
```

### split data frame into data frames for each state. We need this as later on each state is it’s own maxmization problem

``` r
for (i in 1:16) {
  cleaned <- cleaned %>% mutate(Bundesland=replace(Bundesland, Bundesland==i, state[i]))
  assign(paste0(data_state[i]), cleaned[cleaned$Bundesland==state[i],])
}
# drop values not needed:
rm(i)
```

### create dynamic state datasets

this is needed for the calculation of the amount of Ueberhangmandaten
and seats per party from a given state

``` r
dynamic_state <- paste( "dynamic_",state, sep="")

for (i in 1:16) {
  # create loop dummy datasets so coding gets easier
  loop <-  data.frame(partei =c("CDU","SPD","LINKE","GRÜNE","CSU","FDP","AFD"),
                     stringsAsFactors = FALSE)
  loop_state <- get(data_state[i])
  # Sitzkontingent state
  loop$Sitzkontingent <- seats[i]
  # sum Zweitstimmen
  loop <- loop %>% mutate(sum_zweitstimmen=sum(loop_state$`Gültige Zweitstimmen`))
  # divisor: votes per seat
  loop <- loop %>% mutate(divisor=sum_zweitstimmen/Sitzkontingent)
  # Zweitstimmen per party
  loop$zweitstimmen <- 0
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="CDU", sum(loop_state$`Christlich Demokratische Union Deutschlands Zweitstimmen`, na.rm = TRUE))) 
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="SPD", sum(loop_state$`Sozialdemokratische Partei Deutschlands Zweitstimmen`, na.rm = TRUE)  ))         
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="LINKE", sum(loop_state$`DIE LINKE Zweitstimmen`, na.rm = TRUE)  ))                                     
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="GRÜNE", sum(loop_state$`BÜNDNIS 90/DIE GRÜNEN Zweitstimmen`, na.rm = TRUE)  ))                         
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="CSU", sum(loop_state$`Christlich-Soziale Union in Bayern e.V. Zweitstimmen`, na.rm = TRUE)  ))         
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="FDP", sum(loop_state$`Freie Demokratische Partei Zweitstimmen`, na.rm = TRUE)  ))                      
  loop <- loop %>% mutate(zweitstimmen=replace(zweitstimmen, partei=="AFD", sum(loop_state$`Alternative für Deutschland Zweitstimmen`, na.rm = TRUE)  ))
  # save to data frame  
  assign(paste0(dynamic_state[i]), loop,)
  # remove not needed values
  rm(loop,loop_state)
  }
```

Get right mandate allocation per party per state for 2017 election.
Note: in the method described by law you would only add or subtract one
(instead of 10). However, this will make the code run significantly
longer. If you have time to spare feel free to modify the code.

``` r
  # 6. Sitze der jeweiligen Parteien:
# create vector so you can test later on if seats allocated is how the Bundeswahlleiter determined it to be
test <- c()
for (i in 1:16) {
# create loop dummy datasets so coding gets easier
loop_state <- get(dynamic_state[i])
# votes to mandates per party
loop_state <- loop_state %>% mutate(mandate=round(zweitstimmen/divisor))
# sum mandates
number <- sum(loop_state$mandate)
# get mandates allocated to state
allocated_seats <- seats[i]

  # too few allocated seats: solution decrease divisor for convergence from votes to seats
  while (allocated_seats > number){
    loop_state$divisor <- loop_state$divisor-10
    loop_state <- loop_state %>% mutate(mandate=round(zweitstimmen/divisor))
    number <- sum(loop_state$mandate)
  }
  # too many allocated seats: solution increase divisor for convergence from votes to seats
  while (number > allocated_seats){
    loop_state$divisor <- loop_state$divisor+10
    loop_state <- loop_state %>% mutate(mandate=round(zweitstimmen/divisor))
    number <- sum(loop_state$mandate)
  }
# add number for checking
test[i] <- sum(loop_state$mandate)

# save to data frame  
assign(paste0(dynamic_state[i]), loop_state,)

}
```

``` r
# check whether operation successful
all.equal(seats, test)
```

    ## [1] TRUE

``` r
# cleanup
rm(loop_state,number,allocated_seats,test)
```

# Optimal Allocation

``` r
# Placeholder
```
