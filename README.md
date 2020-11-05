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
library(rgdal)
library(rgeos)
library(mapproj)
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
  rename(`Jahr der Bundestagswahl`=`Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
         `Zahl der Wahlkreise`=`Zahl der Wahlkreise_Zahl der Wahlkreise`,
         `Sitze insgesamt`=`Sitze insgesamt 1_Sitze insgesamt 1`) %>%
  slice(2:49) %>%
  select(c(-5))

# second table [2013,2017](after current election reform): adjustments for Überhangmandate
second_part = url %>%
  read_html() %>%
  html_node(xpath = '/html/body/div/div/main/table[2]') %>%
  html_table(fill = TRUE)
# clean up
second_part[] <- lapply(second_part, as.character)
names(second_part) <- paste(names(second_part), second_part[1, ], sep = "_")
second_part <- second_part %>%
  rename(`Jahr der Bundestagswahl`=`Jahr der Bundestagswahl_Jahr der Bundestagswahl`,
         `Zahl der Wahlkreise`=`Zahl der Wahlkreise_Zahl der Wahlkreise`,
         `Sitze insgesamt`=`Sitze insgesamt_Sitze insgesamt`) %>%
  rename_all(funs(str_replace(., "Überhänge", "Überhangmandate"))) %>%
  slice(c(2:8)) %>%
  select(c(1:6))  

# merge data frames  
historic_values <- full_join(first_part,second_part)
historic_values[, 1:5] <- sapply(historic_values[, 1:5], as.numeric)
# drop unnecessary data
rm(first_part,second_part, url)
```

## Historic Size Bundestag Graph

``` r
historic_values %>%
  distinct(`Jahr der Bundestagswahl`, .keep_all = TRUE) %>%
  select(c(`Jahr der Bundestagswahl`,`Sitze insgesamt`)) %>%
  ggplot(aes(x=`Jahr der Bundestagswahl`, y=`Sitze insgesamt`,group=1)) +
  geom_line(aes(group=1), color="#009E73") +
  scale_y_continuous(limits = c(300, 800.1),breaks = c(seq(300,800,100)), expand = c(0, 0), labels=c("300" = "300", "400"="400", "500"="500", "600"= "600", "700"= "700","800"="800 Sitze")) +
  scale_x_continuous(breaks = seq(1949, 2017, by = 4), limits=c(1949,2017), labels=c("1949"="1949", "1953"="53", "1957"="57", "1961"="61", "1965"="65", "1969"="69", "1973"="73", "1977"="77", "1981"="81", "1985"="85", "1989"="89", "1993"="93", "1997"="97", "2001"="01", "2005"="05", "2009"="09", "2013"="13", "2017"="17")) +
  labs(title = "Historische Entwicklung der Größe des Bundestages", caption = "Quelle: Bundeswahlleiter") +
  hp_theme() + theme(axis.text= element_text(size=7.5), axis.title.x = element_blank(),plot.title.position = "plot",  axis.title.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size=.2, color="#656565"), axis.line.x=element_line( size=.3, color="black"), legend.position = "right", legend.key = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x =element_line( size=.3, color="black"), plot.caption=element_text(size=5), axis.text.x=element_text(color="black"))
```

![](README_figs/historic_size_graph-1.png)<!-- -->

## 2017 Election Data Preparation

### get data

``` r
# get data
raw_data <- read.csv("https://www.bundeswahlleiter.de/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv", header=F, sep=";", skip=1, stringsAsFactors = F, na.strings="")
# keep raw data
cleaned <- raw_data
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
election_map <- cleaned %>% 
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

actual map

``` r
# all election maps borrows heavily from https://interaktiv.morgenpost.de/analyse-bundestagswahl-2017/data/btw17_analysis.html
# load state borders and shapes of the constituencies
# You can download them here: https://interaktiv.morgenpost.de/analyse-bundestagswahl-2017/data/btw17-shapes.zip
bund_shp <- readOGR("btw17-shapes/bundeslaender_small.shp", "bundeslaender_small", stringsAsFactors=FALSE, encoding="latin1") %>% broom::tidy()
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
krs_shp <- wahlkreise %>% broom::tidy() # broom to tidy shape data to make it work smoothly with the graphic package ggplot2
                                                        
btw_plot <- merge(krs_shp, election_map, by="id", all.y=T) # merge shapes with vote data by id


# actual graph
ggplot(data=btw_plot, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=Partei), show.legend = T) +
  geom_polygon(data=krs_shp, aes(x=long, y=lat, group=group), fill=NA, color="white", size=0.4) +
  scale_fill_manual(values=c("royalblue1", "#32302e", "blue4", "#46962b", "magenta1", "#E3000F")) +
  theme_void() + # remove axes
  coord_map() # apply projection
```

![](README_figs/election_map-1.png)<!-- -->

``` r
# colors: AFD =royalblue1 ; CDU = #32302e ; CSU = blue4 ; GRÜNE = #46962b ; LINKE = magenta1 ; SPD = #E3000F ;
```

## State Data Frames

create a data set for each state (Land=number to differentiate between
states):
