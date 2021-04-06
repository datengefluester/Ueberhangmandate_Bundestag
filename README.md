Ueberhangmandate 2017
================

This is the repository for the blog post published as “TITLE” on “DATE”.
It contains data from

## Folder Layout

-   the blog\_text.Rmd is used to create the blog post
-   the scripts folder contains the r scripts to create the data and
    graphs, which are stored in data and images respectively.
-   the dashboard folder contains the dashboard (the data for which is
    created with the 01\_data\_cleaning.R in the scripts folder).
-   the text for folder contains the text for the blog post

## Scripts

-   01\_data\_cleaning.R
-   01\_parliament\_size\_calculation.R (this is used in
    ‘01\_data\_cleaning.R’)
-   02\_graphs.R
-   02\_themes (this is used in ‘02\_graphs.R’)
-   03\_app (the script can be found in the dashboard folder)

## Packages needed for the project

``` r
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(stringr) # data manipulation
library(rgdal) # for reading in shape file
library(lpSolveAPI) # optimization
library(rvest) # web scrapping
library(stringr) # structure replacement Überhang to Ueberhang etc.
library(knitr) # package for tables
library(janitor) # for better variable names
#--------------------
library(grid) # plots
library(ggplot2) #plots
library(ggpubr) # plots next to each other
library(rgdal) # for maps
library(rgeos) # for maps
library(mapproj) # for maps
library(gganimate) # for gif
library(transformr) # for gif
#--------------------
library(shiny) # for the dashboard
library(shinydashboard) # for the dashboard
library(shinyWidgets) # for the dashboard
```

# Licence

The raw data is published under Data licence Germany – attribution –
version 2.0. <https://www.govdata.de/dl-de/by-2-0>. It can be downloaded
from:
<https://www.bundeswahlleiter.de/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv>.
It can be found under ‘data/raw/raw.csv’. The data for the historic size
of the parliament can be found under:
<https://www.bundeswahlleiter.de/service/glossar/u/ueberhangmandate.html>.
All other data was modified by the author.
