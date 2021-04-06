# packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# data manipulation
library(tidyverse)
# packages for maps
library(rgdal)
library(rgeos)
library(mapproj)


# color theme
js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #009E73;
}"'

# for editing the dashboard
# dashboard <- read.csv("dashboard/dashboard.csv")

# read in data
dashboard <- read.csv("dashboard.csv")



# shape files
shp_federal <- readOGR("btw17-shapes/bundeslaender_small.shp", 
                    "bundeslaender_small", 
                    stringsAsFactors = FALSE, 
                    encoding = "latin1") %>% 
    broom::tidy()



# max votes needed:
max_votes <- max(dashboard$cummulative_votes, 
                 na.rm = TRUE)
max_votes <- 10000*ceiling(max_votes/10000)

# define colors for parties
Colors <-c("CSU" = "blue4",
           "CDU" = "#32302e",
           "GRÜNE" = "#46962b", 
           "LINKE" = "magenta1",
           "SPD" = "#E3000F",
           "AFD" = "royalblue1",
           "FDP" = "#ffed00") 






# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$style(js),
        tags$style(HTML(".js-irs-0 .irs-single, 
                    .js-irs-0 .irs-bar-edge, 
                    .js-irs-0 .irs-bar {background: #009E73}")),
        tags$style(HTML(".js-irs-1 .irs-single, 
                    .js-irs-1 .irs-bar-edge, 
                    .js-irs-1 .irs-bar {background: #009E73}")),
        tags$style(HTML(".info-box {box-shadow: 0 0px 0px rgba(0,0,0,0);}")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; 
                              -moz-box-shadow: none;
                              box-shadow: none; 
                              border-top-color: #FFF;}'))),
        fluidRow(
            tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", width =  "250px",
                tabPanel("Nur Überhangmandate", 
                         fluidRow(box(tableOutput("ueber_info_box_size")),
                                  box(tableOutput("ueber_info_box_ueber"))),
                         fluidRow(box(uiOutput("ueber_land_selector")),
                                  box(sliderInput(inputId = "v_voters",
                                                  label = "Wähler verschoben:",
                                                  min = 0,
                                                  max = max_votes,
                                                  step = 10000,
                                                  value = 0))),
                         fluidRow(box(plotOutput("ueber_mandate_plot")),
                                  box(tableOutput("ueber_mandate_table")))),
                tabPanel("Alle Direktmandate", 
                         fluidRow(box(tableOutput("direct_info_box_size")),
                                  box(tableOutput("direct_info_box_ueber"))),
                         fluidRow(box(uiOutput("direct_land_selector")),
                                  box(sliderInput(inputId = "v_voters_1",
                                                  label = "Wähler verschoben:",
                                                  min = 0,
                                                  max = max_votes,
                                                  step = 10000,
                                                  value = 0))),
                         fluidRow(box(plotOutput("direct_mandate_plot")),
                                  box(tableOutput("direct_mandate_table"))))
            )
            
        )
    ) 
)



# Define server logic required to draw everything
server <- function(input, output) {
    
    
    # ------ Ueberhangmandate  
    
    # state Selektor    
    output$ueber_land_selector <- renderUI({
        selectInput(inputId="ueber_land",
                    label= "Ansicht:",
                    choices = dashboard %>% 
                        filter(!is.na(cummulative_votes)) %>%
                        group_by(state_name_long) %>% 
                        slice(1) %>% 
                        select(state_name_long) %>%
                        mutate(state_name_long=replace(state_name_long,state_name_long=="Bund", "AAA")) %>% 
                        arrange(state_name_long) %>% 
                        mutate(state_name_long=replace(state_name_long,state_name_long=="AAA", "Bund")) %>% 
                        pull(),
                    selected = "Bund"
        )})
    
    # Überhangmandate Table 1 
    output$ueber_mandate_table <- renderTable({
        dashboard  %>%
            group_by(constituency_name) %>%
            slice(1)  %>%
            filter(!is.na(cummulative_votes),
                   cummulative_votes >= input$v_voters) %>%
            filter(if(input$ueber_land != "Bund") state_name_long == input$ueber_land else TRUE) %>%
            filter(state != "Bund") %>%
            arrange(cummulative_votes) %>%
            ungroup() %>%
            select(constituency_name, mandate_actual, mandate_optimized,
                   votes_halved) %>%
            mutate(votes_halved=as.integer(votes_halved)) %>% 
            rename("Verbleibende Überhangmandate:" = "constituency_name", "Von:" = "mandate_actual", 
                   "Nach:" = "mandate_optimized",
                   "Stimmen benötigt:" = "votes_halved")})
    
    # Info Box: Size Parliament    
    output$ueber_info_box_size <- renderInfoBox({
        ueber_info_box_size <- dashboard %>%
            group_by(constituency_number) %>% 
            slice(1) %>% 
            ungroup() %>% 
            filter(!is.na(cummulative_votes)) %>% 
            arrange(cummulative_votes) %>%
            mutate(rank=as.integer(47-row_number())) %>%
            mutate(final_size=as.integer(final_size)) %>% 
            filter(cummulative_votes <= input$v_voters) %>%
            top_n(cummulative_votes,n=1) %>%
            select(final_size,rank) %>%
            select(final_size) 
        
        ueber_info_box_size <- ueber_info_box_size$final_size[1]
        
        infoBox(
            "Abgeordnete:", paste0(ueber_info_box_size), icon = icon("landmark"),
            color = "green"
        )})
    
    # Info Box: Ueberhang   
    output$ueber_info_box_ueber <- renderInfoBox({
        ueber_info_box_ueber <- dashboard %>%
            group_by(constituency_number) %>% 
            slice(1) %>% 
            ungroup() %>% 
            filter(!is.na(cummulative_votes)) %>% 
            arrange(cummulative_votes) %>%
            mutate(rank=as.integer(47-row_number())) %>%
            mutate(final_size=as.integer(final_size)) %>% 
            filter(cummulative_votes <= input$v_voters) %>%
            top_n(cummulative_votes,n=1) %>%
            select(final_size,rank) %>%
            select(rank) 
        
        ueber_info_box_ueber <- ueber_info_box_ueber$rank[1]
        
        infoBox(
            "Überhangmandate:", paste0(ueber_info_box_ueber), icon = icon("users"),
            color = "green"
        )})
    
    
    # Plot    
    output$ueber_mandate_plot <-  renderPlot({
        dashboard %>%
            mutate(mandate_actual=replace(mandate_actual,
                                          mandate_actual==mandate_optimized,
                                          NA)) %>% 
            mutate(mandate_actual=replace(mandate_actual, 
                                          cummulative_votes <= input$v_voters,
                                          NA)) %>%
            filter(if(input$ueber_land != "Bund") 
                state_name_long == input$ueber_land 
                else TRUE) %>%
            ggplot(aes(x=long, y=lat, group=group))+
            geom_polygon(aes(fill=mandate_actual), show.legend = T) +
            geom_polygon(
                aes(x=long, y=lat, group=group), 
                fill=NA, color="#656565", size=0.1) +
            {if(input$ueber_land == "Bund")
                geom_polygon(data=shp_federal, 
                             aes(x=long, y=lat, group=group), 
                             fill=NA, color="black", size=0.2)} +
            scale_fill_manual(values= Colors,
                              name="Neues Direktmandat",
                              na.translate = F) +
            coord_map() + 
            theme_void() +
            theme(legend.position="none")})  
    
    
    
    #------ Direktmandate  
    
    # state Selektor  
    output$direct_land_selector <- renderUI({
        selectInput(inputId="direct_land",
                    label= "Ansicht:",
                    choices = dashboard %>% 
                        group_by(state_name_long) %>% 
                        slice(1) %>% 
                        select(state_name_long) %>%
                        mutate(state_name_long=replace(state_name_long,state_name_long=="Bund", "AAA")) %>% 
                        arrange(state_name_long) %>% 
                        mutate(state_name_long=replace(state_name_long,state_name_long=="AAA", "Bund")) %>% 
                        pull(),
                    selected = "Bund",
        )})
    
    
    # Info Box: Size Parliament    
    output$direct_info_box_size <- renderInfoBox({
        direct_info_box_size <- dashboard %>%
            group_by(constituency_number) %>% 
            slice(1) %>% 
            ungroup() %>% 
            filter(!is.na(cummulative_votes)) %>% 
            arrange(cummulative_votes) %>%
            mutate(rank=as.integer(47-row_number())) %>%
            mutate(final_size=as.integer(final_size)) %>% 
            filter(cummulative_votes <= input$v_voters_1) %>%
            top_n(cummulative_votes,n=1) %>%
            select(final_size,rank) %>%
            select(final_size) 
        
        direct_info_box_size <- direct_info_box_size$final_size[1]
        
        infoBox(
            "Abgeordnete:", paste0(direct_info_box_size), icon = icon("landmark"),
            color = "green"
        )})
    
    # Info Box: Ueberhang   
    output$direct_info_box_ueber <- renderInfoBox({
        direct_info_box_ueber <- dashboard %>%
            group_by(constituency_number) %>% 
            slice(1) %>% 
            ungroup() %>% 
            filter(!is.na(cummulative_votes)) %>% 
            arrange(cummulative_votes) %>%
            mutate(rank=as.integer(47-row_number())) %>%
            mutate(final_size=as.integer(final_size)) %>% 
            filter(cummulative_votes <= input$v_voters_1) %>%
            top_n(cummulative_votes,n=1) %>%
            select(final_size,rank) %>%
            select(rank) 
        
        direct_info_box_ueber <- direct_info_box_ueber$rank[1]
        
        infoBox(
            "Überhangmandate", paste0(direct_info_box_ueber), icon = icon("users"),
            color = "green"
        )})
    
    
    # Ueberhangmandate Table
    output$direct_mandate_table <- renderTable({
        dashboard  %>%
            group_by(constituency_name) %>%
            slice(1)  %>%
            filter(!is.na(cummulative_votes),
                   cummulative_votes >= input$v_voters_1) %>%
            filter(if(input$direct_land != "Bund") state_name_long == input$direct_land else TRUE) %>%
            filter(state != "Bund") %>%
            arrange(cummulative_votes) %>%
            ungroup() %>%
            select(constituency_name, mandate_actual, mandate_optimized,
                   votes_halved) %>%
            mutate(votes_halved=as.integer(votes_halved)) %>% 
            rename("Verbleibende Überhangmandate:" = "constituency_name", "Von:" = "mandate_actual", 
                   "Nach:" = "mandate_optimized",
                   "Stimmen benötigt:" = "votes_halved")})
    
    # Plot    
    output$direct_mandate_plot <-  renderPlot({
        dashboard %>%
            mutate(cummulative_votes=replace(cummulative_votes,
                                             is.na(cummulative_votes),
                                             10000000000)) %>%  
            mutate(mandate_actual=replace(mandate_actual, 
                                          cummulative_votes <= input$v_voters_1,
                                          mandate_optimized[which(cummulative_votes <= input$v_voters_1)])) %>% 
            filter(if(input$direct_land != "Bund") state_name_long == input$direct_land else TRUE) %>%
            ggplot(aes(x=long, y=lat, group=group))+
            geom_polygon(aes(fill=mandate_actual), show.legend = T) +
            geom_polygon(
                aes(x=long, y=lat, group=group), 
                fill=NA, color="#656565", size=0.1) +
            {if(input$direct_land == "Bund")
                geom_polygon(data=shp_federal, 
                             aes(x=long, y=lat, group=group), 
                             fill=NA, color="black", size=0.2)} +
            scale_fill_manual(values= Colors,
                              name="Neues Direktmandat",
                              na.translate = F) +
            coord_map() + 
            theme_void() +
            theme(legend.position="none")})   
    
    
    # Dynamic Size
    
    output$dynamic_size <- renderTable({
        
        
        
    })  
}





# Run the application 
shinyApp(ui = ui, server = server)