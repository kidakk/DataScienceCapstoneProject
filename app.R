#libraries
library(shiny)
library(rgdal)
library(dygraphs)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(reshape2)
library(readxl)
library(readr)
library(janitor)
library(DT)
library(tidyverse)    
library(lubridate)
library(bslib)
library(usmap)
library(broom)
library(htmltools)
library(plotly)
library(formattable)

#data loading
county<-st_read("tl_2021_us_county", quiet = TRUE) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')
analytic_data2019 <- read_csv("analytic_data2019.csv", 
                                                   col_types = cols(`State FIPS Code` = col_character(), 
                                                                    `County FIPS Code` = col_character(), 
                                                                    `5-digit FIPS Code` = col_character()))
Total_state_2019 <- 
  read_excel("Total_state_2019.xlsx") 

state<-st_read("tl_2019_us_state", quiet = TRUE) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

percent <- read_csv('percent.csv')

#data cleaning
state_map<-
  state %>%
  mutate(State=NAME) %>%
  relocate(State, .before=NAME) %>%
  select(-NAME) %>%
  left_join(Total_state_2019, by="State") %>% 
  select("State", "INTPTLAT", "INTPTLON", "FIPS", "Total population", "Hispanic or Latino (of any race)", 
         "White alone", "Black or African American alone", "American Indian and Alaska Native alone", "Asian alone",
         "Native Hawaiian and Other Pacific Islander alone", "High school graduate or higher", 
         "Unemployment_rate", "Per capita income (dollars)", "Civilian noninstitutionalized population",
         "No health insurance coverage") %>% 
  mutate(Hispanic_p=(`Hispanic or Latino (of any race)`/`Total population`),
         White_p=(`White alone`/`Total population`),
         Black_p=(`Black or African American alone`/`Total population`),
         Asian_p=(`Asian alone`/`Total population`),
         Native_p=(`American Indian and Alaska Native alone`/`Total population`),
         Hawaiian_p=(`Native Hawaiian and Other Pacific Islander alone`/`Total population`), 
         Highschool_rate=(`High school graduate or higher`/`Total population`), 
         Uninsured_rate=(`No health insurance coverage`/`Civilian noninstitutionalized population`))


analytics<-
  analytic_data2019 %>% 
  clean_names()
analytics$x5_digit_fips_code<-str_pad(analytics$x5_digit_fips_code, 5, pad = "0")
analytics_2019<-
  analytics %>% 
  filter(!name%in%c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                 "Iowa", "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana", "Nebraska",
                 "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")) %>% 
  select(3, 5, matches("_raw_value"))
county_map_data<-
  county %>% 
  left_join(analytics_2019, by=c("GEOID"="x5_digit_fips_code")) %>% 
  left_join(fips_info(), by=c("STATEFP"="fips")) %>% 
  mutate(county = paste(NAME, full, sep = ", ")) %>% 
  #select(-c("countyns", "name", "abbr", "full")) %>% 
  relocate(county, .before = NAMELSAD) %>% 
  rename_at(vars(matches("_raw_value")), ~ str_remove(., "_raw_value")) %>% 
  clean_names() %>% 
  select(-c("countyns", "name", "abbr", "full", "namelsad"))
#county_map_data


#percent <- read_csv('percent.csv')

Total_state_2019 <- Total_state_2019 %>%
  mutate(Uninsured_rate = `No health insurance coverage`/`Civilian noninstitutionalized population`)

Total_state_2019$State <- factor(Total_state_2019$State, levels = unique(Total_state_2019$State)[order(Total_state_2019$Uninsured_rate, decreasing = TRUE)])

Total_state_2019$State <- factor(Total_state_2019$State, levels = unique(Total_state_2019$State)[order(Total_state_2019$Unemployment_rate, decreasing = TRUE)])

insurance<-
  Total_state_2019 %>% 
  group_by(State) %>% 
  #mutate(`health insurance`=`With health insurance #coverage`/`Civilian noninstitutionalized population`,
  #      `private health insurance`=`With private health insurance`/`Civilian noninstitutionalized population`,
  #       `public coverage`=`With public coverage`/`Civilian noninstitutionalized population`,
  #      `No health insurance`=`No health insurance coverage`/`Civilian noninstitutionalized population`) %>% 
  select(`With health insurance coverage`,`With private health insurance`,`With public coverage`,`No health insurance coverage`) %>% 
  pivot_longer(2:5, names_to="Insurance", values_to="Population")

Total_state<-
  Total_state_2019 %>%
  # filter(State%in%c("Minnesota", "Wisconsin") )%>%
  group_by(State) %>%
  select(6:18) %>%
  pivot_longer(2:14, names_to = "Age", values_to = "population")

gender <- Total_state_2019 %>%
  group_by(State) %>%
  select(4:5) %>%
  pivot_longer(2:3, names_to = "Gender", values_to = "population")

#library(formattable)
race<-
  Total_state_2019 %>%
  group_by(State) %>%
  mutate(Hispanic = percent(`Hispanic or Latino (of any race)`/`Total population`),
         White = percent(`White alone`/`Total population`),
         Black = percent(`Black or African American alone`/`Total population`),
         Native = percent(`American Indian and Alaska Native alone`/`Total population`),
         Asian = percent(`Asian alone`/`Total population`),
         Hawaiian=percent(`Native Hawaiian and Other Pacific Islander alone`/`Total population`),
         Other = percent(`Total population`-`Hispanic or Latino (of any race)`-`White alone`-`Black or African American alone`-`American Indian and Alaska Native alone`-`Asian alone`-`Native Hawaiian and Other Pacific Islander alone`)/`Total population`) %>%
  select(Hispanic:Other) %>%
  pivot_longer(2:8, names_to="Race", values_to="Percentage")

industry<-
  Total_state_2019 %>%
  group_by(State) %>%
  select(29:41) %>%
  pivot_longer(2:14, names_to = "Industry", values_to = "population")


state_visualization <- function(variable_name, input_state){
  if (variable_name == 'Uninsured_rate'){
    fig <- plot_ly(Total_state_2019, x = ~Total_state_2019$State, y = ~Total_state_2019$Uninsured_rate, type = 'bar', name = 'Uninsured rate', marker = list(color = 'rgb(49,130,189)'))
    
    fig <- fig %>% layout(title = 'Uninsured rate for each state',
                          xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
  }
  
  if (variable_name == 'Unemployment_rate'){
    fig <- plot_ly(Total_state_2019, x = ~Total_state_2019$State, y = ~Total_state_2019$Unemployment_rate, type = 'bar', name = 'Unemployment rate', marker = list(color = 'rgb(49,130,189)'))
    
    fig <- fig %>% layout(title = 'Unemployment rate for each state',
                          xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
  }
  
  if (variable_name == 'Insurance'){
    insurance <- insurance %>% 
      filter(State == input_state)
    
    insurance$Insurance <- factor(insurance$Insurance, levels = unique(insurance$Insurance)[order(insurance$Population, decreasing = TRUE)])
    
    ramp2 <- colorRamp(c("deepskyblue4", "white"))
    ramp.list2 <- rgb( ramp2(seq(0, 1, length = 15)), max = 255)
    
    fig <- plot_ly(insurance, x = ~Insurance, y = ~Population, type = 'bar', color = ~Insurance) %>% 
      layout(list(title = 'Cost'), barmode = 'group') %>%
      layout(colorway = ramp.list2) %>%
      config(displayModeBar = FALSE)
    
    fig <- fig %>% layout(title = 'Insurance Distribution',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if (variable_name == 'Age'){
    Total_state <- Total_state %>% 
      filter(State == input_state)
    
    Total_state$Age <- factor(Total_state$Age, levels = c("Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 74 years", "75 to 84 years", "85 years and over"))
    
    fig <- plot_ly(Total_state, x = ~Total_state$Age, y = ~Total_state$population, type = 'scatter', mode = 'lines')
    
    fig <- fig %>% layout(title = 'Age Distribution',
                          xaxis = list(title = 'Age Group'),
                          yaxis = list(title = 'Population'))
  }
  
  if (variable_name == 'Gender'){
    
    gender <- gender %>% 
      filter(State == input_state)
    
    fig <- plot_ly(gender, labels = ~gender$Gender, values = ~population, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(population),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)
    
    fig <- fig %>% layout(title = 'Gender Distribution',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  
  if (variable_name == 'Race'){
    race <- race %>% 
      filter(State == input_state)
    
    race$Race <- factor(race$Race, levels = unique(race$Race)[order(race$Percentage, decreasing = TRUE)])
    
    
    fig <- plot_ly(race, x = ~race$Race, y = ~Percentage, type = 'bar', color = ~Race) 
    
    
    fig <- fig %>% layout(xaxis = list(title = "Race Distribution", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
  }
  
  if (variable_name == 'Industry'){
    industry <- industry %>% 
      filter(State == input_state)

    
    fig <- plot_ly(industry, x = ~industry$Industry, y = ~population, type = 'bar', color = ~Industry) %>%
      layout(title = list(text = "Industry Distribution", barmode = 'group')) %>%
      layout() %>%
      config(displayModeBar = FALSE)
    
    fig <- fig %>% layout(title = 'Industry Distribution',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
  return(fig)
}

#Shiny ui part

ui <- fluidPage(
   tabsetPanel(
  #   tabPanel("State Level Interactive Map", fluid = TRUE,
  #            sidebarLayout(
  #              sidebarPanel(selectInput("variableselected", "Select variable", choices = c("total_population", "highschool_rate", "uninsured_rate", "per_capita_income_(dollars)"), selected = "")),
  #              mainPanel(
  #                fluidRow(
  #                  column(
  #                    width = 10,
  #                    leafletOutput(outputId = "map_insurance", width="100%")
  #                  )
  #                ),
  #                DT::dataTableOutput("mytable_insurance")
  #              )
  #              
  #            )
  #   ),
    tabPanel("Demographic Data Visualization", fluid = TRUE,
     headerPanel("Data Visualization"),
     sidebarPanel(
       p("Select the inputs for State"),
       selectInput(inputId = "state", label = "state", 
                   multiple = FALSE, choices = Total_state_2019$State),
       p("Select the inputs for the exploration variable"),
       selectInput(inputId = "ExpVar", label = "exploration variable",
                   multiple = FALSE, choices = c('Uninsured_rate', 'Unemployment_rate', 'Insurance', 'Age', 'Gender', 'Race', 'Industry'))
     ),
     mainPanel(
       plotlyOutput("ExpPlot")
       #plotOutput("ExpPlot")
     )),
    tabPanel("Insurance Regression Analysis", fluid = TRUE,
             headerPanel("Regression Analysis"),
             sidebarPanel(
               p("Select the inputs for the Dependent Variable"),
               selectInput(inputId = "DepVar", label = "Dependent Variables", 
                           multiple = FALSE, choices = names(percent[41:42])),
               p("Select the inputs for the Independent Variable"),
               selectInput(inputId = "IndVar", label = "Independent Variables",
                           multiple = FALSE, choices = names(percent[2:40]))
             ),
             mainPanel(
               tableOutput(outputId = "RegSum"),
               plotOutput("RegPlot")
             )),
    tabPanel("County Level Interactive Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("variableselected", "Select variable", 
                                        choices = c("premature_death", 
                                                    "poor_or_fair_health", 
                                                    "poor_physical_health_days", 
                                                    "poor_mental_health_days",
                                                    "low_birthweight",
                                                    "adult_smoking",
                                                    "adult_obesity",
                                                    "physical_inactivity",
                                                    "flu_vaccinations",
                                                    "life_expectancy",
                                                    "premature_age_adjusted_mortality",
                                                    "child_mortality",
                                                    "infant_mortality",
                                                    "diabetes_prevalence"), 
                                        selected = "")),
               mainPanel(
                 fluidRow(
                   column(
                     width = 10,
                     leafletOutput(outputId = "map"),
                     plotlyOutput(outputId = "g")
                     #plotlyOutput(outputId = "f", width = "100%")
                   )
               
             )
    )))
  )
)


server<-function(input, output){
  
  output$map <- renderLeaflet({
   
    pos <- match(input$variableselected, names(county_map_data))
    # find the column number of the colorvariable
    
    pal <- colorNumeric("viridis", domain = county_map_data %>% pull(pos))
    #domain = state_map %>% pull(.data[[input$variableselected]]))
    #domain = map$variableselected) 
    popup_sb <- paste0("<b><h3>", county_map_data$county, sep = ": ", "</h3></b>", as.character(county_map_data %>% pull(pos)))
    # how to collect the info someone look on the map
    leaflet(county_map_data) %>% 
      addTiles() %>% 
      addPolygons(
        #skips drawing the borders:
        stroke = FALSE, 
        #fills according to variable of hex colors:
        fillColor = ~pal(county_map_data %>% pull(pos)), #~pal(total_population),
        #~pal(.data[[input$variableselected]]), 
        #changes transparency, like alpha in ggplot
        fillOpacity = 0.7, 
        #how much to simplify the plot when zooming:
        smoothFactor = 0.5, 
        #changes what happens to the shape when we mouse over it
        highlight = highlightOptions(weight = 5, 
                                     color = "black",
                                     fillOpacity = 0.9,
                                     bringToFront = FALSE),
        popup=~popup_sb) %>% 
      leaflet::addLegend(
        pal = pal, values = ~county_map_data %>% pull(pos),
        opacity = 0.7, title = NULL
      ) 
    
  })
  
  output$g<-renderPlotly({
    g <- ggplot(state_map) +
      geom_sf(aes(map_id=State, fill=Uninsured_rate)) +
      scale_fill_distiller("Uninsured rate", palette="Spectral") +
      ggtitle("Uninsured rate by state")
    ggplotly(g)
  })
  
  #output$f<-renderPlotly({
    #f<-ggplot(state_map)+
      #geom_sf(aes(map_id=state, fill=Unemployment_rate)) +
      #scale_fill_distiller("Unemployment rate", palette="Spectral") +
      #ggtitle("Uninsured rate by state")
    #ggplotly(f)
 # })
  

lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = percent)})

output$RegPlot <-  renderPlot(
  augment(lm1()) %>% 
    ggplot() +
    geom_jitter(aes(x=.data[[input$IndVar]], y=.data[[input$DepVar]])) +
    geom_line(aes(x=.data[[input$IndVar]], y=.fitted))
)
output$RegSum <- renderTable({tidy(lm1())})

output$ExpPlot <- renderPlotly(
  {state_visualization(input$ExpVar, input$state)}
)
}
  
  

shinyApp(ui=ui, server = server)




