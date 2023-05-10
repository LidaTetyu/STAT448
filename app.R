# Load necessary packages
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(leaflet)

install.packages('rsconnect')

# Set the API endpoint and parameters
api_url <- "https://api.data.gov/ed/collegescorecard/v1/schools"
params <- list(api_key = "nMnHF6OvamrpBnFb1ZXX2Op8rWcmeQP6bVZWB4tZ",
               fields = "school.zip,latest.cost.attendance,school.name,latest.student.size,latest.cost.booksupply,latest.cost.roomboard.oncampus,latest.aid.median_debt.completers.overall,latest.student.demographics.avg_family_income,latest.student.demographics.race_ethnicity,school.state,latest.school.degrees_awarded.highest,earnings.10_yrs_after.median",
               per_page = 100,
               school.state = 'IL',
               latest.school.degrees_awarded.highest = "3,4")

# Send GET request to API
response <- GET(api_url, query = params)

data = content(response, "text")%>% fromJSON(flatten = TRUE)

#Filter api data
collegestats = data$results %>%
  select(c(school.zip,latest.cost.attendance.academic_year,latest.cost.attendance.program_year,school.name,latest.cost.booksupply,latest.cost.roomboard.oncampus,
           latest.student.demographics.avg_family_income,latest.student.size,latest.aid.median_debt.completers.overall,
           latest.student.demographics.race_ethnicity.women,latest.student.demographics.race_ethnicity.white,latest.student.demographics.race_ethnicity.black))%>%
  rename(`School Name` = school.name, `Percentage Women` = latest.student.demographics.race_ethnicity.women,`Student Population` = latest.student.size,
         `Percentage White` = latest.student.demographics.race_ethnicity.white, `Percentage Black` = latest.student.demographics.race_ethnicity.black,
         `Family Income` = latest.student.demographics.avg_family_income,RoomBoardCost = latest.cost.roomboard.oncampus, BookSuppliesCost = latest.cost.booksupply,
         AvgAttendanceCost1 = latest.cost.attendance.program_year, AvgAttendanceCost2 = latest.cost.attendance.academic_year, `Median Debt` = latest.aid.median_debt.completers.overall)%>%
  mutate(AvgAttendanceCost1 = replace_na(AvgAttendanceCost1, 0), AvgAttendanceCost2 = replace_na(AvgAttendanceCost2, 0))%>%
  mutate(`Avg Attendance Cost Per Year` = round(as.numeric(AvgAttendanceCost1) + as.numeric(AvgAttendanceCost2) +
                                                  as.numeric(replace_na(BookSuppliesCost,0))+ as.numeric(replace_na(RoomBoardCost,0)),0), .keep = "unused")%>%
  filter(`Avg Attendance Cost Per Year` > 0, !is.na(`Median Debt`), str_detect(`School Name`,"University"))

locations <- data.frame(
  university = c("Aurora University", "Bradley University", "Chicago State University", "University of Chicago",
                 "Concordia University-Chicago", "DePaul University", "East-West University", "Eastern Illinois University",
                 "Elmhurst University", "Governors State University", "Greenville University", "University of Illinois Chicago",
                 "Benedictine University", "University of Illinois Urbana-Champaign", "Illinois Wesleyan University",
                 "Illinois State University", "Judson University", "Lewis University", "Lincoln Christian University",
                 "Loyola University Chicago", "McKendree University", "Millikin University", "National Louis University",
                 "North Park University", "Northern Illinois University", "Northwestern University", "Northeastern Illinois University",
                 "Olivet Nazarene University", "Quincy University", "Rockford University", "Roosevelt University", "Dominican University",
                 "University of St Francis", "Saint Xavier University", "University of Illinois Springfield", "Southern Illinois University-Carbondale",
                 "Southern Illinois University-Edwardsville", "Trinity International University-Illinois", "Western Illinois University",
                 "Rasmussen University-Illinois", "Chamberlain University-Illinois", "DeVry University-Illinois"),
  lats <- c(41.7543, 40.6964, 41.7189, 41.7897, 41.9072, 41.9243, 41.8781, 39.4828, 41.8972,
            41.4379, 38.8909, 41.8723, 41.7645, 40.1106, 40.5094, 40.5106, 42.1008, 41.6029,
            40.1427, 42.0017, 38.6015, 39.8434, 41.9728, 41.9809, 41.9317, 42.0551, 41.98207,
            41.1406, 39.9346, 42.2691, 41.8767, 41.9074, 41.5358, 41.7086, 39.7901, 37.7170,
            38.7887, 42.2447, 40.4766, 41.8009, 41.96757,41.8908),
  lng <- c(-88.3420,-89.6169,-87.6081,-87.5997,-87.8340,-87.6550,-87.6298, -88.1754,-87.9434,
           -87.7539,-89.3967,-87.6511,-88.1461,-88.2073,-88.9896,-88.9913,-88.3219,-88.0817,
           -89.3728,-87.6616,-89.8030,-88.9478,-87.6579,-87.7200,-88.7671,-87.6763,-87.71744,
           -87.8755,-91.3935,-89.0251,-87.6243,-87.8218,-88.0835,-87.7122,-89.6446,-89.2200,
           -89.9842,-87.9606,-90.6815,-88.1527,-87.8722,-87.6259))

ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = HTML("<h4><center><b>Illinois University Finder</b>&nbsp;&nbsp;<i class='fa fa-school-circle-check'></i></center></h4>"),
  tabPanel(title = HTML("<h4><center>About&nbsp;&nbsp;<i class='fa fa-graduation-cap'></i></center></h4>"),
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             dashboardBody(
               style = 'background: white',
               fluidRow(
                 column(width = 12,
                        wellPanel(style = "background: white",
                                  HTML("<h4><font size='+2'><b>Illinois University Finder</b> is a project Lida Tetyusheva and Himanshu Kumat created for their Stat440 class. The result is
                    a shiny application that helps the user narrow down which university in Illinois they would like to attend. In the <i>'Narrow it Down'</i> tab, there is a table where the user can
                    narrow down the Universities in a table by filtering certain variables. In the <i>'Compare Two Universities'</i> tab, the user can choose two Universities and one variable to compare
                    between them.</font></h4>")
                        ))
               ),
               fluidRow(style = 'border: 0px',
                        column(width = 12,
                               wellPanel(style = 'background: white',
                                         HTML("<h2><center><b>Why Go to an Illinois University?</b></center></h2>")
                               ))
               ),
               fluidRow(
                 infoBox(title = NULL,value = '$22,263', subtitle = "a median debt lower than the national average", icon = icon("credit-card"), color = 'maroon', fill = T),
                 infoBox(title = NULL,value = '$50,758',subtitle = "in average cost", icon = icon("money-bill"),color = 'navy', fill = T),
                 infoBox(title = NULL,value = 11, subtitle = "schools ranked among best in the world", icon = icon("globe"),color = 'olive', fill = T)
               ),
               fluidRow(
                 infoBox(title = NULL,value = "Illinois schools outperformed their out-of-state competitors",subtitle = "According to the IBHE", icon = icon("thumbs-up"), color = 'olive', fill = T),
                 infoBox(title = NULL,value = "Illinois schools outperform other states in diversity",subtitle = "According to the IBHE", icon = icon("people-group"),color = 'maroon', fill = T),
                 infoBox(title = NULL,value = "Illinois schools have the best completion rates in the nation", subtitle = "According to the IBHE", icon = icon("graduation-cap"),color = 'navy', fill = T)
               )))),
  tabPanel(title = HTML("<h4><center>Narrow it Down&nbsp;&nbsp;<i class='fa fa-filter'></i></center></h4>"),
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             dashboardBody(
               fluidRow(
                 box(title = "Filters",
                     width = 4,
                     style = "background: white",
                     chooseSliderSkin("Flat"),
                     setSliderColor(c("#d81b60","#001f3f","#3d9970"),c(1,2,3)),
                     sliderInput('cost',"Cost of Attendance",min = 20000, max = 102000, value = 61000),
                     sliderInput("NumStu","Number of Students", min = 0, max = 34000, value = 17000),
                     sliderInput("debt","Median Debt Cost", min = 0, max = 32000, value = 16000),
                     circleButton("button1",icon = icon("filter"), size = "lg")
                 ),
                 box(width = 8,dataTableOutput("table"))
               )))),
  tabPanel(title = HTML("<h4><center>Compare Two Universities&nbsp;&nbsp;<i class='fa fa-scale-balanced'></i></center></h4>"),
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             dashboardBody(
               fluidRow(
                 box(width = 4,title = "Filters",
                     style = 'background: white',
                     selectInput("UniA","University A", choices = collegestats$`School Name`),
                     selectInput("UniB", "University B", choices = collegestats$`School Name`),
                     selectInput("Var1", "Variable to Compare", choices = colnames(select(collegestats, -c(school.zip,`School Name`)))),
                     circleButton("button2", icon = icon("scale-balanced"),size = 'lg')
                 ),
                 box(width = 8, plotOutput("graph1"))
               )
             ))),
  tabPanel(title = HTML("<h4><center>University Location&nbsp;&nbsp;<i class='fa fa-map'></i></center></h4>"),
           dashboardPage(
             dashboardHeader(disable = TRUE),
             dashboardSidebar(disable = TRUE),
             dashboardBody(
               box(width = 4,title = 'Filters',
                   selectInput("university", "Select a University", choices = unique(locations$university))),
               box(width = 8, leafletOutput("map"))
             ))))

server <- function(input, output) {
  output$map <- renderLeaflet({
    university <- input$university
    # Get the location and county of the selected university
    location <- locations[locations$university == university, ]
    # Create a leaflet map centered on the location of the selected university
    leaflet() %>%
      addTiles() %>%
      setView(location$lng, location$lat, zoom = 8) %>%
      # Add a marker at the location of the selected university
      addMarkers(lng = location$lng, lat = location$lat)
  })
  graph1 = eventReactive(input$button2,{
    collegestats %>%
      filter(`School Name` == input$UniA|`School Name` == input$UniB)%>%
      ggplot(aes(x = `School Name`, y = .data[[input$Var1]], fill = `School Name`))+
      geom_bar(position = "dodge",stat = 'identity', width = .5)+
      geom_text(aes(label=.data[[input$Var1]]), position=position_dodge(width=0.9), vjust=-0.25)+
      scale_fill_manual(values=c("#d81b60", "#3d9970"))+
      theme_minimal()+
      xlab("School Name")+
      ggtitle(paste("Comparison between", input$UniA, "and",input$UniB))+
      theme(axis.text.x = element_text(size = 15),axis.title = element_text(size = 13),
            plot.title = element_text(size = 17,hjust = .5), legend.position = 'none')
  })
  output$graph1 = renderPlot(
    graph1()
  )
  table = eventReactive(input$button1,{
    collegestats %>%
      select(-school.zip)%>%
      filter(`Avg Attendance Cost Per Year` <= input$cost)%>%
      filter(`Student Population` <= input$NumStu)%>%
      filter(`Median Debt` <= input$debt)%>%
      arrange(desc(`Avg Attendance Cost Per Year`))
  })
  output$table = renderDataTable(
    table()
  )
}

# Run the application
shinyApp(ui = ui, server = server)
