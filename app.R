# Load required packages
library(dplyr)
library(DT)
library(here)
library(readr)
library(shiny)
library(shinydashboard)
library(tidyr)

# Define UI for the app
ui <- dashboardPage(
  title = "Chillbilly Endurance Events",
  dashboardHeader(
    title = tags$a("Chillbilly", titleWidth = "auto", href = "https://crymearivertrailruns.com/", style = "color: white;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Start Here", tabName = "start", icon = icon("list")),
      menuItem("Course Records", tabName = "course_records", icon = icon("trophy")),
      menuItem("Mileage Totals", tabName = "mileage_totals", icon = icon("table")),
      menuItem("All Results", tabName = "all_results", icon = icon("table")),
      menuItem("Register @ RunRace", href = "https://www.runrace.net/findarace.php?id=24013IL", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      ')),
      tags$script(
        '
      $(document).ready(function() {
        $("a[href^=\'http\'],a[href^=\'https\']").not("[href*=\'http://127.0.0.1\'], [href*=\'http://localhost\']").attr("target", "_blank");
      });
      '
      )
    ),
    tags$style(type="text/css",
               ".dataTables_wrapper .dataTables_filter {
        float:left;
    }
    .dataTables_wrapper .dataTables_length {
        float:right;
    }"
    ),
    tabItems(
      tabItem(
        tabName = "start",
        fluidRow(
          box(
            width = 12,
            title = NULL,
            p(HTML("<b>Getting Started:</b><br>
              <ul>
                <li><b>Course Records</b> lists the furthest distances covered for each race duration.</li>
                <li><b>Mileage Totals</b> for all finishers are given, along with their completed distance each year.  Type a runner's name into the search box to see their lifetime results. By default, the table is sorted by total mileage.</li>
                <li><b>All Results</b> is a merged table of all events for all years. Type a runner's name into the search box to see their lifetime results. You can also search by any combination of other fields. Try '2018 8 Hour' to pull up results for that year and event. By default, the table is sorted by year and event.</li>
                <li><b>Register</b> for this year's race so that you don't miss out on the fun!
                <li><b>Course Map</b> below allows you to view the Strava route or download a GPX file to navigate using your watch, phone, or other GPS device.
              </ul><br>
              If you notice any errors, discrepancies, or have suggestions for improvements, please contact <a href='mailto:david.failing@gmail.com'>David Failing</a>.<br><br>
              <b>Course Maps:</b><br>
              <ul>
                <li>Chillbilly Loop - <a href='https://www.strava.com/routes/3084304554337206980' target='_blank'>Strava</a> | <a href='GPX/CMAR-Lake-Loop.gpx' download>GPX</a></li>
              </ul><br>
              <b>Results Sources:</b><br>
              <ul>
                <li>2016 - <a href='http://www.runrace.net/findarace.php?id=16051IL&tab=a4&drsltrl=4062'>RunRace</a></li>
                <li>2017 - <a href='https://www.runrace.net/findarace.php?id=17014IL&tab=a4&drsltrl=6368'>RunRace</a></li>
                <li>2018 - <a href='https://www.runrace.net/findarace.php?id=18027IL&tab=a4&drsltrl=6869'>RunRace</a></li>
                <li>2019 - <a href='https://www.runrace.net/findarace.php?id=19012IL&tab=a4'>RunRace</a></li>
                <li>2020 - <a href='https://www.runrace.net/findarace.php?id=20011IL&tab=a4'>RunRace</a></li>
                <li>2021 - Canceled due to COVID-19</li>
                <li>2022 - <a href='https://www.runrace.net/findarace.php?id=22015IL&tab=a4'>RunRace</a></li>
                <li>2023 - <a href='https://www.runrace.net/findarace.php?id=23014IL&tab=a4&drsltrl=8737'>RunRace</a></li>
                <li>2024 - <a href='https://www.runrace.net/findarace.php?id=24013IL&tab=a4'>RunRace</a></li>
              </ul>"))
          )
        )
      ),
      
      tabItem(tabName = "course_records",
              fluidRow(
                box(
                  width = 12,
                  uiOutput("course_records_ui")
                )
              )
      ),
      tabItem(
        tabName = "mileage_totals",
        fluidRow(
          box(
            width = 12,
            DTOutput("mileage_table")
          )
        )
      ),
      tabItem(
        tabName = "all_results",
        fluidRow(
          box(
            width = 12,
            DTOutput("all_results_table")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  course_records_data <- reactive({
    read_csv(here("data", "course_records.csv")) %>%
      mutate(Rank = paste0(Rank, ".")) %>% 
      unite(Ranked_Name, Rank, First, Last, sep = " ") %>%
      mutate(Record = paste0(Ranked_Name, " (", Age, "), ", Distance, ", ", Year)) %>%
      select(Event, Gender, Record)
  })
  
  output$course_records_ui <- renderUI({
    course_records <- course_records_data()
    events <- unique(course_records$Event)
    genders <- unique(course_records$Gender)
    
    lapply(events, function(event) {
      fluidRow(lapply(genders, function(gender) {
        sub_table_name <- paste0(event, " - ", gender)
        column(
          width = 6,
          course_records %>%
            filter(Event == event, Gender == gender) %>%
            select(Record) %>%
            rename(!!sub_table_name := Record) %>%
            datatable(
              options = list(
                dom = 't',
                pageLength = 10,
                searching = FALSE,
                columnDefs = list(
                  list(targets = 0, visible = FALSE),
                  list(targets = "_all", orderable = FALSE)
                )
              )
            )
        )
      }))
    })
  })
  
  mileage_data <- reactive({
    read_csv(here("data", "mileage_and_time_totals.csv")) %>%
      datatable(options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ))
  })
  
  output$mileage_table <- renderDT({
    mileage_data()
  })
  
  all_results_data <- reactive({
    read_csv(here("data", "all_results.csv")) %>%
      datatable(options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = TRUE,
        columnDefs = list(list(targets = 0, visible = FALSE))
      ))
  })
  
  output$all_results_table <- renderDT({
    all_results_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)