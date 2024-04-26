# Load Packages
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

# Load data in using vroom
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

# Set up US
ui <- fluidPage(
  # Define first row
  fluidRow(
    # Set column width = 3 and select products from list of codes
    column(3,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # Set second column in first row as either rate or count
    column(2, selectInput("y", "Y-axis Metric", c("rate", "count")))
  ),
  
  # Define second row
  fluidRow(
    # First column in second row is for diagnosis table
    column(4, tableOutput("diag")),
    # Second column in second row is for body part table
    column(4, tableOutput("body_part")),
    # Third column in second row is for location table
    column(4, tableOutput("location"))
  ),
  
  # Define third row
  fluidRow(
    # One column for age_sex plot
    column(12, plotOutput("age_sex"))
  ),
  # Define fourth row with narrative UI
  fluidRow(
    # First column action button
    column(2, actionButton("story", "Tell me a story")),
    # Second column is text output of narrative
    column(10, textOutput("narrative"))
  )
)

# Define function to count only the top 5 of each variable and sum the rest into one
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

# Define Server function 
server <- function(input, output, session) {
  # Define reactive variable for injuries to filter based on input from user
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Call output tables
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  # Define reactive summary object to join selected injury data with population data 
  # & calculate rate
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # Call plot (either count or rate based on input$y)
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries") + 
        theme_bw()
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people") + 
        theme_bw()
    }
  }, res = 96)
  
  # Define reactive narrative server to pull narratives from injuries and sample 1
  # every time the story button is clicked or the selected data changes
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  
  # Call text output of narrative
  output$narrative <- renderText(narrative_sample())
}

# Run app
shinyApp(ui, server)