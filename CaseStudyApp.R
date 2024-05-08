# Load Packages
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)
library(bslib)

# Load data in using vroom
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

# Set up US
ui <- page_fillable(
  # Application Title
 title = "Emergency Room Case Study Application II",
 # Top card with plot
 card(
   card_header("Number of Injuries by Sex & Age", class = "bg-dark"),
   plotOutput("age_sex")
   ),
 # Second row of cards
 layout_columns(
     card(
       card_header("Parameters", class = "bg-primary"),
       selectInput("code", "Product of Interest",
                   choices = setNames(products$prod_code, products$title), width = "100%"),
       selectInput("y", "Y-axis Metric", c("rate", "count")),
       sliderInput("nrow", "Number of Rows to Display", min = 3, max = 10, value = 5)
       ),
     card(
       card_header("Diagnoses", class = "bg-dark"),
       tableOutput("diag")
       ),
     card(
       card_header("Body Part", class = "bg-dark"),
       tableOutput("body_part")
       ),
     card(
       card_header("Location", class = "bg-dark"),
       tableOutput("location")
       )
     ),
 # Third row of cards
 layout_columns(
   card(
     card_header("Example Injury Report:", class = "bg-dark"),
     textOutput("narrative")
     ),
   card(
     actionButton("backone", "Back", class = "bg-primary")
     ),
   card(
     actionButton("nextone", "Next", class = "bg-primary")
     )
   )
 )

  # Define function to count only the top n of each variable and sum the rest into one based on user input
  count_top <- function(df, var, n) {
    df %>%
      mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n)) %>%
      group_by({{ var }}) %>%
      summarise(n = as.integer(sum(weight)))
  }
  
  # Define Server function 
  server <- function(input, output, session) {
    # Define reactive variable for injuries to filter based on input from user
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    
    #Create reactive variable for number of rows
    numrow <- reactive({input$nrow})
    
    # Call output tables
    output$diag <- renderTable(count_top(selected(), diag, numrow()), width = "100%")
    output$body_part <- renderTable(count_top(selected(), body_part, numrow()), width = "100%")
    output$location <- renderTable(count_top(selected(), location, numrow()), width = "100%")
    
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
          theme_bw() + 
          theme(panel.grid = element_blank(),
                legend.position = c(0.9, 0.75)) + 
          scale_x_continuous(limits = c(0, 80))
      } else {
        summary() %>%
          ggplot(aes(age, rate, colour = sex)) +
          geom_line(na.rm = TRUE) +
          labs(y = "Injuries per 10,000 people") + 
          theme_bw() + 
          theme(panel.grid = element_blank(),
                legend.position = c(0.9, 0.75)) + 
          scale_x_continuous(limits = c(0, 80))
      }
    }, res = 96)
    
    # Define reactive narrative server to pull narratives from injuries and print them in order
    # Going back and forth as user pleases
    place <- reactive({input$nextone - input$backone})
    injleng <- reactive({length(selected())})
   
    narrative_sample <- eventReactive(list(selected(), place()), {
      data <- selected()
      index <- place()
      
      if (index == 0) {
        data$narrative[injleng()]
      } else if (index < 0) {
        data$narrative[injleng() + index]
      } else {
        data$narrative[index]
      }
    })
    
    # Call text output of narrative
    output$narrative <- renderText(narrative_sample())
  }
  
  # Run app
  shinyApp(ui, server)