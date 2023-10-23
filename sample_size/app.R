library(shiny)
library(pwr)

ui <- fluidPage(
  titlePanel("Sample Size Calculator (Two-Stage Cluster Sampling)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("icc", "Intra-Class Correlation (ICC):", min = 0, max = 1, value = 0.2, step = 0.05),
      sliderInput("m", "Average Cluster Size (m):", min = 100, max = 5000, value = 300, step = 10),
      sliderInput("pow", "Power:", min = 0.1, max = 0.9, value = 0.8, step = 0.05),
      sliderInput("u", "Covariates:", min = 1, max = 120, value = 5, step = 1),
      selectInput("v", "df of error variance:", choices = c(20, 60, 120, Inf), selected = 20),
      textInput("desired_f", "Desired Effect Size (f):", value = "0.1"),
      textInput("l", "Non-Centrality parameter (λ):", value = "1.1"),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      h4("Sample Size Result:"),
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    icc <- input$icc
    m <- input$m
    pow <- input$pow
    u <- input$u
    v <- input$v
    desired_f <- as.numeric(input$desired_f)
    l <- as.numeric(input$l)
    
    # design effect
    deff <- 1 + (m - 1) * icc
    
    alpha <- 0.05
    
    f <- desired_f
    
    # Adjusted sample size for cluster
    n <- l / f
    
    n_cluster <- round(n * deff)
    
    output$result <- renderText({
      paste("Required sample size:", n_cluster, "with desired effect size:", f)
    })
  })
}

shinyApp(ui, server)
