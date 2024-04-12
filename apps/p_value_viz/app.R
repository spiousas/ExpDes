#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, tidyverse)
theme_set(theme_bw(base_size = 18)) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("La danza de los p valores"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu",
                  "Mu poblacional:",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .1),
      sliderInput("sigma",
                  "Sigma poblacional:",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = .1),
      sliderInput("n",
                  "Tamaño de muestra:",
                  min = 5,
                  max = 200,
                  value = 20,
                  step = 1),
      tags$head(tags$script(src = "message-handler.js")),
      actionButton("do", "Nueva muestra")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(style = "height:300px",
        column(style = "height:300px", width = 6,
        h3("Distribución de X con la muestra x y su media"),
        plotOutput("distPlot")),
        column(style = "height:300px", width = 6,
        h3("Ubicación de la media en la dist. de las medias y p"),
        plotOutput("meandistPlot"))),
      fluidRow(
        textOutput("p_value")),
      fluidRow(
        h3("Histograma acumulado de los p valores"),
        plotOutput("histP"))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ps <- reactiveValues(
    p = c()
  )
  
  sample <- eventReactive(input$do, {tibble(x = rnorm(n = input$n, mean = input$mu, sd = input$sigma))})
  
  normalDistribution <- data.frame(
    x = seq(-6,6, by = 0.01),
    y = dnorm(seq(-6,6, by = 0.01))
  )
  
  low <- reactive({ -abs(mean(sample()$x))/sqrt(input$sigma^2/input$n) })
  high <- reactive({ abs(mean(sample()$x))/sqrt(input$sigma^2/input$n) })
  criticalValues <- reactive({ c(low(), high()) })
  
  shadeNormalTwoTailedLeft <- reactive({rbind(c(criticalValues()[1],0), subset(normalDistribution, x < criticalValues()[1]))})
  shadeNormalTwoTailedRight <- reactive({rbind(c(criticalValues()[2],0), subset(normalDistribution, x > criticalValues()[2]), c(3,0))})
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    X <- sample()
    
    ggplot(data = X) +
      geom_point(aes(x = x, y = 0), color = "steelblue", size = 2) +
      geom_vline(xintercept = X$x, alpha = .3, color = "steelblue", size = 2) +
      geom_point(color = "red", x = mean(X$x), y=0, size = 5) +
      stat_function(fun = dnorm, args = list(mean = input$mu, sd = input$sigma), 
                    color = "black", linewidth = 1) +
      scale_x_continuous(limits = c(-5,5)) +
      scale_y_continuous(limits = c(0,.5)) +
      labs(x = "x", y = "f(x)")
  }, 
  height = 200, 
  width = 350)
  
  output$meandistPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    X <- sample()
    ggplot() +
      geom_point(x = mean(X$x), y = 0, color = "red", size = 5) +
      geom_vline(xintercept = c(-1,1) * mean(X$x)/sqrt(input$sigma^2/input$n), alpha = .3, color = "red", size = 2) +
      geom_vline(xintercept = 0, alpha = .3, color = "black", size = 1, linetype = "dashed") +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                    color = "black", linewidth = 1) +
      geom_polygon(data = shadeNormalTwoTailedLeft(), aes(x=x, y=y, fill="red")) +
      geom_polygon(data = shadeNormalTwoTailedRight(), aes(x=x, y=y, fill="red")) +
      scale_x_continuous(limits = c(-6,6)) +
      scale_y_continuous(limits = c(0,.5)) +
      labs(x = "z", y = "f(z)")+
      theme(legend.position = "none")
  }, 
  height = 200, 
  width = 350)
  
  output$p_value <- renderText({
    X <- sample()
    paste0("p valor de esta muestra = ", round(2*pnorm(-abs(mean(X$x)/sqrt(input$sigma^2/input$n))), digits = 3))
  })
  
  new_p <- reactive({
    X <- sample()
    c(round(2*pnorm(-abs(mean(X$x)/sqrt(input$sigma^2/input$n))), digits = 3))
  })
  
  observeEvent(input$do, {
    ps$p <- append(ps$p, new_p())
  })
  
  output$histP <- renderPlot({
    # generate bins based on input$bins from ui.R
    Ps <- tibble(ps = ps$p)
    
    ggplot(data = Ps) +
      geom_histogram(aes(x = ps), fill = "steelblue", size = 2, binwidth = .05, boundary = .05) +
      scale_x_continuous(limits = c(0,1))
    
  }, 
  height = 300, 
  width = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)
