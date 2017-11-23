#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 60*1024^2)

library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(stringr, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(stringr, quietly = TRUE)
require(parallel, quietly = TRUE)
require(shinycssloaders, quietly = TRUE)



# Define UI for application
ui <- fluidPage(theme = shinytheme("united"), title = "Interactive Visualizations",
                
                uiOutput(outputId = "title"),
                
                navbarPage("Functions",
                           tabPanel("Uniform",
                                    fluidRow(
                                      column(width = 2,
                                             wellPanel(
                                               titlePanel("Uniform"),
                                               uiOutput(outputId = "settings_unif"),
                                               uiOutput(outputId = "choice_unif")
                                             )
                                      ),
                                      column(width = 10,
                                             withSpinner(plotOutput("unif"))
                                      )
                                    )
                           ), # End tabPanel
                           tabPanel("Normal",
                                    fluidRow(
                                      column(width = 2,
                                             wellPanel(
                                               titlePanel("Normal"),
                                               uiOutput(outputId = "settings_norm"),
                                               uiOutput(outputId = "choice_norm")
                                             )
                                      ),
                                      column(width = 10,
                                              withSpinner(plotOutput("normal"))
                                      )
                                    )
                           ), # End tabPanel
                           tabPanel("Studentized t",
                                    fluidRow(
                                      column(width = 2,
                                             wellPanel(
                                               titlePanel("t-distribution"),
                                               uiOutput(outputId = "settings_t"),
                                               uiOutput(outputId = "choice_t")
                                             )
                                      ),
                                      column(width = 10,
                                             withSpinner(plotOutput("t"))
                                      )
                                    )
                           ) # End tabPanel
                ) # End navbarPage
) # End fluidPage

# Define server logic 
server <- function(input, output) {

  #################################################
  ### Choice of what to visualize
  #################################################

  output$choice_unif <- output$choice_norm <- output$choice_t <- renderUI({
    
    radioButtons(inputId = "vis_selection", label = "What do you want to visualize?",
                 choices = c("pdf", 
                             "Probability more than" = ">", 
                             "Probability less than" = "<", 
                             "Probability between interval" = "int"))
  })
  
  output$settings_unif <- renderUI({
    
    tagList(
      numericInput(inputId = "unif_a", label = "Set the lower limit (a):", value = 0),
      numericInput(inputId = "unif_b", label = "Set the higher limit (b):", value = 1)
    )
    
  })
  
  output$settings_norm <- renderUI({
    
    tagList(
      numericInput(inputId = "norm_mu", label = "Set the mean:", value = 0),
      numericInput(inputId = "norm_sigma", label = "Set the standard deviation:", value = 1)
    )
    
  })
  
  output$settings_t <- renderUI({
    
    tagList(
      numericInput(inputId = "t_mu", label = "Set the mean:", value = 0),
      numericInput(inputId = "t_sigma", label = "Set the standard deviation:", value = 1),
      numericInput(inputId = "t_df", label = "Set the degrees of freedom:", value = 30)
    )
    
  })
  
  #################################################
  ### Uniform
  #################################################
  
  output$unif <- renderPlot({
    
    if(any(is.na(input$unif_a), is.na(input$unif_b))){
      return(NULL)
    } else if(input$unif_a > input$unif_b){
      stop("Upper limit needs to be larger than lower limit.")
    }
    
    a <- input$unif_a
    b <- input$unif_b
    
    start <- a - 0.1*(b-a)
    end <- b + 0.1*(b-a)
    x <- seq(start, end, by = 0.001)
    y <- dunif(x = x, min = a, max = b)
    
    data <- data.frame(x, y)
    
    # plot(x = x, y = y, type = "l")
    ggplot(data) + aes(x = x, y = y) + geom_line() +
      theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      labs(y = "pdf") + scale_y_continuous(expand = c(0,0), limits = c(0, 1.1*max(y))) +
      scale_x_continuous(limits = c(start, end), breaks = seq(a, b, by = round((b-a)/10, 1)))
    
  })
  
  #################################################
  ### Normal
  #################################################
  
  output$normal <- renderPlot({
    
    if(any(is.na(input$norm_mu), is.na(input$norm_sigma))){
      return(NULL)
    } 
    
    mu <- input$norm_mu
    sigma <- input$norm_sigma
    
    start <- mu - 3*sigma
    end <- mu + 3*sigma
    
    x <- seq(start, end, by = 0.001)
    y <- dnorm(x = x, mean = mu, sd = sigma)
    
    data <- data.frame(x, y)
    
    # plot(x = x, y = y, type = "l")
    ggplot(data) + aes(x = x, y = y) + geom_line() +
      theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      labs(y = "pdf") + scale_y_continuous(expand = c(0,0), limits = c(0, 1.1*max(y))) +
      scale_x_continuous(breaks = c(rev(seq(mu, min(x), by = -round((mu-min(x))/5, 1))[-1]), 
                                    seq(mu, max(x), by = round((max(x)-mu)/5, 1))))
    
  })
  
  #################################################
  ### t-distribution
  #################################################
  
  output$t <- renderPlot({
    
    if(any(is.na(input$t_mu), is.na(input$t_sigma))){
      return(NULL)
    } 
    
    mu <- input$t_mu
    sigma <- input$t_sigma
    df <- input$t_df
    
    start <- mu - 5*sigma
    end <- mu + 5*sigma
    
    x <- seq(start, end, by = 0.001)
    y <- sigma*dt(x = x, df = df) + mu
    
    data <- data.frame(x, y)
    
    # plot(x = x, y = y, type = "l")
    ggplot(data) + aes(x = x, y = y) + geom_line() +
      theme_bw() + theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      labs(y = "pdf") + scale_y_continuous(expand = c(0,0), limits = c(0, 1.1*max(y))) +
      scale_x_continuous(breaks = c(rev(seq(mu, min(x), by = -round((mu-min(x))/5, 1))[-1]), 
                                    seq(mu, max(x), by = round((max(x)-mu)/5, 1))))
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
