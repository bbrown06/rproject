library(shiny)
library(tidyverse)
library(ggrepel)
library(rsconnect)
library(rvest)
library(plotly)
library(ggvis)
east <- read.csv("east.csv")
west <- read_csv("west.csv")
alldata <- read.csv("finaldata.csv")

ui <- fluidPage(
    titlePanel("Progression of 3 Pointers shot by Centers from 2005-2020"),
    fluidRow(
        column(3,
               wellPanel(
                   radioButtons(inputId = "radio",
                                label = "View",
                                choices = list("3PA by Team in the Eastern Conference" = 1,
                                               "3PA by Team in the Western Conference" = 2,
                                               "3P by Team by Season in the Eastern Conference" = 3,
                                               "3P by Team by Season in the Western Conference" = 4,
                                               "2P vs 3P by Team in the NBA" = 5)),
                   conditionalPanel(
                       condition = "input.radio == '1' || input.radio == '3'",
                       selectInput(inputId = "eastteams",
                                   label = "Teams",
                                   choices = c(unique(east$Team)),
                                   selectize = FALSE,
                                   selected = "MIA")),
                   conditionalPanel(
                       condition = "input.radio == '2' || input.radio == '4'",
                       selectInput(inputId = "westteams",
                                   label = "Teams",
                                   choices = c(unique(west$Team)),
                                   selectize = FALSE,
                                   selected = "DEN")),
                   conditionalPanel(
                       condition = "input.radio == '5'",
                       selectInput(inputId = "teams",
                                   label = "Teams",
                                   choices = c(unique(alldata$Team)),
                                   selectize = FALSE,
                                   selected = "MIA")),
               )
        ),
        column(9, mainPanel(plotlyOutput("plot1", width = 700, height = 700)))
    )
)

server <- function(input, output) {
    
    # Eastern Conference Attempted
    Eastconf <- reactive({
        req(input$radio)
        req(input$eastteams)
        filter(east, Team %in% input$eastteams) %>%
            group_by(Season, X3PA)
    })
    
    # Western Conference Attempted
    Westconf <- reactive({
        req(input$radio)
        req(input$westteams)
        filter(west, Team %in% input$westteams) %>%
            group_by(Season, `3PA`)
    })
    
    # Eastern Conference Made
    Eastconf2 <- reactive({
        req(input$radio)
        req(input$eastteams)
        filter(east, Team %in% input$eastteams) %>%
            group_by(Season, X3P)
    })
    
    # Western Conference Made
    Westconf2 <- reactive({
        req(input$radio)
        req(input$westteams)
        filter(west, Team %in% input$westteams) %>%
            group_by(Season, `3P`)
    })
    
    # All teams
    allteams <- reactive({
        req(input$radio)
        req(input$teams)
        filter(alldata, Team %in% input$teams) %>%
            group_by(X2P, X3P)
    })
    
    output$plot1 <- renderPlotly({
        
        input$radio
        if(input$radio == "1"){
            ggplot(Eastconf(), aes(x=Season, y=X3PA, color=X3P)) +
                geom_point() +
                geom_smooth(method = "lm") +
                ggtitle("3 Pointers Attempted per Player per Season") +
                xlab("Season") +
                ylab("3 Pointers Attempted Per Game")
        }
        else if(input$radio == "2"){
            ggplot(Westconf(), aes(x=Season, y=`3PA`, color=`3P`)) +
                geom_point() +
                geom_smooth(method = "lm") +
                ggtitle("3 Pointers Attempted per Player per Season") +
                xlab("Season") +
                ylab("3 Pointers Attempted Per Game")
        } 
        else if(input$radio == "3"){
            ggplot(Eastconf2(), aes(x = Season, y = X3P, fill = Season)) +
                geom_boxplot(color = "blue4", fill = "steelblue") +
                coord_flip() +
                xlim(2004, 2021) +
                ggtitle(paste("Distribution of 3's Made Per Game Per Player by:", input$eastteams)) +
                xlab("Season") +
                ylab("3 Pointers Made Per Game")
        } 
        else if(input$radio == "4"){
            ggplot(Westconf2(), aes(x=Season, y=`3P`, fill = Season)) +
                geom_boxplot(color = "blue4", fill = "steelblue") +
                coord_flip() +
                xlim(2004, 2021) +
                ggtitle(paste("Distribution of 3's Made per Game Per Player by:", input$westteams)) +
                xlab("Season") +
                ylab("3 Pointers Made Per Game")
        } 
        else if(input$radio == "5"){
            ggplot(allteams(), aes(x=X2P, y=X3P, color=Season, text = paste0(Player))) +
                geom_point() +
                ggtitle("2 Pointers Made Per Game vs. 3 Pointers Made Per Game") +
                xlab("2 Pointers Made Per Game") +
                ylab("3 Pointers Made Per Game")
        } 
    })
}

shinyApp(ui = ui, server = server)
