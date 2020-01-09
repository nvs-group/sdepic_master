## app.R ##
##Self Directed EPIC 1/07/2020
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(markdown)
library(tidyverse)
library(DT)
library(tools)
library(shinyjs)
library(sodium)
library(shinyalert)
library(RSQLite)
library(RODBC)
library(shinyjqui)
library(shinymanager)
library(gmailr)
library(shinyBS)

master1 <- read.csv("Master1.csv", stringsAsFactors = FALSE)
cip2 <- read_tsv("cip_code.txt")
cip1 <- cip2[order(cip2$CIP_Category),]
soc2 <- read_tsv("soc_code.txt")
soc1 <- soc2[order(soc2$SOC_Cat_Name),]

header <- dashboardHeader( title = "E.P.I.C. Planning", titleWidth = 230, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Welcome", tabName = "welcome"),
              menuItem("Page 1", tabName = "page1", icon = icon("user")),
              menuItem("Page 2", tabName = "page2", icon = icon("user")),
              menuItem("Page 3", tabName = "page3", icon = icon("tasks")),
              menuItem("Page 4", tabName = "page4", icon = icon("tasks")),
              menuItem("Page 5", tabName = "page5", icon = icon("tasks"))
              
  )
) 
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "welcome", useShinyalert(), useShinyjs(), 
            fluidPage(
              box(width = 12, 
                  h1("Welcome to your Personel EPIC Portal"), align = 'center'),
              carousel(id = "mycarousel", width = 8,
                       carouselItem(caption = "",
                                    h1("  EPIC is a unique investment calculator"),
                                    style = "background-color: gray; height: 600px; width: 1100px"
                                    ),
                       carouselItem(caption = "",
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    h1("for"), align = 'center',
                                    style = "background-color: gray; height: 600px; width: 1100px"
                                    ),
                       carouselItem(caption = "",
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    h1("Building and comparing         "), align = 'right',
                                    style = "background-color: gray; height: 600px; width: 1100px"
                       ),
                       carouselItem(caption = "",
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    br(),br(),br(),
                                    h1("  Education and career scenarios"), align = 'left',
                                    style = "background-color: gray; height: 600px; width: 1100px"
                       )
                       ),
              fluidRow(
                box(width = 12,
                column(width = 2,
                       actionButton(inputId = "get_started", label = "GET STARTED", width = '100%')),
              column(width = 2, offset = 8,
                     actionButton(inputId = "learn_more", label = "LEARN MORE")
              ))))
            ),
    tabItem(tabName = "page1",
            fluidPage(
              box(width = 12, 
                  h1("Let's Get Started"), 
                  h3("Total estimated time to complete is 7 minutes."), 
                  h3("Step 1: Indicate how important and settled these six preferences are in your mind (1 min)"),
                  
                  h3("Low <---------------------------------- IMPORTANCE TO YOU? ----------------------------------> High"),
                  align = 'center'),
              column(width = 2,
                     box(width = 12,
                         h3("Decided"),
                         br(),
                         br(),
                         br(),
                         h3("HOW"),
                         h3("SETTLED"),
                         h3("IN YOUR"),
                         h3("MIND?"),
                         br(),
                         br(),
                         br(),
                         h3("Undecided")
                         )
                     ),
              jqui_droppable(
              column(width = 8, id = "box_2", style = "background-color:#2442aa; height: 475px"
  #                   boxPlus(width = 12, height = '450px', background = 'blue')
                     )),
              column(width = 2,
                     box(width = 6,
                       verbatimTextOutput(outputId = "school_loc" ),
                       verbatimTextOutput(outputId = "occupation_loc" ),
                       verbatimTextOutput(outputId = "curriculum_loc" )
                       ),
                     box(width = 6,
                       verbatimTextOutput(outputId = "degree_loc" ),
                       verbatimTextOutput(outputId = "tuition_loc" ),
                       verbatimTextOutput(outputId = "salary_loc" )
                     )),
              fluidRow(
                box(width = 3,
                    h4("Drag each preference and place it in the space above based on how settled and how important it is to you.")
                    
                ),
              box(width =2,
              jqui_draggable(
                div(id = "school_choice",'School',
                    style = 'width:110px; height:40px; background-color:#79BBF2; font-size: 150%') ,
                options = list( containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "occupation_choice", 'Occupation',
                    style = 'width:110px; height:40px; background-color: red; font-size: 150%'),
                options = list( containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "curriculum_choice", 'Curriculum',
                    style = 'width:110px; height:40px; background-color: green; font-size: 150%'),
                options = list( containment = "#box_2", scroll = FALSE)
              )),
              box(width = 2,
  
              jqui_draggable(
                div(id = "degree_choice", 'Degree',
                    style = 'width:110px; height:40px; background-color: gray; font-size: 150%'),
                options = list( containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "tuition_choice", 'Tuition', 
                    style = 'width:110px; height:40px; background-color: yellow; font-size: 150%'),
                options = list( containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "salary_choice", 'Salary',
                    style = 'width:110px; height:40px; background-color: orange; font-size: 150%'),
                options = list( containment = "#box_2", scroll = FALSE)
              ))
              ))
            ),
    tabItem(tabName = "page2",
            fluidPage(
              box(width = 12, 
                  h3("Step 2: Fill in preferences if you have them (3 min)"), align = 'center'),
              fluidRow(
                column(1,  HTML('<b>State of Residence</b>')),
                column(2,  selectInput(inputId = "epic.state", label = NULL,
                                       choices = sort(unique(master1$State))))
                ),
              fluidRow(
                column(1, HTML('<b>Min. Starting Salary</b>')), 
                column(2, numericInput("epic.income", label = NULL, value = 1))
                ),
              fluidRow(
                column(1, HTML('<b>Max. Annual Ed Cost</b>')), 
                column(2, numericInput("epic.tuition", label = NULL, value = 1))
              ),  
              )
              
            
            ),
    tabItem(tabName = "page3",
            fluidPage(
              box(width = 12, 
                  h3("Step 3: Create and Save Education and Career Scenarios (2 min per scenario)"), align = 'center')
            )
            ),
    tabItem(tabName = "page4",
            fluidPage(
              box(width = 12, 
                  h3("Step 4: Select up to 3 saved scenarios to compare (1 min)"), align = 'center')
            )
            ),
    tabItem(tabName = "page5", 
            fluidPage(
              box(width = 12, 
                  h3("Step 5: Compare Scenarios, Select Favorites, and Request a Report (1 min)"), align = 'center')
            )
            )
  )
)
ui <- dashboardPagePlus(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {

    output$school_loc <- renderPrint({ input$school_choice_position })
    output$occupation_loc <- renderPrint({ input$occupation_choice_position })
    output$curriculum_loc <- renderPrint({ input$curriculum_choice_position })
    output$degree_loc <- renderPrint({ input$degree_choice_position })
    output$tuition_loc <- renderPrint({ input$tuition_choice_position })
    output$salary_loc <- renderPrint({ input$salary_choice_position })
}

shinyApp(ui, server)