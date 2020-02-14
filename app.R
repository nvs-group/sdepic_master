## app.R ##
##Self Directed EPIC 2/13/2020
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
#library(dplyr)
#cnames <- readRDS("Column_names.rds")
#degrees <- readRDS("Degrees.rds")
#state_abbr <- read_csv("Book1.csv")
#saveRDS(state_abbr, "state_abbr.rds")
state_abbr <- readRDS("state_abbr.rds")
cips <- readRDS("CIPS.rds")
alt_title <- readRDS("AltTitle.rds")
backbone <- readRDS("Backbone.rds")
backbone <- backbone %>% select(UNITID, CIPCODE, AWLEVEL, CTOTALT, OCCCODE, Entry_Code)
school <- readRDS("Schools.rds")
occupation <- readRDS("Occupations.rds") %>% select(OCCNAME, OCCCODE, OCCTYPE, Emply2018, Emply2028,
                                                    EmplyChg, EmplyPC, SelfEmpl, Openings, MedWage, 
                                                    Experience, OJT)
ent_degree <- readRDS("Ent_Degree.rds")
aw_degree <- readRDS("AW_Degree.rds")

# epic_columns <- c( "INSTNM", "CIPNAME", "Degree_Name", "OCCNAME",
#                  "Entry_Degree", "MedWage")

page1_order <- data.frame(col1 = sample(c("school", "occupation", "degree", "curriculum")), col2 = sample(c(1,2,3,4)))

school_list <- c(character())
degree_list <- c(character())
occupation_list <- c(character())
curriculum_list <- c(character())

school_filter <- c(character())
degree_filter <- c(character())
occupation_filter <- c(character())
curriculum_filter <- c(character())

header <- dashboardHeader( title = "E.P.I.C. Planning", titleWidth = 230, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(
  tags$style(".fa-adjust {color:#E87722}"),
  tags$style(".fa-user {color:blue}"),
  sidebarMenu(id = "tabs",
              menuItem("Welcome", tabName = "welcome"),
              menuItem("Page 1", tabName = "page1", icon = icon("adjust")),
              menuItem("Page 2", tabName = "page2", icon = icon("user")),
              menuItem("Page 3", tabName = "page3", icon = icon("tasks")),
              menuItem("Page 4", tabName = "page4", icon = icon("tasks")),
              menuItem("Page 5", tabName = "page5", icon = icon("tasks"))
  )
) 
body <- dashboardBody(HTML('<meta name="viewport" content="width=1920">'),
                      tags$head(tags$style(HTML("div.col-sm-12 {padding: 0px;
                                                margin-top: -5px; margin-bottom: -10px;
                                                margin-left: -10px; margin-right: -10px; }; "))), 
  tabItems(
    tabItem(tabName = "welcome", useShinyalert(), useShinyjs(),
            fluidPage(
              div(align = 'center', style = "font-size: 20px; padding-top: 0px; margin-top:1em",
                  h1("Welcome to your Personel EPIC Portal")),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '250px',
                height = '100px',
                actionBttn(
                  inputId = "more_welcome",
                  label = "Learn More",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '250px',
                height = '100px',
                actionBttn(
                  inputId = "next_welcome",
                  label = "Get Started",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              )
              )
            ),
    tabItem(tabName = "page1",
            fluidPage( 
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
             
                  h2("Let's Get Started"), 
                  HTML("Total estimated time to complete is 7 minutes.<br>Step 1: Indicate how important and settled these six preferences are in your mind (1 min)<br>
                  Low <---------------------------------- IMPORTANCE TO YOU? ----------------------------------> High")
                  ),
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
#              verbatimTextOutput(outputId = "school_loc" ),
              jqui_droppable(
                column(width = 8, id = "box_2", style = "background-color:#2442aa; height: 475px")),
              
                column(width = 2,
                    h5("Drag each preference and place it in the space to the left based on how settled and how important it is to you.")
                ),
              column(width =2, tags$script(src = "jquery.ui.touch-punch.js"),
              jqui_draggable(
                div(id = "school_1",'School',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color:#79BBF2; font-size: 150%') ,
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "occupation_1", 'Occupation',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: red; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "curriculum_1", 'Curriculum',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: green; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "degree_1", 'Degree',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: gray; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "tuition_1", 'Tuition', 
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: yellow; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "salary_1", 'Salary',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: orange; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              bsPopover(id = "box_2", title = "Placement Help", content = "This is the help",
                        placement = "top", trigger = "click")
              ),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "previous_page1",
                  label = "Go Back",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "next_page1",
                  label = "Next",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              )
              )
            ),
    tabItem(tabName = "page2",
            fluidPage(
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                  h2("Step 2: Fill in preferences if you have them (3 min)")),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "previous_page2",
                  label = "Go Back",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "next_page2",
                  label = "Next",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              fluidRow(
                
                box(width = 2,
                selectInput(inputId = "residence", label = "State of Residence",
                            choices = c(None = '', (sort(state_abbr$State))))
                ),
                box(width = 2,
                    numericInput(inputId = "min_start_salary", label = "Min Starting Salary",
                                 value = 0, min = 0)
                    ),
                box(width = 2,
                    numericInput(inputId = "annual_ed_cost", label = "Max Annual Ed Cost",
                                 value = 50000, min = 0)
                    )
              ),
              fluidRow(
                jqui_sortable(div(id = 'choices',
                                  column(
                                    id = '1',
                                    width = 3,
                                    uiOutput(outputId = "first_choice1"),
                                    uiOutput(outputId = "first_choice2")
                                  ),
                                  column(
                                    id = '2',
                                    width = 3,
                                    uiOutput(outputId = "second_choice1"),
                                    uiOutput(outputId = "second_choice2")
                                    
                                  ), 
                                  column(
                                    id = '3',
                                    width = 3,
                                    uiOutput(outputId = "third_choice1"),
                                    uiOutput(outputId = "third_choice2")
                                  ), 
                                  column(
                                    id = '4',
                                    width = 3,
                                    uiOutput(outputId = "fourth_choice1"),
                                    uiOutput(outputId = "fourth_choice2")
                                  )
                )
                )
              )
            )
    ), 
    tabItem(tabName = "page3",
            fluidPage(
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                  h2("Step 3: Create and Save Education and Career Scenarios (2 min per scenario)")),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "previous_page3",
                  label = "Go Back",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "next_page3",
                  label = "Next",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              fluidRow(jqui_sortable(div(
                id = 'preferences',
                column(width = 3,
                         uiOutput(outputId = "first_choice3")
                       ),
                column(width = 3,
                         uiOutput(outputId = "second_choice3")
                       ),
                column(width = 3,
                         uiOutput(outputId = "third_choice3")
                       ),
                column(width = 3,
                         uiOutput(outputId = "fourth_choice3")
                       )
              ))), 
              fluidRow(
                column(width = 2,
                       actionButton(
                         inputId = "make_table",
                         label = "Build Table",
                         width = '100%'
                       )), 
                       column(width = 8,
                              div(
                                style = 'overflow-x: scroll', DT::dataTableOutput(outputId = "table")
                              )),
                
                absolutePanel(
                  top = 340,
                  right = 50,
                  width = '150px',
                  height = '100px',
                  actionBttn(
                    inputId = "save_row",
                    label = "Save Scenario",
                    size = "md",
                    block = TRUE,
                    color = "primary"
                  )
                )
              )
            )
    ), 
    tabItem(tabName = "page4",
            fluidPage(
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                  h2("Step 4: Select up to 3 saved scenarios to compare (1 min)")),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "previous_page4",
                  label = "Go Back",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "next_page4",
                  label = "Next",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              )
            )
            ),
    tabItem(tabName = "page5", 
            fluidPage(
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                  h2("Step 5: Compare Scenarios, Select Favorites, and Request a Report (1 min)")),
              absolutePanel(
                bottom = 0,
                left = "auto",
                width = '150px',
                height = '100px',
                actionBttn(
                  inputId = "previous_page5",
                  label = "Go Back",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ),
              absolutePanel(
                bottom = 0,
                right = 50,
                width = '300px',
                height = '100px',
                actionBttn(
                  inputId = "email_page5",
                  label = "Please Send Me a Copy",
                  size = "md",
                  block = TRUE,
                  color = "primary"
                )
              ) 
            )
            )
  )
)
ui <- dashboardPagePlus(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  
  page1_list <- reactive({
    page1_order <- c(character(), numeric())
    school <- as.numeric(input$school_1_position$top)
    school <- round(((-school + 370) / 48.3), 1)
    page1_order <- rbind(page1_order, c("school", school))
    degree <- as.numeric(input$degree_1_position$top)
    degree <- round(((-degree + 370) / 48.3), 1)
    page1_order <- rbind(page1_order, c("degree", degree))
    occupation <- as.numeric(input$occupation_1_position$top)
    occupation <- round(((-occupation + 370) / 48.3), 1)
    page1_order <- rbind(page1_order, c("occupation", occupation))
    curriculum <- as.numeric(input$curriculum_1_position$top)
    curriculum <- round(((-curriculum + 370) / 48.3), 1)
    page1_order <- rbind(page1_order, c("curriculum", curriculum))
    orderlist <- page1_order[ order( page1_order[ ,2]), ]
    
  }) 
  first_spot <- reactive({
    page1_list()[4]
  })
  second_spot <- reactive({
    page1_list()[3]
  })
  third_spot <- reactive({
    page1_list()[2]
  })
  fourth_spot <- reactive({
    page1_list()[1]
  })
  
  occ_temp1 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
                strong("Occupation")),
            textInput(inputId = "occupation_text", label = NULL, width = '100%'),
      selectInput(
        inputId = "occupation_first",
        label = NULL,
        choices = isolate(occupation$OCCNAME),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ),
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: -18px",
          actionBttn(inputId = "occupation_add", "Add", style = "fill", color = "primary", size = "sm"))
    )
  })
  occ_temp2 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
          selectInput(    
        inputId = "occupation_middle",
        label = "",
        choices = c(None = '',occupation_list),
        selectize = FALSE,
        size = 5,
        width = '100%'
      )),
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: 10px",
          actionBttn(inputId = "occupation_delete", "Delete", style = "fill", color = "danger", size = "sm"))
    )
  })
  occ_temp3 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
      selectInput(
        inputId = "occupation_last",
        label = "Occupation",
        choices = c(All = '', occupation_list),
        selectize = FALSE,
        size = 7,
        width = '100%'
      ))
    )
  })
  school_temp1 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
          strong("School")),
      textInput(inputId = "school_text", label = NULL, width = '100%'),
      selectInput(
        inputId = "school_first",
        label = NULL,
        choices = isolate(sort(school$INSTNM)),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ),
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: -18px",
      actionBttn(inputId = "school_add", "Add", style = "fill", color = "primary", size = "sm"))
    )
  })
  
  school_temp2 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
      selectInput(
        inputId = "school_middle",
        label = "",
        choices = c(None = '', school_list),
        selectize = FALSE,
        size = 5,
        width = '100%'
    )),
    div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: 10px",
        actionBttn(inputId = "school_delete", "Delete", style = "fill", color = "danger", size = "sm"))
    )
  })
  school_temp3 <- renderUI({
    tagList(
        div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
      selectInput(
        inputId = "school_last",
        label = "School",
        choices = c(All = '',school_list),
        selectize = FALSE,
        size = 7,
        width = '100%'
      ))
    )
  })
 curr_temp1 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
          strong("Curriculum")),
      textInput(inputId = "curriculum_text", label = NULL, width = '100%'),
        selectInput(
          inputId = "curriculum_first",
          label = NULL,
          choices = isolate(cips$CIPNAME),
          selectize = FALSE,
          size = 5,
          width = '100%'
        ),
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: -18px",
          actionBttn(inputId = "curriculum_add", "Add", style = "fill", color = "primary", size = "sm"))
      )
    
  })
 curr_temp2 <- renderUI({
   tagList(div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
     selectInput(
       inputId = "curriculum_middle",
       label = "",
       choices = c(None = '',curriculum_list),
       selectize = FALSE,
       size = 5,
       width = '100%'
     )),
     div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: 10px",
         actionBttn(inputId = "curriculum_delete", "Delete", style = "fill", color = "danger", size = "sm"))
   )
 })
 curr_temp3 <- renderUI({
   tagList(
     div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
     selectInput(
       inputId = "curriculum_last",
       label = "Curriculum",
       choices = c(All = '', curriculum_list),
       selectize = FALSE,
       size = 7,
       width = '100%'
     )
   ))
 })

 degree_temp1 <- renderUI({
   tagList(
     div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
               strong("Degree")),
     textInput(inputId = "degree_text", label = NULL, width = '100%'),
     selectInput(
       inputId = "degree_first",
       label = NULL,
       choices = isolate(aw_degree$Degree_Name),
       selectize = FALSE,
       size = 5,
       width = '100%'
     ),
     div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: -18px",
         actionBttn(inputId = "degree_add", "Add", style = "fill", color = "primary", size = "sm"))
   )
 })
  degree_temp2 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
        selectInput(
          inputId = "degree_middle",
          label = "",
          choices = c(None = '',degree_list),
          selectize = FALSE,
          size = 5,
          width = '100%'
        )),
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-10px; margin-bottom: 10px",
              actionBttn(inputId = "degree_delete", "Delete", style = "fill", color = "danger", size = "sm"))
    )
  })
  degree_temp3 <- renderUI({
    tagList(
      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
        selectInput(
        inputId = "degree_last",
        label = "Degree",
        choices = c(All = '', degree_list),
        selectize = FALSE,
        size = 7,
        width = '100%'
      ))
    )
  })
  observeEvent(input$tabs,{
    if(input$tabs == "page2"){
      click("next_page1")
    }
  })
  observeEvent(input$next_page1,{
    req(input$next_page1 < 2)
    if (first_spot() == "school") {
      output$first_choice1 <- school_temp1
      output$first_choice2 <- school_temp2
      output$first_choice3 <- school_temp3
    }
    if (first_spot() == "occupation") {
      output$first_choice1 <- occ_temp1
      output$first_choice2 <- occ_temp2
      output$first_choice3 <- occ_temp3
    }
    if (first_spot() == "degree") {
      output$first_choice1 <- degree_temp1
      output$first_choice2 <- degree_temp2
      output$first_choice3 <- degree_temp3
    }
    if (first_spot() == "curriculum") {
      output$first_choice1 <- curr_temp1
      output$first_choice2 <- curr_temp2
      output$first_choice3 <- curr_temp3
    }
    if (second_spot() == "school") {
      output$second_choice1 <- school_temp1
      output$second_choice2 <- school_temp2
      output$second_choice3 <- school_temp3
    }
    if (second_spot() == "occupation") {
      output$second_choice1 <- occ_temp1
      output$second_choice2 <- occ_temp2
      output$second_choice3 <- occ_temp3
    }
    if (second_spot() == "degree") {
      output$second_choice1 <- degree_temp1
      output$second_choice2 <- degree_temp2
      output$second_choice3 <- degree_temp3
    }
    if (second_spot() == "curriculum") {
      output$second_choice1 <- curr_temp1
      output$second_choice2 <- curr_temp2
      output$second_choice3 <- curr_temp3
    }  
    if (third_spot() == "school") {
      output$third_choice1 <- school_temp1
      output$third_choice2 <- school_temp2
      output$third_choice3 <- school_temp3
    }
    if (third_spot() == "occupation") {
      output$third_choice1 <- occ_temp1
      output$third_choice2 <- occ_temp2
      output$third_choice3 <- occ_temp3
    }
    if (third_spot() == "degree") {
      output$third_choice1 <- degree_temp1
      output$third_choice2 <- degree_temp2
      output$third_choice3 <- degree_temp3
    }
    if (third_spot() == "curriculum") {
      output$third_choice1 <- curr_temp1
      output$third_choice2 <- curr_temp2
      output$third_choice3 <- curr_temp3
    }      
    if (fourth_spot() == "school") {
      output$fourth_choice1 <- school_temp1
      output$fourth_choice2 <- school_temp2
      output$fourth_choice3 <- school_temp3
    }
    if (fourth_spot() == "occupation") {
      output$fourth_choice1 <- occ_temp1
      output$fourth_choice2 <- occ_temp2
      output$fourth_choice3 <- occ_temp3
    }
    if (fourth_spot() == "degree") {
      output$fourth_choice1 <- degree_temp1
      output$fourth_choice2 <- degree_temp2
      output$fourth_choice3 <- degree_temp3
    }
    if (fourth_spot() == "curriculum") {
      output$fourth_choice1 <- curr_temp1
      output$fourth_choice2 <- curr_temp2
      output$fourth_choice3 <- curr_temp3
    }      
  })
school_table <- reactive({
  school_t1 <- school
  if(!is.null(input$school_text)){
    school_t1 <- filter(school_t1 , grepl(input$school_text, school_t1$INSTNM, ignore.case = TRUE))
  }
})
observeEvent(input$school_text,{
  if(!is.null(input$school_text)){
    school_filter <<- school %>% filter(school$INSTNM %in% school_table()$INSTNM) 
    school_filter <<- school_filter %>% select(INSTNM)
    updateSelectInput(session, inputId = "school_first", label = NULL,
                      choices = isolate(sort(school_filter[,1])))
  }
})
degree_table <- reactive({
  degree_t1 <- aw_degree
  if(!is.null(input$degree_text)){
    degree_t1 <- filter(degree_t1 , grepl(input$degree_text, degree_t1$Degree_Name, ignore.case = TRUE))
  }
})
observeEvent(input$degree_text, {
#  req(input$degree_text)
  if(!is.null(input$degree_text)){
    degree_filter <<- aw_degree %>% filter(aw_degree$Degree_Name %in% degree_table()$Degree_Name)
    degree_filter <<- degree_filter$Degree_Name
 
    updateSelectInput(session, inputId = "degree_first", label = NULL,
                      choices = isolate(degree_filter))
  }
})
curriculum_table <- reactive({
  curriculum_t1 <- cips
  if(!is.null(input$curriculum_text)){
    curriculum_t1 <- filter(curriculum_t1 , grepl(input$curriculum_text, curriculum_t1$CIPNAME, ignore.case = TRUE))
  }
})
observeEvent(input$curriculum_text,{
  if(!is.null(input$curriculum_text)){
    curriculum_filter <<- cips %>% filter(cips$CIPNAME %in% curriculum_table()$CIPNAME) 
    curriculum_filter <<- curriculum_filter %>% select(CIPNAME)
    updateSelectInput(session, inputId = "curriculum_first", label = NULL,
                      choices = isolate(sort(curriculum_filter[,1])))
  }
})
  occupation_table <- reactive({
    occupation_t1 <- alt_title
    if(!is.null(input$occupation_text)){
      occupation_t1 <- filter(occupation_t1 , grepl(input$occupation_text, occupation_t1$AltName, ignore.case = TRUE))
    }
  })
  observeEvent(input$occupation_text,{
    if(!is.null(input$occupation_text)){
    occupation_filter <<- occupation %>% filter(occupation$OCCCODE %in% occupation_table()$OCCCODE) 
    updateSelectInput(session, inputId = "occupation_first", label = NULL,
                      choices = sort(unique(occupation_filter$OCCNAME)))
    }
  })
  
  table_var <- reactive ({
    backbone_temp <- backbone
    if(!is.null(input$occupation_last) & input$occupation_last !='') {
      occupation_temp <- filter(occupation, OCCNAME %in% input$occupation_last) %>% select(OCCCODE)
      backbone_temp <- filter(backbone_temp, OCCCODE %in% occupation_temp)
    }
    if(!is.null(input$school_last) & input$school_last !='') {
      school_temp <- filter(school, INSTNM %in% input$school_last) %>% select(UNITID)
      backbone_temp <- filter(backbone_temp, UNITID %in% school_temp)
    }
    if(!is.null(input$curriculum_last) & input$curriculum_last !='') {
      curriculum_temp <- filter(cips, CIPNAME %in% input$curriculum_last) %>% select(CIPCODE)
      backbone_temp <- filter(backbone_temp, CIPCODE %in% curriculum_temp)                      
    }
    if(!is.null(input$degree_last) & input$degree_last !='') {
      degree_temp <- filter(aw_degree, Degree_Name %in% input$degree_last) %>% select(AWLEVEL)
      backbone_temp <- filter(backbone_temp, AWLEVEL %in% degree_temp)
    }
    filtered_table <- left_join(backbone_temp, school, by = "UNITID")
    filtered_table <- left_join(filtered_table, cips, by = "CIPCODE")
    filtered_table <- left_join(filtered_table, aw_degree, by = "AWLEVEL")
    filtered_table <- left_join(filtered_table, ent_degree, by = "Entry_Code")
    filtered_table <- left_join(filtered_table, occupation, by = "OCCCODE")
    filtered_table <- filtered_table %>% select("INSTNM", "CIPNAME", "Degree_Name", "OCCNAME",
                                                "Entry_Degree", "MedWage")
    school_filter <<- unique(filtered_table$INSTNM)
    degree_filter <<- unique(filtered_table$Degree_Name)
    occupation_filter <<- unique(filtered_table$OCCNAME)
    curriculum_filter <<- unique(filtered_table$CIPNAME)
    
    if(identical(school_list, character(0))){
      updateSelectInput(session, inputId = "school_last", label = "School",
                        choices = isolate(c(All = '', sort(school_filter))), selected = '')
    }
    if(identical(occupation_list, character(0))){
      updateSelectInput(session, inputId = "occupation_last", label = "Occupation",
                        choices = isolate(c(All = '', sort(occupation_filter))), selected = '')
    }
    if(identical(curriculum_list, character(0))){
      updateSelectInput(session, inputId = "curriculum_last", label = "Curriculum",
                        choices = isolate(c(All = '', sort(curriculum_filter))), selected = '')
    }
    if(identical(degree_list, character(0))){
      updateSelectInput(session, inputId = "degree_last", label = "Degree",
                        choices = isolate(c(All = '', sort(degree_filter))), selected = '')
    }
    
    filtered_table <- filtered_table %>% rename("Institution<br>Name" = "INSTNM",
                                                "Curriculum<br>Name" = "CIPNAME", "Degree<br>Name" = "Degree_Name", 
                                                "Occupation<br>Name" = "OCCNAME", "Median<br>Wage" = "MedWage",
                                                "Entry_Level<br>Degree" = "Entry_Degree")
  })
  
  observeEvent(input$make_table,{
    output$table <- renderDataTable({
      DT::datatable(
        data = table_var(),
        escape = FALSE,
        options = list(
          filter = FALSE,
          pageLength = 5,
          autoWidth = TRUE,
          lengthMenu = c(5, 8, 10)
        ),
        selection = list(mode = 'single')
      ) %>%
        formatStyle(
          0,
          target = 'row',
          color = 'black',
          backgroundColor = 'grey',
          fontWeight = 'bold',
          lineHeight = '100%'
        )
    })
  })
    observeEvent(input$next_welcome, {
      updateTabsetPanel(session, "tabs",selected = "page1")
    })
    observeEvent(input$next_page1, {
      updateTabsetPanel(session, "tabs",selected = "page2")
    })
    observeEvent(input$previous_page1, {
      updateTabsetPanel(session, "tabs",selected = "welcome")
    })
    observeEvent(input$next_page2, {
      updateTabsetPanel(session, "tabs",selected = "page3")
    })
    observeEvent(input$previous_page2, {
      updateTabsetPanel(session, "tabs",selected = "page1")
    })
    observeEvent(input$next_page3, {
      updateTabsetPanel(session, "tabs",selected = "page4")
    })
    observeEvent(input$previous_page3, {
      updateTabsetPanel(session, "tabs",selected = "page2")
    })
    observeEvent(input$next_page4, {
      updateTabsetPanel(session, "tabs",selected = "page5")
    })
    observeEvent(input$previous_page4, {
      updateTabsetPanel(session, "tabs",selected = "page3")
    })
    observeEvent(input$previous_page5, {
      updateTabsetPanel(session, "tabs",selected = "page4")
    })

    observeEvent(input$school_add, {
      if (!is.null(input$school_first)) {
        school_list <<- rbind(school_list, input$school_first)
        school_list <<- unique(school_list)
        updateSelectInput(session,
                          inputId = "school_middle",
                          label = "",
                          choices = isolate(c(None = '', school_list))
        )
        updateSelectInput(session,
                          inputId = "school_last",
                          label = "School",
                          choices = c(All = '', school_list),
                          selected = '')
      }
    })
    observeEvent(input$school_delete, {
      if(!is.null(input$school_middle)) {
        school_list <<- school_list[!school_list %in% input$school_middle]
        updateSelectInput(session,
                          inputId = "school_middle",
                          label = "",
                          choices = isolate(c(None = '', school_list)))
        updateSelectInput(session,
                          inputId = "school_last",
                          label = "School",
                          choices = isolate(c(All = '', school_list)),
                          selected = '')
      }
    })
    observeEvent(input$degree_add, {
      if (!is.null(input$degree_first)) {
        degree_list <<- rbind(degree_list, input$degree_first)
        degree_list <<- unique(degree_list)
        updateSelectInput(session,
                          inputId = "degree_middle",
                          label = "",
                          choices = isolate(c(None = '', degree_list)))
        updateSelectInput(session,
                          inputId = "degree_last",
                          label = "Degree",
                          choices = c(All = '', degree_list),
                          selected = '')
      }
    })
    observeEvent(input$degree_delete, {
      if(!is.null(input$degree_middle)) {
        degree_list <<- degree_list[!degree_list %in% input$degree_middle]
        updateSelectInput(session,
                          inputId = "degree_middle",
                          label = "",
                          choices = isolate(c(None = '', degree_list)))
        updateSelectInput(session,
                          inputId = "degree_last",
                          label = "Degree",
                          choices = isolate(c(All = '', degree_list)),
                          selected = '')
      }
    })
    observeEvent(input$degree_last,{
      req(input$degree_last)
      if(identical(degree_list, character(0))){
        degree_list <<- rbind(degree_list, input$degree_last)
        updateSelectInput(session,
                          inputId = "degree_last",
                          label = "Degree",
                          choices = c(All = '', degree_list),
                          selected = degree_list[,1])
        updateSelectInput(session,
                          inputId = "degree_middle",
                          label = "",
                          choices = isolate(c(None = '', degree_list))
        )
      }
    })
    observeEvent(input$occupation_last,{
      req(input$occupation_last)
      if(identical(occupation_list, character(0))){
        occupation_list <<- rbind(occupation_list, input$occupation_last)
        updateSelectInput(session,
                          inputId = "occupation_last",
                          label = "Occupation",
                          choices = c(All = '', occupation_list),
                          selected = occupation_list[,1])
        updateSelectInput(session,
                          inputId = "occupation_middle",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
      }
    })
    observeEvent(input$school_last,{
      req(input$school_last)
      if(identical(school_list, character(0))){
        school_list <<- rbind(school_list, input$school_last)
        updateSelectInput(session,
                          inputId = "school_last",
                          label = "School",
                          choices = c(All = '', school_list),
                          selected = school_list[,1])
        updateSelectInput(session,
                          inputId = "school_middle",
                          label = "",
                          choices = isolate(c(None = '', school_list)))
      }
    })
    observeEvent(input$curriculum_last,{
      req(input$curriculum_last)
      if(identical(curriculum_list, character(0))){
      curriculum_list <<- rbind(curriculum_list, input$curriculum_last)
        updateSelectInput(session,
                          inputId = "curriculum_last",
                          label = "Curriculum",
                          choices = c(All = '', curriculum_list),
                          selected = curriculum_list[,1])
        updateSelectInput(session,
                          inputId = "curriculum_middle",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
      }
    })
    observeEvent(input$occupation_add, {
      if (!is.null(input$occupation_first)) {
        occupation_list <<- rbind(occupation_list, input$occupation_first)
        occupation_list <<- unique(occupation_list)
        updateSelectInput(session,
                          inputId = "occupation_middle",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
        updateSelectInput(session,
                          inputId = "occupation_last",
                          label = "Occupation",
                          choices = c(All = '', occupation_list),
                          selected = '')
      }
    })
    observeEvent(input$occupation_delete, {
      if(!is.null(input$occupation_middle)) {
        occupation_list <<- occupation_list[!occupation_list %in% input$occupation_middle]
        updateSelectInput(session,
                          inputId = "occupation_middle",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
        updateSelectInput(session,
                          inputId = "occupation_last",
                          label = "Occupation",
                          choices = isolate(c(All = '', occupation_list)),
                          selected = '')
      }
    })
    observeEvent(input$curriculum_add, {
      if (!is.null(input$curriculum_first)) {
        curriculum_list <<- rbind(curriculum_list, input$curriculum_first)
        curriculum_list <<- unique(curriculum_list)
        updateSelectInput(session,
                          inputId = "curriculum_middle",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
        updateSelectInput(session,
                          inputId = "curriculum_last",
                          label = "Curriculum",
                          choices = c(All = '', curriculum_list),
                          selected = '')
      }
    })
    observeEvent(input$curriculum_delete, {
      if(!is.null(input$curriculum_middle)) {
        curriculum_list <<- curriculum_list[!curriculum_list %in% input$curriculum_middle]
        updateSelectInput(session,
                          inputId = "curriculum_middle",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
        updateSelectInput(session,
                          inputId = "curriculum_last",
                          label = "Curriculum",
                          choices = isolate(c(All = '', curriculum_list)),
                          selected = '')
      }
    })
}

shinyApp(ui, server)