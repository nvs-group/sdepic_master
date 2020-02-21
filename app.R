## app.R ##
##Self Directed EPIC 2/20/2020
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
library(V8)


school_list <- c(character())
degree_list <- c(character())
occupation_list <- c(character())
curriculum_list <- c(character())

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


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
body <- dashboardBody(
                    #  tags$script(HTML("$('body').addClass('fixed');")),
                   # tags$script (src="http://code.jquery.com/jquery-1.7.2.min.js"),
                   # tags$script (src="http://code.jquery.com/ui/1.8.21/jquery-ui.min.js"),
                      tags$script(src = "jquery.ui.touch-punch.js"),
#                      tags$head(tags$style(HTML("div.col-sm-12 {padding: 0px;
#                                                margin-top: -5px; margin-bottom: -10px;
#                                                margin-left: -10px; margin-right: -10px; }; "))), 
  tabItems(
    tabItem(tabName = "welcome", useShinyjs(), useShinyalert(),extendShinyjs(text = jscode),
            fluidPage(
              div(align = 'center', style = "font-size: 20px; padding-top: 0px; margin-top:1em",
                  h1("Welcome to your Personel EPIC Portal"))
              )
            ),
    tabItem(tabName = "page2",
            fluidPage(
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em") 
              )
            ), 
            tabItem(tabName = "page3",
                    fluidPage(
                      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                          h2("Step 3: Create and Save Education and Career Scenarios (2 min per scenario)"))
                    )
            ), 
            tabItem(tabName = "page4",
                    fluidPage(
                      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                          h2("Step 4: Select up to 3 saved scenarios to compare (1 min)"))
                    )
            ),
            tabItem(tabName = "page5", 
                    fluidPage(
                      div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                          h2("Step 5: Compare Scenarios, Select Favorites, and Request a Report (1 min)"))
                    )
            ),
    tabItem(tabName = "page1",
            fluidPage( 
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em", 
                  h2("Let's Get Started")),
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em", 
                  h2("Step 1: Fill in preferences if you have them (3 min)")),
              fluidRow(
                column(
                  id = '5',
                  width = 3,
                  div( style = "font-size: 16px; margin-left:-20px;margin-right:-20px; margin-top:0em",
                  boxPlus(
                    title = "State Tuition Salary",
                    width = 12,
                    solidHeader = TRUE,
                    background = 'navy',
                    collapsible = TRUE,
                    collapsed = FALSE,
                    closable = FALSE,
                    accordion(
                      inputId = "accordion5",
                      accordionItem(
                        id = 50,
                        title = "State of Residence",
                        color = "primary",
                        collapsed = TRUE,
                        div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                            selectInput(
                              inputId = "residence",
                              label = "State of Residence",
                              choices = '',
                              selectize = FALSE,
                              size = 6,
                              width = '100%'
                            )
                            )
                      ),
                      accordionItem(
                        id = 51,
                        title = "State of Interest",
                        color = "primary",
                        collapsed = TRUE,
                        div( style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                            selectInput(
                              inputId = "state_filter",
                              label = "",
                              choices = c(None = '', "Confusion"),
                              selectize = FALSE,
                              size = 6,
                              width = '100%'
                            )
                        )
                      ),
                      accordionItem(
                        id = 52,
                        title = "Required Entry Degree",
                        color = "primary",
                        collapsed = TRUE,
                        div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                            selectInput(
                              inputId = "required_degree",
                              label = "Required Degree",
                              choices = c(None = ''),
                              selectize = FALSE,
                              size = 6,
                              width = '100%'
                            )
                        )
                      ),
                      accordionItem(
                        id = 53,
                        title = "Minimum Starting Salary",
                        color = "primary",
                        collapsed = TRUE,
                        div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                            sliderInput(inputId = "min_start_salary",
                                        label = "Desired Income Level:",
                                        value = 0,
                                        min = 0,
                                        step = 1000,
                                        max = 150000
                                        )
                            )
                      ),
                      accordionItem(
                        id = 54,
                        title = "Desired Education Yearly Cost",
                        color = "primary",
                        collapsed = TRUE,
                        div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                            sliderInput(inputId = "annual_ed_cost",
                                        label = "Desired Tuition Level",
                                        value = 100000,
                                        min = 0,
                                        step = 1000,
                                        max = 100000
                                        )
                        ) 
                      )
                      )
                    )))
              ),
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em", 
                  h2("Step 2: Arrange the items below in order of importance and certainty (3 min)")),
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-bottom: 1em",
                  actionBttn(inputId = "min_max_button", "Open/Close All", style = "fill", color = "primary", size = "sm")
                  ),
                  
              fluidRow(
                jqui_sortable(div(id = 'choices',
                                  column(
                                    id = '1',
                                    width = 3,
                                    div( style = "font-size: 16px; margin-left:-20px;margin-right:-20px; margin-top:0em",
                                    boxPlus(
                                      id = "box1",
                                      title = "Occupation",
                                      width = 12,
                                      solidHeader = TRUE,
                                      background = 'blue',
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      closable = FALSE,
                                      accordion(
                                        inputId = "accordion1",
                                        accordionItem(
                                          id = 10,
                                          title = "Select Occupation(s)",
                                          color = "primary",
                                          collapsed = FALSE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                          textInput(inputId = "occupation_text", label = NULL, width = '100%')),
                                          selectInput(
                                            inputId = "occupation_first",
                                            label = NULL,
                                            choices = '',
                                            selected = '',
                                            selectize = FALSE,
                                            size = 8,
                                            width = '100%'
                                          ),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 5px",
                                              actionBttn(inputId = "occupation_add", "Add", style = "fill", color = "primary", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                              selectInput(    
                                                inputId = "occupation_middle",
                                                label = "",
                                                choices = c(None = '',occupation_list),
                                                selectize = FALSE,
                                                size = 5,
                                                width = '100%'
                                              ))
                                        ),
                                        accordionItem(
                                          id = 11,
                                          title = "Selected Items",
                                          color = "success",
                                          collapsed = TRUE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                                              selectInput(    
                                                inputId = "occupation_last",
                                                label = "",
                                                choices = c(None = '',occupation_list),
                                                selectize = FALSE,
                                                size = 6,
                                                width = '100%'
                                              )),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: -10px",
                                              actionBttn(inputId = "occupation_delete", "Delete", style = "fill", color = "danger", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
                                              selectInput(
                                                inputId = "occupation_filtered",
                                                label = "Occupation",
                                                choices = c(All = '', occupation_list),
                                                selectize = FALSE,
                                                size = 9,
                                                width = '100%'
                                              ))
                                        )
                                        )
                                      ))
                                    ),
                                  column(
                                    id = '2',
                                    width = 3,
                                    div( style = "font-size: 16px; margin-left:-20px;margin-right:-20px; margin-top:0em",
                                    boxPlus(
                                      id = "box2",
                                      title = "Curriculum",
                                      width = 12,
                                      solidHeader = TRUE,
                                      background = 'green',
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      closable = FALSE,
                                      accordion(
                                        inputId = "accordion2",
                                        accordionItem(
                                          id = 20,
                                          title = "Select Curriculum(s)",
                                          color = "primary",
                                          collapsed = FALSE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                          textInput(inputId = "curriculum_text", label = NULL, width = '100%')),
                                          selectInput(
                                            inputId = "curriculum_first",
                                            label = NULL,
                                            choices = '',
                                            selected = '',
                                            selectize = FALSE,
                                            size = 8,
                                            width = '100%'
                                          ),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 5px",
                                              actionBttn(inputId = "curriculum_add", "Add", style = "fill", color = "primary", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                              selectInput(
                                                inputId = "curriculum_middle",
                                                label = "",
                                                choices = c(None = '',curriculum_list),
                                                selectize = FALSE,
                                                size = 5,
                                                width = '100%'
                                              ))
                                    
                                        ),
                                        accordionItem(
                                          id = 21,
                                          title = "Selected Items",
                                          color = "success",
                                          collapsed = TRUE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                                              selectInput(
                                                inputId = "curriculum_last",
                                                label = "",
                                                choices = c(None = '',curriculum_list),
                                                selectize = FALSE,
                                                size = 6,
                                                width = '100%'
                                              )),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: -10px",
                                              actionBttn(inputId = "curriculum_delete", "Delete", style = "fill", color = "danger", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
                                              selectInput(
                                                inputId = "curriculum_filtered",
                                                label = "Curriculum",
                                                choices = c(All = '', curriculum_list),
                                                selectize = FALSE,
                                                size = 9,
                                                width = '100%'
                                              )
                                          )
                                      )
                                    )
                                    ))
                                  )
                                  , 
                                  column(
                                    id = '3',
                                    width = 3,
                                    div( style = "font-size: 16px; margin-left:-20px;margin-right:-20px; margin-top:0em",
                                    boxPlus(
                                      id = "box3",
                                      title = "School",
                                      width = 12,
                                      solidHeader = TRUE,
                                      background = 'orange',
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      closable = FALSE,
                                      accordion(
                                        inputId = "accordion3",
                                        accordionItem(
                                          id = 30,
                                          title = "Select School(s)",
                                          color = "primary",
                                          collapsed = FALSE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                          textInput(inputId = "school_text", label = NULL, width = '100%')),
                                          selectInput(
                                            inputId = "school_first",
                                            label = NULL,
                                            choices = '',
                                            selected = '',
                                            selectize = FALSE,
                                            size = 8,
                                            width = '100%'
                                          ),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 5px",
                                              actionBttn(inputId = "school_add", "Add", style = "fill", color = "primary", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                              selectInput(
                                                inputId = "school_middle",
                                                label = "",
                                                choices = c(None = '', school_list),
                                                selectize = FALSE,
                                                size = 5,
                                                width = '100%'
                                              ))
                                        ),
                                        accordionItem(
                                          id = 31,
                                          title = "Selected Items",
                                          color = "success",
                                          collapsed = TRUE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                                              selectInput(
                                                inputId = "school_last",
                                                label = "",
                                                choices = c(None = '', school_list),
                                                selectize = FALSE,
                                                size = 6,
                                                width = '100%'
                                              )),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: -10px",
                                              actionBttn(inputId = "school_delete", "Delete", style = "fill", color = "danger", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
                                              selectInput(
                                                inputId = "school_filtered",
                                                label = "School",
                                                choices = c(All = '',school_list),
                                                selectize = FALSE,
                                                size = 9,
                                                width = '100%'
                                              ))
                                        )
                                      )
                                    ))
                                  ), 
                                  column(
                                    id = '4',
                                    width = 3,
                                    div( style = "font-size: 16px; margin-left:-20px;margin-right:-20px; margin-top:0em",
                                    boxPlus(
                                      id = "box4",
                                      title = "Degree",
                                      width = 12,
                                      solidHeader = TRUE,
                                      background = 'red',
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      closable = FALSE,
                                      accordion(
                                        inputId = "accordion4",
                                        accordionItem(
                                          id = 40,
                                          title = "Select Degree(s)",
                                          color = "primary",
                                          collapsed = FALSE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                              
                                          textInput(inputId = "degree_text", label = NULL, width = '100%')),
                                          selectInput(
                                            inputId = "degree_first",
                                            label = NULL,
                                            choices = '',
                                            selected = '',
                                            selectize = FALSE,
                                            size = 8,
                                            width = '100%'
                                          ),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 5px",
                                              actionBttn(inputId = "degree_add", "Add", style = "fill", color = "primary", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em",
                                              selectInput(
                                                inputId = "degree_middle",
                                                label = "",
                                                choices = c(None = '',degree_list),
                                                selectize = FALSE,
                                                size = 5,
                                                width = '100%'
                                              ))
                                        ),
                                        accordionItem(
                                          id = 41,
                                          title = "Selected Items",
                                          color = "success",
                                          collapsed = TRUE,
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-2em",
                                              selectInput(
                                                inputId = "degree_last",
                                                label = "",
                                                choices = c(None = '',degree_list),
                                                selectize = FALSE,
                                                size = 6,
                                                width = '100%'
                                              )),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: -10px",
                                              actionBttn(inputId = "degree_delete", "Delete", style = "fill", color = "danger", size = "sm")),
                                          div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:0em",
                                              selectInput(
                                                inputId = "degree_filtered",
                                                label = "Degree",
                                                choices = c(All = '', degree_list),
                                                selectize = FALSE,
                                                size = 9,
                                                width = '100%'
                                              ))
                                        )
                                      )
                                    ))
                                    
                                  )
                                  
                )
                )
              ),
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em", 
                  h2("Step 3: Build Scenerios and Create list from choices above")
                  ),
              fluidRow(column(
                width = 6,
                div(
                  align = 'center',
                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
                  actionBttn(
                    inputId = "create_table",
                    "Build Table",
                    style = "fill",
                    color = "danger",
                    size = "md"
                  )
                )
              ),
              column(
                width = 6,
                div(
                  align = 'center',
                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
                  
                  actionBttn(
                    inputId = "add_scenerio",
                    "Add Scenerio",
                    style = "fill",
                    color = "success",
                    size = "md"
                  )
                )
              )), 
              
              fluidRow(
                column(
                  width = 12,
                  
                  div(style = 'overflow-x: scroll',
                      DT::dataTableOutput(
                        outputId = "table",
                        width = "100%",
                        height = "auto"
                      )))
              ),
              div(align = 'center', style = "font-size: 16px; padding-top: 0px; margin-top:-1em", 
                  h2("Step 4: Select scenerios to compare")
              ),
              fluidRow(column(
                width = 4,
                div(
                  align = 'center',
                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
                  actionBttn(
                    inputId = "load_scenerio",
                    "Load Scenerios",
                    style = "fill",
                    color = "primary",
                    size = "md"
                  )
                )
              ),
              column(
                width = 4,
                div(
                  align = 'center',
                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
                  
                  actionBttn(
                    inputId = "save_scenerio",
                    "Save Scenerio",
                    style = "fill",
                    color = "success",
                    size = "md"
                  )
                )
              ),
              column(
                width = 4,
                div(
                  align = 'center',
                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
                  
                  actionBttn(
                    inputId = "delete_scenerio",
                    "Delete Scenerio",
                    style = "fill",
                    color = "danger",
                    size = "md"
                  )
                )
              )
#              column(
#                width = 3,
#                div(
#                  align = 'center',
#                  style = "font-size: 16px; padding-top: 0px; margin-top:0px; margin-bottom: 0px",
#                  numericInput(inputId = "num_years", label = "Number of Years",value = 25, min = 9, max = 50)
#                ))
              ),
              fluidRow(
                column(
                  width = 12,
                  div(style = 'overflow-x: scroll',
                      DT::dataTableOutput(
                        outputId = "scenerio",
                        width = "100%",
                        height = "auto"
                      )))
              )
            )
    )
  )
)
ui <- dashboardPagePlus(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  state_abbr <- readRDS("state_abbr.rds")
  cips <- readRDS("CIPS.rds")
  alt_title <- readRDS("AltTitle.rds")
  backbone <- readRDS("Backbone.rds")
  school <- readRDS("Schools.rds")
  school <- school %>% select("UNITID", "INSTNM", "STABBR", "State", "TOTALCOST")
  occupation <- readRDS("Occupations.rds") 
  occupation <- occupation %>% select("OCCNAME", "OCCCODE", "X17p")
  ent_degree <- readRDS("Ent_Degree.rds")
  aw_degree <- readRDS("AW_Degree.rds")
  
  scenerio_table <- readRDS("Scenerio.rds")
  b_placeholder_table <- readRDS("Scenerio.rds")
  
  school_list_filter <- c(character())
  degree_list_filter <- c(character())
  occupation_list_filter <- c(character())
  curriculum_list_filter <- c(character())
  
  school_filter <- c(character())
  degree_filter <- c(character())
  occupation_filter <- c(character())
  curriculum_filter <- c(character())
  
  observe({
    #req(aw_degree)
    updateSelectInput(
      session,
      inputId = "residence",
      label = "State of Residence",
      choices = c(Any = '', sort(state_abbr$State)),
      selected = ''
    )
    updateSelectInput(
      session,
        inputId = "state_filter",
        label = "",
        choices = c(Any = '', sort(state_abbr$State)),
      selected = ''
    )
    updateSelectInput(
      session,
      inputId = "required_degree",
      label = "Required Degree",
      choices = c(Any = '', sort(ent_degree$Entry_Degree)),
      selected = ''
    )
    updateSliderInput(
      session,
      inputId = "min_start_salary",
      label = "Desired Income Level:",
      value = min(sort(unique(occupation$X17p))),
      min = min(sort(unique(occupation$X17p))),
      step = 1000,
      max = max(sort(unique(occupation$X17p)))
    )
    updateSliderInput(
      session,
      inputId = "annual_ed_cost",
      label = "Desired Tuition Level",
      value = max(sort(unique(school$TOTALCOST))),
      min = min(sort(unique(school$TOTALCOST))),
      step = 1000,
      max = max(sort(unique(school$TOTALCOST)))
    )
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
    if(!is.null(input$required_degree) & input$required_degree !='') {
      required_temp1 <- filter(ent_degree, Entry_Degree %in% input$required_degree) %>% select(Entry_Code)
      backbone_temp <- filter(backbone_temp, Entry_Code %in% required_temp1[,1])
    }
    if( input$state_filter !='') {
      state_temp1 <- filter(school, State %in% input$state_filter) %>% select(UNITID)
      backbone_temp <- filter(backbone_temp, UNITID %in% state_temp1[,1])
    }
    if(!is.null(input$occupation_filtered) & input$occupation_filtered !='') {
      occupation_temp <- filter(occupation, OCCNAME %in% input$occupation_filtered) %>% select(OCCCODE)
      backbone_temp <- filter(backbone_temp, OCCCODE %in% occupation_temp)
    }
    if(!is.null(input$school_filtered) & input$school_filtered !='') {
      school_temp <- filter(school, INSTNM %in% input$school_filtered) %>% select(UNITID)
      backbone_temp <- filter(backbone_temp, UNITID %in% school_temp)
    }
    if(!is.null(input$curriculum_filtered) & input$curriculum_filtered !='') {
      curriculum_temp <- filter(cips, CIPNAME %in% input$curriculum_filtered) %>% select(CIPCODE)
      backbone_temp <- filter(backbone_temp, CIPCODE %in% curriculum_temp)                      
    }
    if(!is.null(input$degree_filtered) & input$degree_filtered !='') {
      degree_temp <- filter(aw_degree, Degree_Name %in% input$degree_filtered) %>% select(AWLEVEL)
      backbone_temp <- filter(backbone_temp, AWLEVEL %in% degree_temp)
    }
    if(input$min_start_salary > 0){
      start_salary_temp <- filter(occupation, X17p >= input$min_start_salary ) %>% select(OCCCODE)
      backbone_temp <- filter(backbone_temp, OCCCODE %in% (start_salary_temp[[1]]))
    }
    if(input$annual_ed_cost >= 0){
      annual_ed_temp <- filter(school, TOTALCOST <= input$annual_ed_cost ) %>% select(UNITID)
      backbone_temp <- filter(backbone_temp, UNITID %in% annual_ed_temp[,1])
    }
    backbone_temp <- left_join(backbone_temp, school, by = "UNITID")
    backbone_temp <- left_join(backbone_temp, cips, by = "CIPCODE")
    backbone_temp <- left_join(backbone_temp, aw_degree, by = "AWLEVEL")
    backbone_temp <- left_join(backbone_temp, ent_degree, by = "Entry_Code")
    backbone_temp <- left_join(backbone_temp, occupation, by = "OCCCODE")
    backbone_temp <- backbone_temp %>% select( "INSTNM", "STABBR","CIPNAME", "Degree_Name", "OCCNAME",
                                                "Entry_Degree", "X17p", "TOTALCOST")
    school_filter <<- unique(backbone_temp$INSTNM)
    degree_filter <<- unique(backbone_temp$Degree_Name)
    occupation_filter <<- unique(backbone_temp$OCCNAME)
    curriculum_filter <<- unique(backbone_temp$CIPNAME)
    if(identical(school_list_filter, character(0))){
      updateSelectInput(session, inputId = "school_filtered", label = "",
                        choices = isolate(c(All = '', sort(school_filter))), selected = '')
    }
    if(identical(occupation_list_filter, character(0))){
      updateSelectInput(session, inputId = "occupation_filtered", label = "",
                        choices = isolate(c(All = '', sort(occupation_filter))), selected = '')
    }
    if(identical(curriculum_list_filter, character(0))){
      updateSelectInput(session, inputId = "curriculum_filtered", label = "",
                        choices = isolate(c(All = '', sort(curriculum_filter))), selected = '')
    }
    if(identical(degree_list_filter, character(0))){
      updateSelectInput(session, inputId = "degree_filtered", label = "",
                        choices = isolate(c(All = '', sort(degree_filter))), selected = '')
    }

    backbone_temp <- backbone_temp %>% rename("Institution<br>Name" = "INSTNM", "State<br>Abbrv" = "STABBR",
                                                "Curriculum<br>Name" = "CIPNAME", "Degree<br>Name" = "Degree_Name", 
                                                "Occupation<br>Name" = "OCCNAME", "Start<br>Wage" = "X17p",
                                                "Entry_Level<br>Degree" = "Entry_Degree")
 
  })
 
  
observeEvent(input$create_table, {  
  output$table <- renderDataTable({
    DT::datatable(
      data = table_var(),
      escape = FALSE,
      rownames = FALSE,
      options = list(
        saveState = TRUE,
        filter = FALSE,
        autoWidth = TRUE,
        lengthMenu = c(12, 24, 36)
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
  
#Add button 
observeEvent(input$add_scenerio, {
  req(input$table_rows_selected)
  scenerio_to_add <- table_var()[input$table_rows_selected,]
  scenerio_table <<- rbind(scenerio_table, scenerio_to_add)
  row.names(scenerio_table) <<- 1:nrow(scenerio_table)
})
#Delete Button  
observeEvent(input$delete_scenerio, {
  if(length(input$scenerio_rows_selected)>= 1){
    scenerio_table <<- scenerio_table[-input$scenerio_rows_selected,]
    if(nrow(scenerio_table)>= 1){
      row.names(scenerio_table) <<- 1:nrow(scenerio_table)}
  }
})
#Save scenerio
observeEvent(input$save_scenerio,{
  filename <- ("test.rds")
  saveRDS(scenerio_table, filename)
  #  drop_upload(filename, path = "responses")
  shinyalert(title = "Saved!", type = "success")
})
#Load scenerio
observeEvent(input$load_scenerio, {
  filename2 <- ("test.rds")
  if(file.exists(filename2) == FALSE) {
    shinyalert(title = "File Not Found", type = "error")
  } else {
    #      filename2 <- paste0("responses", filename)
    #    drop_download(filename2, overwrite = TRUE)
    filename <- ("test.rds")
    scenerio_table <<- readRDS(filename)
    shinyalert(title = "Loaded", type = "success")
  } 
})
observe ( {
  req(input$add_scenerio | input$delete_scenerio | input$load_scenerio)
  output$scenerio <- renderDataTable({
    DT::datatable(
      data = scenerio_table,
      escape = FALSE,
      rownames = TRUE,
      options = list(
        saveState = TRUE,
        filter = FALSE,
        autoWidth = TRUE,
        lengthMenu = c(10, 24, 36)
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
                          choices = isolate(c(None = '', school_list)),
                          selected = '')
      }
    })
    observeEvent(input$school_delete, {
      if(!is.null(input$school_last)) {
        school_list <<- school_list[!school_list %in% input$school_last]
        updateSelectInput(session,
                          inputId = "school_middle",
                          label = "",
                          choices = isolate(c(None = '', school_list)))
        updateSelectInput(session,
                          inputId = "school_last",
                          label = "",
                          choices = isolate(c(None = '', school_list)),
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
                          label = "",
                          choices = isolate(c(None = '', degree_list)),
                          selected = '')
      }
    })
    observeEvent(input$degree_delete, {
      if(!is.null(input$degree_last)) {
        degree_list <<- degree_list[!degree_list %in% input$degree_last]
        updateSelectInput(session,
                          inputId = "degree_middle",
                          label = "",
                          choices = isolate(c(None = '', degree_list)))
        updateSelectInput(session,
                          inputId = "degree_last",
                          label = "",
                          choices = isolate(c(None = '', degree_list)),
                          selected = '')
      }
    })
    observeEvent(input$degree_filtered,{
      req(input$degree_filtered)
      if(identical(degree_list_filter, character(0))){
        degree_list <<- rbind(degree_list, input$degree_filtered)
        degree_list <<- unique(degree_list)
        degree_list_filter <<- rbind(degree_list_filter, input$degree_filtered)
        updateSelectInput(session,
                          inputId = "degree_filtered",
                          label = "",
                          choices = c(All = '', degree_list_filter),
                          selected = degree_list_filter[,1])
        updateSelectInput(session,
                          inputId = "degree_middle",
                          label = "",
                          choices = isolate(c(None = '', degree_list))
        )
        updateSelectInput(session,
                          inputId = "degree_last",
                          label = "",
                          choices = isolate(c(None = '', degree_list))
        )
      }
    })
    observe({
      if(input$degree_filtered == ""){
        degree_list_filter <<- character(0)
      }
    })
    observeEvent(input$degree_last, {
      req(input$degree_last)
      degree_list_filter <<- character(0)
      degree_list_filter <<- rbind(degree_list_filter, input$degree_last)
      updateSelectInput(session,
                        inputId = "degree_filtered",
                        label = "",
                        choices = c(All = '', degree_list_filter),
                        selected = degree_list_filter[,1])  
    })
    observeEvent(input$occupation_filtered,{
      req(input$occupation_filtered)
      if(identical(occupation_list_filter, character(0))){
        occupation_list <<- rbind(occupation_list, input$occupation_filtered)
        occupation_list <<- unique(occupation_list)      
        occupation_list_filter <<- rbind(occupation_list_filter, input$occupation_filtered)
        updateSelectInput(session,
                          inputId = "occupation_filtered",
                          label = "",
                          choices = c(All = '', occupation_list_filter),
                          selected = occupation_list_filter[,1])
        updateSelectInput(session,
                          inputId = "occupation_middle",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
        updateSelectInput(session,
                          inputId = "occupation_last",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
      }
    })
    observe({
      if(input$occupation_filtered == ""){
        occupation_list_filter <<- character(0)
      }
    })
    observeEvent(input$occupation_last, {
      req(input$occupation_last)
      occupation_list_filter <<- character(0)
      occupation_list_filter <<- rbind(occupation_list_filter, input$occupation_last)
      updateSelectInput(session,
                        inputId = "occupation_filtered",
                        label = "",
                        choices = c(All = '', occupation_list_filter),
                        selected = occupation_list_filter[,1])  
    })
    
    
    
    observeEvent(input$school_filtered,{
      req(input$school_filtered)
      if(identical(school_list_filter, character(0))){
        school_list <<- rbind(school_list, input$school_filtered)
        school_list <<- unique(school_list)
        school_list_filter <<- rbind(school_list_filter, input$school_filtered)
        updateSelectInput(session,
                          inputId = "school_filtered",
                          label = "",
                          choices = c(All = '', school_list_filter),
                          selected = school_list_filter[,1])
        updateSelectInput(session,
                          inputId = "school_middle",
                          label = "",
                          choices = isolate(c(None = '', school_list)))
        updateSelectInput(session,
                          inputId = "school_last",
                          label = "",
                          choices = isolate(c(None = '', school_list)))
      }
    })
    observe({
      if(input$school_filtered == ''){
        school_list_filter <<- character(0)
      }
    })
    observeEvent(input$school_last, {
      req(input$school_last)
      school_list_filter <<- character(0)
      school_list_filter <<- rbind(school_list_filter, input$school_last)
      updateSelectInput(session,
                        inputId = "school_filtered",
                        label = "",
                        choices = c(All = '', school_list_filter),
                        selected = school_list_filter[,1])  
    })
    
    observeEvent(input$curriculum_filtered,{
      req(input$curriculum_filtered)
      if(identical(curriculum_list_filter, character(0))){
      curriculum_list <<- rbind(curriculum_list, input$curriculum_filtered)
      curriculum_list <<- unique(curriculum_list)
      curriculum_list_filter <<- rbind(curriculum_list_filter, input$curriculum_filtered)
        updateSelectInput(session,
                          inputId = "curriculum_filtered",
                          label = "",
                          choices = c(All = '', curriculum_list_filter),
                          selected = curriculum_list_filter[,1])
        updateSelectInput(session,
                          inputId = "curriculum_middle",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
        updateSelectInput(session,
                          inputId = "curriculum_last",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
      }
    })
    observe({
      if(input$curriculum_filtered == ''){
        curriculum_list_filter <<- character(0)
      }
    })
    observeEvent(input$curriculum_last, {
      req(input$curriculum_last)
      curriculum_list_filter <<- character(0)
      curriculum_list_filter <<- rbind(curriculum_list_filter, input$curriculum_last)
      updateSelectInput(session,
                        inputId = "curriculum_filtered",
                        label = "",
                        choices = c(All = '', curriculum_list_filter),
                        selected = curriculum_list_filter[,1])  
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
                          label = "",
                          choices = isolate(c(None = '', occupation_list)),
                          selected = '')
      }
    })
    observeEvent(input$occupation_delete, {
      if(!is.null(input$occupation_last)) {
        occupation_list <<- occupation_list[!occupation_list %in% input$occupation_last]
        updateSelectInput(session,
                          inputId = "occupation_middle",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)))
        updateSelectInput(session,
                          inputId = "occupation_last",
                          label = "",
                          choices = isolate(c(None = '', occupation_list)),
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
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)),
                          selected = '')
      }
    })
    observeEvent(input$curriculum_delete, {
      if(!is.null(input$curriculum_last)) {
        curriculum_list <<- curriculum_list[!curriculum_list %in% input$curriculum_last]
        updateSelectInput(session,
                          inputId = "curriculum_middle",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)))
        updateSelectInput(session,
                          inputId = "curriculum_last",
                          label = "",
                          choices = isolate(c(None = '', curriculum_list)),
                          selected = '')
      }
    })
    observeEvent(input$min_max_button,{
      js$collapse("box1")
      js$collapse("box2")
      js$collapse("box3")
      js$collapse("box4")
    })
}

shinyApp(ui, server)