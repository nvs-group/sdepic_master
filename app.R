## app.R ##
##Self Directed EPIC-Fork 2/17/2020
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

school_list_filter <- c(character())
degree_list_filter <- c(character())
occupation_list_filter <- c(character())
curriculum_list_filter <- c(character())

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
                    #  tags$script(HTML("$('body').addClass('fixed');")),
                   # tags$script (src="http://code.jquery.com/jquery-1.7.2.min.js"),
                   # tags$script (src="http://code.jquery.com/ui/1.8.21/jquery-ui.min.js"),
                      tags$script(src = "jquery.ui.touch-punch.js"),
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
                  h2("Let's Get Started"),
                  h2("Step 2: Fill in preferences if you have them (3 min)")),
              fluidRow(
                
                box(width = 2,
                    selectInput(
                      inputId = "residence",
                      label = "State of Residence",
                      choices = c(None = '', (sort(state_abbr$State)))
                    )), 
                box(
                  width = 2,
                  numericInput(
                    inputId = "min_start_salary",
                    label = "Min Starting Salary",
                    value = 0,
                    min = 0
                  )
                ), 
                box(
                  width = 2,
                  numericInput(
                    inputId = "annual_ed_cost",
                    label = "Max Annual Ed Cost",
                    value = 50000,
                    min = 0
                  )
                ),
                box(
                  width = 2,
                  selectInput(inputId = "required_degree",
                              label = "Required Degree",
                              choices = c(None = '', sort(ent_degree$Entry_Degree)))
                )
              ),
              div(align = 'center', style = "font-size: 16px; padding-top: -10px; margin-top:0em", 
                  h2("Arrange the items below in order of"),
                  h2("importance and certainty (3 min)")
                  ),
              fluidRow(
                jqui_sortable(div(id = 'choices',
                                  column(
                                    id = '1',
                                    width = 3,
                                    boxPlus(
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
                                            choices = isolate(occupation$OCCNAME),
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
                                      )
                                    ),
                                  column(
                                    id = '2',
                                    width = 3,
                                    boxPlus(
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
                                            choices = isolate(cips$CIPNAME),
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
                                    )
                                  )
                                  , 
                                  column(
                                    id = '3',
                                    width = 3,
                                    boxPlus(
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
                                            choices = isolate(sort(school$INSTNM)),
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
                                    )
                                  ), 
                                  column(
                                    id = '4',
                                    width = 3,
                                    boxPlus(
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
                                            choices = isolate(aw_degree$Degree_Name),
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
                                    )
                                    
                                  )
                                  
                )
                )
              ),
              
              fluidRow(column(width = 12,
                              
                              div(
                                style = 'overflow-x: scroll', DT::dataTableOutput(outputId = "table")
                              )))
            )
    )
  )
)
ui <- dashboardPagePlus(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {

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
    
    filtered_table <- filtered_table %>% rename("Institution<br>Name" = "INSTNM",
                                                "Curriculum<br>Name" = "CIPNAME", "Degree<br>Name" = "Degree_Name", 
                                                "Occupation<br>Name" = "OCCNAME", "Median<br>Wage" = "MedWage",
                                                "Entry_Level<br>Degree" = "Entry_Degree")
  })
 
  
  observe({
    output$table <- renderDataTable({
      DT::datatable(
        data = table_var(),
        escape = FALSE,
        options = list(
          filter = FALSE,
          pageLength = 5,
          autoWidth = TRUE,
          lengthMenu = c(5, 10, 15, 30)
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
}

shinyApp(ui, server)