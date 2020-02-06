## app.R ##
##Self Directed EPIC 2/04/2020
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

epic_columns <- c( "INSTNM", "CIPNAME", "Degree_Name", "OCCNAME",
                  "Entry_Degree", "MedWage")

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
body <- dashboardBody(HTML('<meta name="viewport" content="width=1920">'),
  tabItems(
    tabItem(tabName = "welcome", useShinyalert(), useShinyjs(),
            fluidPage(
              box(width = 12, 
                  h1("Welcome to your Personel EPIC Portal"), align = 'center'),
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
             
                  h3("Let's Get Started"), 
                  HTML("Total estimated time to complete is 7 minutes.<br>Step 1: Indicate how important and settled these six preferences are in your mind (1 min)<br>
                  Low <---------------------------------- IMPORTANCE TO YOU? ----------------------------------> High")
                  ),
              column(width = 2,
                     box(width = 12,
                         h3("Decided"),
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
              verbatimTextOutput(outputId = "school_loc" ),
              jqui_droppable(
                column(width = 8, id = "box_2", style = "background-color:#2442aa; height: 475px")),
              
                column(width = 2,
                    h5("Drag each preference and place it in the space to the left based on how settled and how important it is to you.")
                ),
              column(width =2, tags$script(src = "jquery.ui.touch-punch.js"),
              jqui_draggable(
                div(id = "school_choice",'School',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color:#79BBF2; font-size: 150%') ,
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "occupation_choice", 'Occupation',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: red; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "curriculum_choice", 'Curriculum',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: green; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "degree_choice", 'Degree',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: gray; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "tuition_choice", 'Tuition', 
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: yellow; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              ),
              jqui_draggable(
                div(id = "salary_choice", 'Salary',
                    style = 'border-radius:10px; text-align: center;width:110px; height:30px;
                    background-color: orange; font-size: 150%'),
                options = list(containment = "#box_2", scroll = FALSE)
              )
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
              box(width = 12, 
                  h3("Step 2: Fill in preferences if you have them (3 min)"), align = 'center'),
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
                            choices = unique(sort(school$STABBR)))
                ),
                box(width = 2,
                    numericInput(inputId = "min_start_salary", label = "Min Starting Salary",
                                 value = 0, min = 0)
                    ),
                box(width = 2,
                    numericInput(inputId = "annual_ed_cost", label = "Max Annual Ed Cost",
                                 value = 50000, min = 0)
                    ),
                box(width = 3,
                    textInput(inputId = "alt_temp", label = "Alternate Title")),
                actionBttn(inputId = "reorder", label = "Reorder")
              ),
              fluidRow(
                jqui_sortable(div(id = 'choices',
                                  column(id = '1',
                                    width = 3,
                                    box(
                                      width = 12,
                                      height = '180px',
                                      selectInput(
                                        inputId = "occupation_epic",
                                        label = "Occupation",
                                        choices = occupation$OCCNAME,
                                        multiple = TRUE
                                      )
                                    ),
                                    box(width = 12,
                                        uiOutput(outputId = "occupation_choice"))
                                  ), 
                                  column(id = '2',
                                    width = 3,
                                         box(
                                           width = 12,
                                           height = '180px',
                                           selectInput(
                                             inputId = "school_epic",
                                             label = "School",
                                             choices = school$INSTNM,
                                             multiple = TRUE
                                           )
                                         ),
                                         box(width = 12,
                                             uiOutput(outputId = "school_choice"))), 
                                  column(id = '3',
                                    width = 3,
                                         box(
                                           width = 12,
                                           height = '180px',
                                           selectInput(
                                             inputId = "curriculum_epic",
                                             label = "Curriculum",
                                             choices = cips$CIPNAME,
                                             multiple = TRUE
                                           )
                                         ),
                                         box(width = 12,
                                             uiOutput(outputId = "curriculum_choice"))), 
                                  column(id = '4',
                                    width = 3,
                                         box(
                                           width = 12,
                                           height = '180px',
                                           selectInput(
                                             inputId = "degree_epic",
                                             label = "Degree",
                                             choices = unique(aw_degree$Degree_Name),
                                             multiple = TRUE
                                           )
                                         ),
                                         box(width = 12,
                                             uiOutput(outputId = "degree_choice"))),
                )
                )
              )
            )
            
            
            
    ), 
    tabItem(tabName = "page3",
            fluidPage(
              box(width = 12, 
                  h3("Step 3: Create and Save Education and Career Scenarios (2 min per scenario)"), align = 'center'),
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
                       box(
                         width = 12,
                         uiOutput(outputId = "degree_pref")
                       )),
                column(width = 3,
                       box(
                         width = 12,
                         uiOutput(outputId = "school_pref")
                       )),
                column(width = 3,
                       box(
                         width = 12,
                         uiOutput(outputId = "curriculum_pref")
                       )),
                column(width = 3,
                       box(
                         width = 12,
                         uiOutput(outputId = "occupation_pref")
                       )),
              ))), 
              fluidRow(
                column(width = 2,
                              box(
                                width = 12,
                                actionButton(
                                  inputId = "make_table",
                                  label = "Show All",
                                  width = '100%'
                                )
                              )),
                       column(width = 8,
                              div(
                                style = 'overflow-x: scroll', DT::dataTableOutput(outputId = "table")
                              )))
            )
    ), 
    tabItem(tabName = "page4",
            fluidPage(
              box(width = 12, 
                  h3("Step 4: Select up to 3 saved scenarios to compare (1 min)"), align = 'center'),
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
              box(width = 12, 
                  h3("Step 5: Compare Scenarios, Select Favorites, and Request a Report (1 min)"), align = 'center'),
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
  
  begin_order <- reactive({
    input$choices_order$id
  })
 
  output$occupation_choice <- renderUI({
    tagList(
      selectInput(
        inputId = "occ_text",
        label = "",
        choices = input$occupation_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$occupation_pref <- renderUI({
    tagList(
      selectInput(
        inputId = "occupation_preference",
        label = "Occupation",
        choices = c(All = '', input$occupation_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$school_choice <- renderUI({
    tagList(
      selectInput(
        inputId = "school_text",
        label = "",
        choices = input$school_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$school_pref <- renderUI({
    tagList(
      selectInput(
        inputId = "school_preference",
        label = "School",
        choices = c(All = '', input$school_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$curriculum_choice <- renderUI({
    tagList(
      selectInput(
        inputId = "cur_text",
        label = "",
        choices = input$curriculum_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$curriculum_pref <- renderUI({
    tagList(
      selectInput(
        inputId = "curriculum_preference",
        label = "Curriculum",
        choices = c(All = '', input$curriculum_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$degree_choice <- renderUI({
    tagList(
      selectInput(
        inputId = "deg_text",
        label = "",
        choices = input$degree_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  output$degree_pref <- renderUI({
    tagList(
      selectInput(
        inputId = "degree_preference",
        label = "Degree",
        choices = c(All = '', input$degree_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    )
  })
  temp_table <- reactive({
    mast_temp <- alt_title
    if(!is.null(input$alt_temp)){
      mast_temp <- filter(mast_temp , grepl(input$alt_temp, mast_temp$AltName, ignore.case = TRUE))
    }
  })
  observe({
    if(!is.null(input$alt_temp)){
    occ_filter <- occupation %>% filter(occupation$OCCCODE %in% temp_table()$OCCCODE) 
    updateSelectInput(session, inputId = "occupation_epic", label = "Occupation",
                      choices = sort(unique(occ_filter$OCCNAME)))
    }
  })
  
  table_var <- reactive ({
    backbone_temp <- backbone
    if(!is.null(input$occupation_preference) & input$occupation_preference !='') {
      occupation_temp <- filter(occupation, OCCNAME %in% input$occupation_preference) %>% select(OCCCODE)
      backbone_temp <- filter(backbone_temp, OCCCODE %in% occupation_temp)
    }
    if(!is.null(input$school_preference) & input$school_preference !='') {
      school_temp <- filter(school, INSTNM %in% input$school_preference) %>% select(UNITID)
      backbone_temp <- filter(backbone_temp, UNITID %in% school_temp)
    }
    if(!is.null(input$curriculum_preference) & input$curriculum_preference !='') {
      curriculum_temp <- filter(cips, CIPNAME %in% input$curriculum_preference) %>% select(CIPCODE)
      backbone_temp <- filter(backbone_temp, CIPCODE %in% curriculum_temp)                      
    }
    if(!is.null(input$degree_preference) & input$degree_preference !='') {
      degree_temp <- filter(aw_degree, Degree_Name %in% input$degree_preference) %>% select(AWLEVEL)
      backbone_temp <- filter(backbone_temp, AWLEVEL %in% degree_temp)
    }
    filtered_table <- left_join(backbone_temp, school, by = "UNITID")
    filtered_table <- left_join(filtered_table, cips, by = "CIPCODE")
    filtered_table <- left_join(filtered_table, aw_degree, by = "AWLEVEL")
    filtered_table <- left_join(filtered_table, ent_degree, by = "Entry_Code")
    filtered_table <- left_join(filtered_table, occupation, by = "OCCCODE")
    filtered_table <- filtered_table %>% select(all_of(epic_columns))
    filtered_table <- filtered_table %>% rename("Institution<br>Name" = "INSTNM",
                                                "Curriculum<br>Name" = "CIPNAME", "Degree<br>Name" = "Degree_Name", 
                                                "Occupation<br>Name" = "OCCNAME", "Median<br>Wage" = "MedWage",
                                                "Entry_Level<br>Degree" = "Entry_Degree")
  })
  
  observeEvent(input$make_table, {
    output$table <- renderDataTable({
      DT::datatable(
        data = table_var(),
        escape = FALSE,
        options = list(
          filter = FALSE,
          pageLength = 5,
          autoWidth = TRUE,
          #columnDefs = list(list(width = '180px', targets = c(1,2,3,4,5))),
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
    output$school_loc <- renderPrint({ 
      school <- as.numeric(input$school_choice_position$top)
      school <- round(((-school + 370) / 43.5), 0)

      print(school)
      print(begin_order())
      })
}

shinyApp(ui, server)