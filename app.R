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
library(dplyr)
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

epic_columns <- c( "INSTNM", "CIPNAME", "Degree_Name", "OCCNAME",
                  "Entry_Degree", "MedWage")

page1_order <- data.frame(col1 = sample(c("school", "occupation", "degree", "curriculum")), col2 = sample(c(1,2,3,4)))


header <- dashboardHeader( title = "E.P.I.C. Planning", titleWidth = 230, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(
  tags$style(".fa-adjust {color:#E87722}"),
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
                            choices = c(None = '', (sort(state_abbr$State))))
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
                    textInput(inputId = "alt_temp", label = "Alternate Title"))
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
                              box(
                                width = 12,
                                actionButton(
                                  inputId = "make_table",
                                  label = "Begin",
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
    tagList(box(
      width = 12,
      height = '180px',
      selectInput(
        inputId = "occupation_epic",
        label = "Occupation",
        choices = isolate(occupation$OCCNAME),
        multiple = TRUE
      )
    ))
  })
  occ_temp2 <- renderUI({
    tagList(box(
      width = 12,
      selectInput(
        inputId = "occ_text",
        label = "",
        choices = input$occupation_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    ))
  })
  occ_temp3 <- renderUI({
    tagList(
      box(
        width = 12,
      selectInput(
        inputId = "occupation_preference",
        label = "Occupation",
        choices = c(All = '', input$occupation_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ))
    )
  })
  school_temp1 <- renderUI({
    tagList(box(
      width = 12,
      height = '180px',
      selectInput(
        inputId = "school_epic",
        label = "School",
        choices = isolate(school$INSTNM),
        multiple = TRUE
      )
    ))
  })
  
  school_temp2 <- renderUI({
    tagList(box(
      width = 12,
      selectInput(
        inputId = "school_text",
        label = "",
        choices = input$school_epic,
        selectize = FALSE,
        size = 5,
        width = '100%'
      )
    ))
  })
  school_temp3 <- renderUI({
    tagList(
      box(
        width = 12,
      selectInput(
        inputId = "school_preference",
        label = "School",
        choices = c(All = '', input$school_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ))
    )
  })
 curr_temp1 <- renderUI({
    tagList(
      box(
        width = 12,
        height = '180px',
        selectInput(
          inputId = "curriculum_epic",
          label = "Curriculum",
          choices = isolate(cips$CIPNAME),
          multiple = TRUE
        )
      )
    )
  })
 curr_temp2 <- renderUI({
   tagList(box(
     width = 12,
     selectInput(
       inputId = "cur_text",
       label = "",
       choices = input$curriculum_epic,
       selectize = FALSE,
       size = 5,
       width = '100%'
     )
   ))
 })
  curr_temp3 <- renderUI({
    tagList(
      box(
        width = 12,
      selectInput(
        inputId = "curriculum_preference",
        label = "Curriculum",
        choices = c(All = '', input$curriculum_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ))
    )
  })

  degree_temp1 <- renderUI({
    tagList(box(
      width = 12,
      height = '180px',
      selectInput(
        inputId = "degree_epic",
        label = "Degree",
        choices = isolate(unique(aw_degree$Degree_Name)),
        multiple = TRUE
      )
    )
 )
  })
  degree_temp2 <- renderUI({
    tagList(
    box(width = 12,
        selectInput(
          inputId = "deg_text",
          label = "",
          choices = input$degree_epic,
          selectize = FALSE,
          size = 5,
          width = '100%'
        )
    ))
  })
  degree_temp3 <- renderUI({
    tagList(
      box(
        width = 12,
      selectInput(
        inputId = "degree_preference",
        label = "Degree",
        choices = c(All = '', input$degree_epic),
        selectize = FALSE,
        size = 5,
        width = '100%'
      ))
    )
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
  
  observeEvent(input$make_table,{
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
 #   output$school_loc <- renderPrint({ 
      
 #     print(page1_list())
 #     print(first_spot())
 #     print(second_spot())
 #     print(third_spot())
 #     print(fourth_spot())
 #     })
    

}

shinyApp(ui, server)