#### Preamble ####

# This is where we will load libraries, define variable options, and define
# useful functions

##### Load packages ####

library(shiny)
library(shinythemes) # To select a theme
library(DT) # To display an interactive dataset table
library(tidyverse) # To perform data manipulation tasks
library(RMariaDB) # To communicate with the MySQL database
library(pool) # To manage connections with the MySQL database
library(shinyalert) # To display informative dialogue boxes
library(shinyjs) # To add Javascript to the app
library(ggplot2) # To visualize your data
library(leaflet) # To create interactive maps
library(uuid) # To create unique identifiers for each entry
library(shinyhelper) # To add helper notes
library(lubridate) # To work with dates

##### Define user credentials ####

# Define a list of valid usernames and passwords

valid_users <- c("user1", "user2", "user3", "user4")
valid_passwords <- c("password1", "password2", "password3", "password4")

##### Define variable options ####

# User options

users_options <- c(
  "",
  "Coder 1",
  "Coder 2",
  "Coder 3"
)

# Reliability options

reliability_options <- c(
  "Very poorly documented",
  "Poorly documented",
  "Moderately documented",
  "Well documented",
  "Very well documented"
)

##### MySQL-related commands ####

# Define the SQL connection
con <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "your_database_name",
  host = "your_host_name",
  user = "your_user_name",
  password = "your_password",
  encoding = "utf8"
)

# Define the reactiveValues object
values <- reactiveValues(dataframe = NULL, selected_rows = NULL)

# Function to retrieve data from MySQL table
getDataFromMySQL <- function() {
  query <- "SELECT * FROM tutorial_dataset"
  dbGetQuery(con, query)
}

# Function to update the MySQL table with new row
addRowToMySQL <- function(new_row) {
  query <- "INSERT INTO tutorial_dataset (id, coding_date, coder, reviewer, status, reliability, violence, year, event_date, source) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  params <- c(new_row$id, new_row$coding_date, new_row$coder, new_row$reviewer, new_row$status, new_row$reliability, new_row$violence, new_row$year, as.character(new_row$event_date), new_row$source)
  
  # Debugging aid
  print("Variable values recorded by the user:")
  print(new_row)
  print("Variable values pushed to the MySQL database:")
  print(params)
  cat("Number of variable values pushed to the MySQL database: ", length(params), "\n")
  
  poolWithTransaction(con, function(conn) {
    dbExecute(conn, query, params = params)
  })
} 

# Create initial dataframe from MySQL table
values$dataframe <- getDataFromMySQL()

# Function to update a row in the MySQL table
updateRowInMySQL <- function(updated_row) {
  query <- "UPDATE tutorial_dataset SET coding_date = ?, coder = ?, reviewer = ?,
  status = ?, reliability = ?, violence = ?, year = ?, event_date = ?, source = ? 
  WHERE id = ?"
  
  params <- c(updated_row$coding_date, updated_row$coder, updated_row$reviewer,
              updated_row$status, updated_row$reliability, updated_row$violence, 
              updated_row$year, as.character(updated_row$event_date), 
              updated_row$source, updated_row$id)
  
  # Error handling
  result <- try({
    rows_affected <- poolWithTransaction(con, function(conn) {
      dbExecute(conn, query, params = params)
    })
    print(paste("Rows affected: ", rows_affected))
  }, silent = TRUE)
  
  if (class(result) == "try-error") {
    shinyalert::shinyalert("An error occurred. Go talk to your project leader!", type = "error")
  }
  
  poolWithTransaction(con, function(conn) {
    dbExecute(conn, query, params = params)
  })
}

# Function to generate unique ID using UUID
generateUniqueID <- function() {
  uuid::UUIDgenerate()
}

# Function to add the coding date
generateDate <- function() {
  today <- format(Sys.Date(), "%Y%m%d")
  as.numeric(today)
}

#### Define UI ####

ui <- fluidPage(
  
  # This is where we will add the code for creating the UI
  
  useShinyjs(),
  hidden(textInput("row_id_hidden", "")),
  
  # Login panel
  div(
    id = "login_panel",
    textInput("username", "Username"),
    passwordInput("password", "Password"),
    actionButton("login_button", "Login")
  ),
  
  # Main app content
  shinyjs::hidden(
    div(
      id = "main_panel",
      div(id = "navbar-container",
  
  navbarPage(
    title = "A Basic Data Collection App",
    theme = shinytheme("sandstone"),
    tabPanel(
      "Let's start coding",
      sidebarLayout(
        sidebarPanel(
          h3("Welcome to our coding platform"),
          "Use the form below to start coding. Use the help buttons 
            to consult the code book for further details on specific variables and coding rules.
            Once you have filled in all relevant fields, click the ADD RECORD button 
            at the bottom of the form to save the record.",
          br(),
    
          ##### Coding form ####
          
          h3("Start coding"),
          br(),
          fluidRow(
            column(
              width = 4,
              downloadButton("download_button", "Download as CSV", class = "btn-info")
            ),
            column(
              width = 4,
              actionButton("edit_button", icon = icon("pen-to-square"), "Edit record", class = "btn-warning")
            ),
            column(
              width = 4,
              actionButton("delete_button", icon = icon("trash"), "Delete record", class = "btn-danger")
            ),
          ),
          br(),
          br(),
          fluidRow(
            column(4, selectInput(
              "coder_input", "Select coder name", choices = users_options, selected = "") %>% 
                helper(
                  type = "markdown",
                  content = "example",
                  colour = "black",
                  buttonLabel = "Got it!"
                )
            ),
            column(4, selectInput("reviewer_input", label = "Select reviewer", choices = users_options, selected = "")),
            column(4, selectInput("status_input", label = "Coding status", choices = c("Not finished", "Finished"), selected = "Not finished"))
          ),
          radioButtons("reliability_input", "Indicate how well-documented the event is", choiceNames = reliability_options, choiceValues = c(0, 1, 2, 3, 4), selected = 2),
          checkboxInput("violence_input", "Was the event violent?", value = F),
          numericInput("year_input", "Select a year", 1946, min = 1946, max = 2022),
          dateInput("event_date_input", "Select the event date", value = "2023-01-01", min = "1946-01-01", max = "2023-12-31"),
          textAreaInput(
            "source_input",
            "Enter the full source text and reference(s) used to code the event",
            placeholder = "Sebastian van Baalen & Abel Gbala (2024) Patterns of electoral violence during Côte d'Ivoire's third-term crisis. African Affairs 122(488).",
            width = "100%", height = "100px"
          ),
          
          actionButton("add_button", icon = icon("plus"), "Add record", class = "btn-success")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Our dataset",
              h3("Show our dataset"),
              br(),
              tags$hr(),
              br(),
              DTOutput("data_table")
            ),
            ##### Verification tools ####
            tabPanel(
              "Verification tools",
              h3("Verification tools"),
              br(),
              fluidRow(
                column(4, h4("Non-unique reviewer"), br(), tableOutput("nonunique_reviewer"))
              )
            )
          )
        )
      )
    )
  )
)))
)

#### Define server logic ####

server <- function(input, output, session) {
  
  # This is where we will add the code for saving, editing, and deleting dataset rows,
  # as well as for other functionalities
  
  #### Login panel ####
  
  # Create a reactive value to track login status
  logged_in <- reactiveVal(FALSE)
  
  # Event handler for login button
  observeEvent(input$login_button, {
    # Check if entered username and password match the valid credentials
    if (input$username %in% valid_users && input$password %in% valid_passwords) {
      # Set logged_in to TRUE if credentials are valid
      logged_in(TRUE)
      
      # Hide the login panel and show the main panel
      shinyjs::hide("login_panel")
      shinyjs::show("main_panel")
    } else {
      # Show an error message if credentials are invalid
      showModal(
        modalDialog(
          title = "Login Error",
          "Invalid username or password. Please try again.",
          easyClose = TRUE
        )
      )
    }
  })
  
  #### Set help file directory ####
  
  observe_helpers(help_dir = "helpfiles")
  
  #### Create and display the dataset ####
  
  output$data_table <- renderDT({
    req(values$dataframe)
    
    df_ordered <- values$dataframe
    
    datatable(
      df_ordered,
      options = list(
        dom = 'tfip',
        pageLength = 30,
        autoWidth = TRUE,
        searching = TRUE,
        order = list(list(2, 'desc')),
        scrollX = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {
              if (type === 'display' && typeof data === 'string') {
                if (data.length > 20) {
                  return data.substr(0, 20) + '...';  // Truncate long strings
                }
                return data;  // Return short strings as-is
              }
              return data;  // Return other types (e.g., numbers, dates) as-is
            }"
            )
          )
        )
      ),
      rownames = FALSE
    )
  })
  
  #### Add a new row to the data table ####
  
  observeEvent(input$add_button, {
    
    # Create the new_row object from user inputs
    new_row <- data.frame(
      id = generateUniqueID(),
      coding_date = generateDate(),
      coder = input$coder_input,
      reviewer = input$reviewer_input,
      status = input$status_input,
      reliability = as.integer(input$reliability_input),
      violence = input$violence_input,
      year = input$year_input,
      event_date = input$event_date_input,
      source = input$source_input
    )
    
    # Add row to dataset
    addRowToMySQL(new_row)
    
    # Refresh the dataframe by re-reading data from the MySQL database
    values$dataframe <- getDataFromMySQL()
    
    # Clear input values
    updateSelectInput(session, "coder_input", selected = NULL)
    updateSelectInput(session, "reviewer_input", selected = NULL)
    updateSelectInput(session, "status_input", selected = "Not finished")
    updateRadioButtons(session, "reliability_input", selected = 2)
    updateCheckboxInput(session, "violence_input", value = FALSE)
    updateNumericInput(session, "year_input", value = 1946)
    updateDateInput(session, "event_date_input", value = "2023-01-01")
    updateTextInput(session, "source_input", value = "")
    
    # Open alert box
    shinyalert(title = "Boycott record successfully added!", type = "success")
  })
  
  #### Edit a row in the data table ####
  
  observeEvent(input$edit_button, {
    selected_rows <- input$data_table_rows_selected
    
    # Debugging aid
    print(paste("Selected row when clicking edit button: ", values$dataframe[selected_rows, "id"]))
    
    # Error handling to prevent selection of multiple rows
    if (length(selected_rows) != 1) {
      showModal(
        modalDialog(
          title = "Warning",
          "Please select only one row to edit.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } else {
      row_id <- values$dataframe[selected_rows, "id"]
      values$selected_rows <- selected_rows
      selected_row <- values$dataframe[selected_rows, ]
      
      # Pre-fill input controls with data from the selected row
      updateSelectInput(session, "coder_input_edit", selected = selected_row$coder)
      updateSelectInput(session, "reviewer_input_edit", selected = selected_row$reviewer)
      updateSelectInput(session, "status_input_edit", selected = selected_row$status)
      updateRadioButtons(session, "reliability_input_edit", selected = selected_row$reliability)
      updateCheckboxInput(session, "violence_input_edit", value = selected_row$violence)
      updateNumericInput(session, "year_input_edit", value = selected_row$year)
      updateDateInput(session, "event_date_input_edit", value = as.Date(selected_row$event_date))
      updateTextAreaInput(session, "source_input_edit", value = selected_row$source)
 
      # Show modal box for editing
      showModal(
        modalDialog(
          title = "Edit record",
          size = "l",
          fluidPage(
            fluidRow(
              column(6, selectInput("coder_input_edit", "Select coder name", choices = users_options)),
              column(6, selectInput("reviewer_input_edit", "Select reviewer", choices = users_options)),
              column(6, selectInput("status_input_edit", label = "Coding status", choices = c("Not finished", "Finished"))),
              column(6, radioButtons("reliability_input_edit", "Indicate how well-documented the event is", choiceNames = reliability_options, choiceValues = c(0, 1, 2, 3, 4), selected = 2)),              column(6, checkboxInput("violence_input_edit", "Was the event violent?", value = FALSE)),
              column(6, numericInput("year_input_edit", "Select a year", 1946, min = 1946, max = 2022)),
              column(12, dateInput("event_date_input_edit", "Select the event date", value = "2023-01-01", min = "1946-01-01", max = "2023-12-31")),
              column(12, textAreaInput(
                "source_input_edit",
                "Enter the source",
                placeholder = "Sebastian van Baalen & Abel Gbala (2024)...",
                width = "100%", height = "100px"
              )),
              column(12, actionButton("save_edit_button", "Save edits", class = "btn-success"))
            )
          ),
          footer = modalButton("Dismiss")
        )
      )
    }
  })

  #### Save  edits ####
  
  observeEvent(input$save_edit_button, {
    
    # Retrieve the row ID of the record being edited
    selected_rows <- values$selected_rows
    row_id <- values$dataframe[selected_rows, "id"]
    
    # Debugging aid
    print(paste("Selected row when saving edit: ", row_id))
    
    if (!is.null(row_id)) {
      # Create an updated_row object with the edited data
      updated_row <- data.frame(
        id = row_id,
        coding_date = values$dataframe[values$dataframe$id == row_id, "coding_date"],
        coder = input$coder_input_edit,
        reviewer = input$reviewer_input_edit,
        status = input$status_input_edit,
        reliability = as.integer(input$reliability_input_edit),
        violence = as.integer(input$violence_input_edit),
        year = input$year_input_edit,
        event_date = as.Date(input$event_date_input_edit),
        source = input$source_input
      )
      
      # Debugging aid
      print("Variable values recorded by the user in the edit modal:")
      print(updated_row)
      
      # Update the database with the edited row
      updateRowInMySQL(updated_row)
      
      # Debugging aid
      print(paste("Updated row id: ", updated_row$id))
      
      # Update only the modified row in the dataframe
      values$dataframe[values$dataframe$id == row_id, ] <- updated_row
      
      # Debugging aid
      print("Dataframe updated with new values.")
      
      # Remove the modal and show success notification
      removeModal()
      shinyalert(title = "Record successfully updated!", type = "success")
      
      # Refresh the table output
      output$data_table <- renderDT({
        req(values$dataframe)
        datatable(
          values$dataframe,
          options = list(
            dom = 'tfip',
            pageLength = 30,
            autoWidth = TRUE,
            searching = TRUE,
            order = list(list(2, 'desc')),
            scrollX = TRUE,
            columnDefs = list(
              list(
                targets = "_all",
                render = JS(
                  "function(data, type, row, meta) {
                  if (type === 'display' && typeof data === 'string') {
                    if (data.length > 20) {
                      return data.substr(0, 20) + '...';  // Truncate long strings
                    }
                    return data;  // Return short strings as-is
                  }
                  return data;  // Return other types as-is
                }"
                )
              )
            )
          ),
          rownames = FALSE
        )
      })
    } else {
      shinyalert(title = "Error: No row selected!", text = "Please try again.", type = "error")
    }
  })
  
  #### Download the data as a CSV file ####
  
  output$download_button <- downloadHandler(
    filename = function() {
      paste("Tutorial dataset ", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$dataframe, file, row.names = FALSE)
    }
  )
  
  #### Automatic quality checks ####
  
  ##### Non-unique reviewer ####
  
  output$nonunique_reviewer <- renderTable({
    req(values$dataframe)
    
    df <- values$dataframe %>%
      filter(coder == reviewer) %>% 
      select(id, coder, reviewer)
    
    return(df)
    
  })
  
  #### Close MySQL connection ####
  
  on.exit(function() {
    dbPoolClose(con)
  })
}

#### Run the application ####

shinyApp(ui = ui, server = server)
