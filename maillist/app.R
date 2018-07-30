#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(shinyjs)

# Load libraries and functions needed to create SQLite databases.
library(RSQLite)
CastData <- function(data) {
  datar <- data.frame(name = data["name"],
                      organization = data["organization"],
                      title = data["title"],
                      address = data["address"],
                      email = data["email"],
                      home_or_cell = data["home_or_cell"],
                      business = data["business"],
                      fax_lol = data["fax_lol"],
                      is_elected = as.logical(data["is_elected"]), 
                      #r_num_years = as.integer(data["r_num_years"]),
                      
                      stringsAsFactors = FALSE)
  rownames(datar) <- data["id"]
  return (datar)
}

CreateDefaultRecord <- function() {
  mydefault <- CastData(list(id = "0", 
                             name = "", 
                             organization = "",
                             title = "",
                             address = "",
                             email = "",
                             home_or_cell = "",
                             business = "",
                             fax_lol = "",
                             is_elected = FALSE
                             #r_num_years = 2
                             ))
  return (mydefault)
}

UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(rownames(data)))
  updateTextInput(session, "name", value = unname(data["name"]))
  updateTextInput(session,"organization",value = unname(data["organization"]))
  updateTextInput(session,"title",value = unname(data["title"]))
  updateTextInput(session,"address",value = unname(data["address"]))
  updateTextInput(session,"email", value = unname(data["email"]))
  updateTextInput(session,"home_or_cell",value = unname(data["home_or_cell"]))
  updateTextInput(session,"business",value = unname(data["business"]))
  updateTextInput(session,"fax_lol", value = unname(data["fax_lol"]))
  updateCheckboxInput(session, "is_elected", value = as.logical(data["is_elected"]))
  #updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
}

GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    max(as.integer(rownames(responses))) + 1
  } else {
    return (1)
  }
}

CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

GetTableMetadata <- function() {
  fields <- c(id = "Id", 
              name = "Name", 
              organization = "Organization",
              title = "Title",
              address = "Address",
              email = "Email",
              home_or_cell = "Home or Cell",
              business = "Business",
              fax_lol = "Fax",
              is_elected = "Is Elected") 
              
              #r_num_years = "R Years")
  result <- list(fields = fields)
  return (result)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  fluidRow(column(width = 6,
  #input fields
  tags$hr(),
  shinyjs::disabled(textInput("id", "Id", "0")),
  textInput("name", "Name", ""),
  textInput("organization","Organization",""),
  textInput("title","Title",""),
  textInput("address","Address",""),
  textInput("email", "Email",""),
  textInput("home_or_cell","Home or Cell",""),
  textInput("business","Business",""),
  textInput("fax_lol", "Fax", ""),
  checkboxInput("is_elected", "Is Elected", FALSE),
  #sliderInput("r_num_years", "R Years", 0, 25, 2, ticks = FALSE),
  
  #action buttons
  actionButton("submit", "Submit"),
  actionButton("new", "New"),
  actionButton("delete", "Delete")),
  
  #data table
  column(DT::dataTableOutput("responses"),width = 6)
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      UpdateData(formData())
    } else {
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)
  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected, ]
      UpdateInputs(data, session)
    }
  })
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadata()$fields)[-1]
  )     
}
# Run the application 
shinyApp(ui = ui, server = server)

