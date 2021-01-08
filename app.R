#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
Sys.setlocale(locale = "English")
library(shiny)
library(shinyjs)
library(europepmc)
library(tidytext)
library(xml2)
library(dplyr)
library(zip)
library(shinythemes)
library(qdapTools)
library(DT)
source("helper.R")


# Define UI for application
ui <- function(request){
    fluidPage(
        
        theme = shinytheme("journal"),
        
        #Need to enable 'disabled' function
        useShinyjs(),
        
        # Application title
        titlePanel(title=div(img(src="logo.svg", width = "70px"), "Text sieve")),
        
        
        
        fluidRow(
            column(width = 3,
                   wellPanel(
                       #Term 
                       textInput("srch_term",
                                 "Search term",
                                 value = "blebbistatin",
                                 placeholder = "Enter you search phrase here"),
                       
                       fileInput("Dic_1", "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       
                       fileInput("Dic_2", "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       
                       #Number of papers to analyze
                       numericInput("que_limit",
                                    "Maximum number of records",
                                    value = 3,
                                    min = 1,
                                    max = 3000,
                                    step = 1),
                       
                       #Button to start search
                       actionButton("search", "Search", width = '100%', class = "btn-primary btn-sm"),
                       
                   )
            ),
            column(width = 9,
                   wellPanel(
                       textOutput("nothing"),
                       DTOutput("tbl")
                   )
            )
        )
    )
}
# Define server logic required
server <- function(input, output) {
    
    values = reactiveValues(collected.data = "Nothing to show, yet")
    
    observeEvent(input$search, {
        #browser()
        dictionary1 = csv_to_vector(file = input$Dic_1)
        dictionary2 = csv_to_vector(file = input$Dic_2)
        
        collected.data = sieving(srch_term = input$srch_term,
                                        que_limit = input$que_limit,
                                        dic1 = dictionary1,
                                        dic2 = dictionary2)
        if (class(collected.data) != "character"){
            #https://github.com/rasmusab/tidyverse-in-a-table/blob/master/tidyverse_in_a_table.R
            # A mystery column consisting only of pluses (&oplus;), read on for explanation!
            collected.data$" " = '&oplus;'
        }
        values$collected.data = collected.data
       
    })
    
    output$nothing = renderText(if (class(values$collected.data) == "character") {values$collected.data} )
    
    output$tbl = renderDT(
        if (class(values$collected.data) != "character"){
            df_nrow = nrow(values$collected.data)
            
            #https://github.com/rasmusab/tidyverse-in-a-table/blob/master/tidyverse_in_a_table.R
            datatable(
                
                as.data.frame(values$collected.data[,c("pmcid", "pubYear",
                                                       "author", "title",
                                                       "sentence", "paragraphs",
                                                       " ", "journalTitle", "doi",
                                                       "issue", "journalVolume")]),
                # Render HTML in the table
                escape = FALSE,
                # Add search boxes for each column at the "top" of the table
                filter = "top",
                # Register the javascript code we wrote above as a callback
                callback = datatable_callback,
                selection = list(mode = 'multiple',
                                 target = 'cell',
                                 selectable = matrix(c(1:df_nrow, rep(5, times = df_nrow)), ncol = 2)),
                options =  list(
                    # Show 5 rows by default
                    pageLength = 5,
                    # But it will be possible to show up to a 100 rows!
                    lengthMenu = c(5, 10, 20, 100),
                    # Some column specific settings
                    columnDefs = list(
                        # column 0 (row numbers) and 6 (Examples) are hidden
                        list(visible = FALSE, targets = c(0, 3, 4, 6, 8, 9, 10, 11)),
                        # The special column with (+) gets the details-control class so that it
                        # triggers the callback code
                        list(orderable = FALSE, className = 'details-control', targets = 7))))
            
        }
    )
    
    #input$tbl_cells_selected[,1] - rows numbers of the selected cells
}

# Run the application 
shinyApp(ui = ui, server = server)
