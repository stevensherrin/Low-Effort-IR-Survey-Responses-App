library(shiny)
library(dplyr)
library(reactable)
library(bslib)

# Source the helper functions
source("helpers.R")
source("library_functions.R")

# UI definition
ui <- fluidPage(
  theme = bslib::bs_theme(
    primary = "#007BFF",
    secondary = "#6C757D"
  ),
  tags$head(
    tags$style(HTML("
      .shiny-input-container {  
        font-family: 'IBM Plex Sans', sans-serif;
      }
      .icon {
        font-size: 24px;
        vertical-align: middle;
        margin-right: 10px;
      }
      .footer {
        font-size: 12px;
        color: #6C757D;
        margin-top: 20px;
        text-align: right;
      }
      .header {
        display: flex;
        align-items: center;
        margin-bottom: 20px;
      }
      .header img {
        margin-right: 10px;
      }
      .safety-icon {
        font-size: 24px;
        color: #28a745;
        vertical-align: middle;
        margin-right: 10px;
      }
      .overview-icon {
        font-size: 24px;
        color: #007BFF;
        vertical-align: middle;
        margin-right: 10px;
      }
      .main-title {
        font-size: 36px;
        font-weight: bold;
        text-align: left;
        margin-bottom: 20px;
      }
    "))
  ),
  
  # Main header with the title
  div(
    class = "header",
    div(
      style = "background: linear-gradient(to right, #007BFF, #6C757D);
              padding: 10px; border-radius: 8px; display: inline-block;",
      h1(
        style = "font-family: 'IBM Plex Sans', sans-serif;
                 color: white; font-weight: bold;
                 text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.4);",
        "Detecting Low-Effort IR Survey Responses"
      )
    )
  ),
  
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("surveySelect", "Select your survey", 
                  choices = c("NSSE" = "National Survey of Student Engagement (NSSE)")),
      fileInput("file", "Upload the raw data file (.csv, .xlsx):"),
      actionButton("run", "Run")#,
      #downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 uiOutput("analysisCompleteOverview"),
                 HTML("<div style='margin-top: 20px;'>
        <p><span class='overview-icon' style='font-size: 150%;'>&#128200;</span> This dashboard is a free tool designed to flag and remove low-effort responses on higher education surveys like the NSSE.</p>
        <p><span class='safety-icon' style='font-size: 150%;'>&#128274;</span> This tool is deployed using <a href='https://shiny.posit.co/py/docs/shinylive.html' target='_blank'>ShinyLive</a>, ensuring your data are processed locally and remain secure.</p>
        <p><span class='process-icon' style='font-size: 150%;'>&#128187;</span> How does this work? Import your <b>raw</b> data file for the survey provided by the organization running the survey - <a href='#' onclick='alert(\"Details on how to prepare your data file go here.\");'>click here for details</a>.</p>
    </div>")
        ),
        tabPanel("Summary",
                 tags$div(
                   tags$br(),  # Add a line break here
                   tags$h3(style = "font-weight: bold;", "Survey Response Summary"),
                   tags$p(style = "font-size: 18px; font-weight: bold; color: #2E86C1;", uiOutput("summaryText")),
                   tags$br(), tags$br(),  # Add two line breaks here
                   tags$h3(style = "font-weight: bold;", "Summary Table"),
                   reactableOutput("summaryTable")
                 )
        ),

        tabPanel("Explore Data",
                 selectInput("dataViewSelect", "See Low-Effort Responses for:",
                             choices = c("Filter 1: Survey Duration" = "Completed survey in 3 minutes or less",
                                         "Filter 2: Skipped Questions" = "Skipped over 25% of survey questions",
                                         "Filter 3: Straightlined 15 Responses" = "Straightlined 15 Responses",
                                         "Filters 4 & 5: Repeated Straightline Behavior" = "Repeated Straightline Behavior",
                                         "Filter 6: Other Repetitive Behavior" = "Other Repetitive Behavior",
                                         "Filter 7: Unrealistic Quantitative Responses" = "Unrealistic Quantitative Responses")),
                 HTML("<p><i>This table displays respondents who were flagged for this particular issue, suggesting a low-effort response.</i></p>"),
                 reactableOutput("individualExamplesTable")
        ),
        tabPanel("Download Data",
                 HTML("<div style='margin-top: 20px;'>
                        <h2>&#128190; Download Data</h2>
                        <p>The 'Download Data' exports a file containing your raw data with the addition of new columns indicating if (and why) the respondent was flagged as a low-effort responder.</p>
                      </div>"),
                 downloadButton("downloadData", "Download Data")
        ),
        tabPanel("About",
                 HTML("<h2 style='font-weight: bold;'>About this Tool</h2>
        <p>This is an 'opinionated' tool: it decides what responses qualify as 'low effort' or not based on subjectively selected criteria. Our tool uses a sequential screening method to exclude respondents whose behavior may suggest a low-effort response. The table above provides a summary of your results from the raw data file. <a href='methodology_link_here'>More details on the methodology can be found here.</a></p>
        <p>You can view the source code at Steve's <a href='https://github.com/stevensherrin' target='_blank'>GitHub page</a>. You are allowed to use and improve it, but not for any commercial purposes.</p>")
        )
        
        
      )
    )
  ),
  div(class = "footer",
      HTML("<p>This dashboard was created by <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a>. Research was conducted by <a href='https://www.linkedin.com/in/ingerbergom' target='_blank'>Inger Bergom</a> (Harvard University) and <a href='https://www.linkedin.com/in/steven-sherrin' target='_blank'>Steven Sherrin</a> (Wentworth Institute of Technology).</p>")
  )
)

# Server logic
server <- function(input, output, session) {
  data <- reactiveVal()
  
  
  observeEvent(input$run, {
    req(input$file)
    
    # Read the uploaded file
    df <- read_uploaded_file(input$file)
    
    # Check for missing variables
    missing_vars <- check_missing_vars(df)
    
    # Show screen if any variables are missing
    if (length(missing_vars) > 0) {
      showModal(modalDialog(
        title = "Missing Variables",
        paste(missing_vars, collapse = ", "),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$run, {
    req(input$file)
    
    # Remove recoded or estimated variables not used in the analysis
    df <- remove_recoded_vars(df)
    
    # Clean the uploaded data
    df <- clean_data(df)
    
    # Identify careless responses
    df <- identify_careless_responses(df)
    
    # Process the summary table data
    summary_df <- calculate_summary(df)
    data(summary_df)
    
    # Let user know analysis is complete
    output$analysisCompleteOverview <- renderUI({
      HTML("<h3 style='font-size: 16px; border: 1px dotted pink; padding: 10px; font-weight: bold;'><br>Analysis of your data is complete! To see a summary table of your results, go to 'Summary'. To see individual responses, go to 'Explore Data'. To download your data, go to 'Download Data'.</h3>")
    })
    
    output$analysisComplete <- renderUI({
      HTML("<h3 style='font-size: 16px; border: 1px dotted pink; padding: 10px; font-weight: bold;'><br>Analysis of your data is complete! To see a summary table of your results, go to 'Summary'. To see individual responses, go to 'Explore Data'. To download your data, go to 'Download Data'.</h3>")
    })
    
    
    # Render summary text
    output$summaryText <- renderText({
      max_val <- max(data()$`Total Remaining Respondents`)
      min_val <- min(data()$`Total Remaining Respondents`)
      filtered_out <- max_val - min_val
      percent_flagged <- (filtered_out / max_val) * 100
      paste0("You started with <b style='text-decoration: underline double;'>", max_val, "</b> respondents. After we searched for low-effort responses, we identified and removed <b style='text-decoration: underline double;'>", 
             filtered_out, "</b> respondents (<b style='text-decoration: underline double;'>", round(percent_flagged, 1), "% of total</b>). You now have <b style='text-decoration: underline double;'>", min_val, "</b> remaining responses. For details on why respondents were removed, see the table below. For methodology details, see About.")
    })
    
    
    # Render summary table
    output$summaryTable <- renderReactable({
      req(data())
      
      modified_data <- data()[-1, ]  # Remove the first row
      
      reactable(
        modified_data,
        columns = list(
          Criteria = colDef(
            name = "",
            cell = function(value) {
              div(style = list(fontStyle = "italic"), value)
            },
            headerStyle = list(verticalAlign = "top"),  # Align header to bottom
            minWidth = 350, maxWidth = 350
          ),
          `Total Remaining Respondents` = colDef(
            name = "Total Remaining Respondents",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150, maxWidth = 150
          ),
          `Total Respondents Excluded` = colDef(
            name = "Number of Respondents Excluded due to this Step",
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center"),
            minWidth = 150, maxWidth = 150
          ),
          `Percentage of Total Respondents Removed` = colDef(
            name = "Percent of Respondents Excluded due to this Step",
            headerStyle = list(textAlign = "center"),
            format = colFormat(percent = TRUE, digits = 1),
            style = list(textAlign = "center"),
            minWidth = 150, maxWidth = 150
            
          )
        ),
        rowStyle = function(index) {
          if (index == nrow(modified_data)) {
            list(
              background = "darkred",  # dark red background
              color = "white",
              fontWeight = "bold",
              border = "2px solid #cc0000",  # border to further highlight
              width = "100%"
            )
          } else {
            NULL
          }
        }
      )
    })
    
    
    
    # output$diagram <- renderDiagrammeR({
    #   generate_diagram(summary_df)
    # })
    
    
    # Render Explore Data table
    output$individualExamplesTable <- renderReactable({
      req(data()) # Ensure data is loaded
      
      if (input$dataViewSelect == "Completed survey in 3 minutes or less") {
        process_individual_examples(df, "Completed survey in 3 minutes or less")
        columns <- colnames(step_1_filtered)
        
        column_defs <- setNames(lapply(columns, function(col) {
          if (col == "Duration (minutes)") {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value <= 3) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_1_filtered, defaultPageSize = 10, columns = column_defs, 
                  style = list(width = "75%")) # Limit the width of the table because it's only two columns
        
      } else if (input$dataViewSelect == "Skipped over 25% of survey questions") {
        process_individual_examples(df, "Skipped over 25% of survey questions")
        columns <- colnames(step_2_filtered) # Get the column names of the data
        
        # Create a named list of column definitions
        column_defs <- setNames(lapply(columns, function(col) {
          colDef(
            cell = function(value) {
              if (col == "Percentage of Missing Values") {
                if (!is.na(value) && value >= 25) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              } else {
                if (is.na(value)) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;")
                } else {
                  value
                }
              }
            },
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center")
          )
        }), columns)
        
        reactable(step_2_filtered, defaultPageSize = 10, columns = column_defs)
        
      }
      
      else if (input$dataViewSelect == "Straightlined 15 Responses") {
        process_individual_examples(df, "Straightlined 15 Responses")
        columns <- colnames(step_3_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for "Straightline Length" column
        column_defs <- setNames(lapply(columns, function(col) {
          if (col == "Straightline Length") {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value >= 15) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_3_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Repeated Straightline Behavior") {
        process_individual_examples(df, "Repeated Straightline Behavior")
        columns <- colnames(step_4_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for the specified columns
        column_defs <- setNames(lapply(columns, function(col) {
          if (col %in% c("# of Times Straightlined 7 or More Responses", "# of Subscales Straightlined")) {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value >= 3) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_4_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Other Repetitive Behavior") {
        process_individual_examples(df, "Other Repetitive Behavior")
        columns <- colnames(step_6_values) # Get the column names of the data
        
        # Create a named list of column definitions with specific styling for the specified columns
        column_defs <- setNames(lapply(columns, function(col) {
          if (col %in% c("Flagged for Repetitive Behavior")) {
            colDef(
              cell = function(value) {
                if (!is.na(value) && value == 1) {
                  div(style = "background-color: lightcoral; height: 100%; width: 100%;", value)
                } else {
                  value
                }
              }
            )
          } else {
            colDef(
              headerStyle = list(textAlign = "center"),
              style = list(textAlign = "center")
            )
          }
        }), columns)
        
        reactable(step_6_values, defaultPageSize = 10, columns = column_defs)
      }
      
      else if (input$dataViewSelect == "Unrealistic Quantitative Responses") {
        process_individual_examples(df, "Unrealistic Quantitative Responses")
        columns <- colnames(step_7_values) # Get the column names of the data
        
        column_defs <- setNames(lapply(columns, function(col) {
          colDef(
            headerStyle = list(textAlign = "center"),
            style = list(textAlign = "center")
          )
        }), columns)
        
        reactable(step_7_values, defaultPageSize = 10, columns = column_defs)
      }
    })
    
    
    # Allow download of processed data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("nsse_survey_low_effort_responses_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        # Create a new workbook
        new_wb <- createWorkbook()
        
        # Define styles
        header_style <- createStyle(textDecoration = "bold", fontSize = 14)
        text_style <- createStyle(fontSize = 12, wrapText = TRUE)
        bullet_style <- createStyle(fontSize = 12, wrapText = TRUE, indent = 1)
        
        # Add "About" sheet with the specified text
        addWorksheet(new_wb, "About")
        
        about_text <- c(
          "This Excel file has your data and results from the 'Detecting Low Effort Surveys' tool. It contains the following sheets:",
          "",
          "1. 'Your Summary': A table displaying the frequency (and percentage) of how often respondents exhibited behaviors indicating low-effort responding.",
          "2. 'Your Data': Your data with new columns indicating which respondents should be removed from future analyses.",
          "",
          "If you have any questions, please contact Steve at stevensherrin@gmail.com with the title 'Detecting Low Effort Surveys'."
        )
        
        # Write the "About" text to the first column
        writeData(new_wb, "About", about_text, startCol = 1, startRow = 1)
        
        # Apply styles to the text
        addStyle(new_wb, "About", header_style, rows = 1, cols = 1)
        addStyle(new_wb, "About", text_style, rows = 2:2, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", bullet_style, rows = 3:5, cols = 1, gridExpand = TRUE)
        addStyle(new_wb, "About", text_style, rows = 6:6, cols = 1, gridExpand = TRUE)
        
        # Adjust column width to fit content
        setColWidths(new_wb, "About", cols = 1, widths = 100)
        
        # Add the summary sheet with data
        addWorksheet(new_wb, "Your Summary")
        writeData(new_wb, "Your Summary", data())
        
        # Autofit the column widths for the "Your Summary" sheet
        for (col in 1:ncol(data())) {
          setColWidths(new_wb, "Your Summary", cols = col, widths = "auto")
        }
        
        # Bold the top row of "Your Summary"
        addStyle(new_wb, "Your Summary", header_style, rows = 1, cols = 1:ncol(data()), gridExpand = TRUE)
        
        # Style the last row of "Your Summary" with black fill and white font
        last_row <- nrow(data()) + 1
        last_row_style <- createStyle(fgFill = "black", fontColour = "white")
        addStyle(new_wb, "Your Summary", last_row_style, rows = last_row, cols = 1:ncol(data()), gridExpand = TRUE)
        
        # Add a data sheet with detailed data
        addWorksheet(new_wb, "Your Data")
        
        df2 <- df
        df2 <- df2 %>% rename(`Row # in Original Data` = unique_id)
        writeData(new_wb, "Your Data", df2)
        
        # Autofit the column widths for the "Your Data" sheet
        for (col in 1:ncol(df)) {
          setColWidths(new_wb, "Your Data", cols = col, widths = "auto")
        }
        
        # Bold the column names of "Your Data"
        addStyle(new_wb, "Your Data", header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        
        # Apply light blue background to the first two columns of "Your Data"
        light_blue_style <- createStyle(fgFill = "#ADD8E6")
        addStyle(new_wb, "Your Data", light_blue_style, rows = 1:nrow(df) + 1, cols = 1:2, gridExpand = TRUE)
        
        # Save the new workbook
        saveWorkbook(new_wb, file, overwrite = TRUE)
      }
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
