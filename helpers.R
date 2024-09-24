# Load necessary libraries
#library(tidyverse)
library(openxlsx) #YES
library(writexl) #Yes
library(dplyr) #YES
library(tidyr) #YES
library(ggplot2) #YES
library(purrr) #YES
library(janitor) #YES
library(gtools) #YES
library(conflicted) #YES
library(reactable) #YES
library(scales) #YES
library(crosstalk) #YES
library(htmltools) #Yes

conflicts_prefer(base::as.numeric())
conflicts_prefer(base::is.character())
conflicts_prefer(base::`&&`)
conflicts_prefer(base::`||`)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

done <- 0

# List of required variables
required_vars <- c(
  "askquest", "drafts", "unprepared", "attendart",
  "CLaskhelp", "CLexplain", "CLstudy", "CLproject", "present",
  "RIintegrate", "RIsocietal", "RIdiverse", "RIownview", "RIperspect",
  "RInewview", "RIconnect", "SFcareer", "SFotherwork", "SFdiscuss",
  "SFperform", "memorize", "HOapply", "HOanalyze", "HOevaluate",
  "HOform", "ETgoals", "ETorganize", "ETexample", "ETdraftfb",
  "ETfeedback", "QRconclude", "QRproblem", "QRevaluate", 
  "DDrace", "DDeconomic", "DDreligion", "DDpolitical",
  "LSreading", "LSnotes", "LSsummary",
  "challenge", "intern", "leader", "learncom", "abroad",
  "research", "capstone", "servcourse",
  "QIstudent", "QIadvisor", "QIfaculty", "QIstaff", "QIadmin",
  "empstudy", "SEacademic", "SElearnsup", "SEdiverse", "SEsocial", "SEwellness", "SEnonacad", "SEactivities", "SEevents",
  "pgwrite", "pgwspeak","pgthink", "pganalyze", "pgwork", "pgothers", "pgvalues", "pgdiverse", "pgprobsolve", "pgcitizen",
  "tmprephrs","tmcocurrhrs", "tmworkonhrs", "tmworkoffhrs", "tmservicehrs",
  "tmrelaxhrs", "tmcarehrs", "tmcommutehrs", "duration"
)

# Function to check required variables
check_missing_vars <- function(df) {
  # Get the column names of the dataframe
  df_columns <- colnames(df)
  
  # Check for the presence of specific required columns
  missing_vars <- setdiff(required_vars, df_columns)
  
  # Print missing variables if any
  if (length(missing_vars) > 0) {
    print(paste("The following columns are missing from your raw NSSE file:", 
                paste(missing_vars, collapse = ", "), 
                ". Some of this may be due to analyzing a particular year from NSSE, especially older years. Keep in mind this may prevent the analysis from running properly."))
  } else {
    print("All required columns are present.")
  }
  
}

# Function to remove recoded and estimated variables
# (In some cases - such as hours per week questions - we do choose to keep the estimated variables and remove the original ones)
remove_recoded_vars <- function(df) {
  # Create list of possible variables to remove
  # (This will depend on year of survey - not all variables will be present in all years)
  recoded_vars <- c("unpreparedr",
                    "wrshortnum",
                    "wrmednum",
                    "wrlongnum",
                    "wrpages",
                    "HIPsumFY",
                    "HIPsumSR",
                    "QIstudentR",
                    "QIadvisorR",
                    "QIfacultyR",
                    "QIstaffR",
                    "QIadminR",
                    "tmprep",
                    "tmcocurr",
                    "tmworkon",
                    "tmworkoff",
                    "tmworkhrs",
                    "tmservice",
                    "tmrelax",
                    "tmcare",
                    "tmcommute",
                    "tmread",
                    "reading",
                    "tmreadinghrscol",
                    "wrshort", # These "wr"/writing questions are wedged in the middle of the main page and have a different response scale. They are not used in the main analyses.
                    "wrmed", # See above
                    "wrlong",
                    "wrshortnum",
                    "wrmednum",
                    "wrlongnum",
                    "wrpages")
  
  # Remove recoded variables from the dataframe
  df <- df %>%
    select(-any_of(recoded_vars))
  
  print("Removed recoded and/or estimated variables not used in analyses.")
  
  return(df)
}

# Function to read the uploaded file
read_uploaded_file <- function(file) {
  ext <- tools::file_ext(file$name)
  if (ext == "csv") {
    df <- read.csv(file$datapath)
  } else if (ext == "xlsx") {
    df <- read.xlsx(file$datapath)
  } else {
    stop("Invalid file type. Please upload a .csv or .xlsx file.")
  }
  assign("df", df, envir = .GlobalEnv)
  return(df)
}

# Function to clean and prepare the dataframe
clean_data <- function(df) {
  # Clean column names
  df <- clean_names(df)
  print("Column names successfully cleaned")
  
  # Check if all columns have valid names
  na_or_empty_names <- which(is.na(names(df)) | names(df) == "")
  
  # Print columns that have invalid names; if there are none, say "No columns with NA or empty names"
  if (length(na_or_empty_names) > 0) {
    cat("Columns with NA or empty names:", na_or_empty_names, "\n")
    # Assign new names to these columns, for example: V1, V2, etc.
    names(df)[na_or_empty_names] <- paste("V", na_or_empty_names, sep = "")
  } else {
    print("No columns with NA or empty names")
  }
  
  # Create a unique identifier for each row
  df <- df %>%
    mutate(unique_id = paste0(row_number()))
  print("Unique identifier for each row created")
  
  assign("df", df, envir = .GlobalEnv)
  return(df)
}

# Function to process the data for summary table
calculate_summary <- function(df) {
  summary_df <- tibble(
    Criteria = c("Responses in raw NSSE dataset",
                 "Completed survey in 3 minutes or less", 
                 "Skipped over 25% of survey questions",
                 "Straightlined 15 or more responses in a row",
                 "Had 3 or more times they straightlined least 7 responses in a row",
                 "Straightlined 3 or more scales",
                 "Made repetitive pattern (e.g. AB-AB-AB) 50% or more of the time",
                 "Unrealistic response to quantitative question (e.g. hours per week)",
                 "Highly unusual responses to highly correlated items"),
    `Percentage of Total Respondents Removed` = c(
      0,
      abs(row_changes[1]) / nrow(df),
      abs(row_changes[2]) / nrow(df),
      abs(row_changes[3]) / nrow(df),
      abs(row_changes[4]) / nrow(df),
      abs(row_changes[5]) / nrow(df),
      abs(row_changes[6]) / nrow(df),
      abs(row_changes[7]) / nrow(df),
      abs(row_changes[8]) / nrow(df)
      
    ),
    `Total Respondents Excluded` = c(
      0,
      abs(row_changes[1]),
      abs(row_changes[2]),
      abs(row_changes[3]),
      abs(row_changes[4]),
      abs(row_changes[5]),
      abs(row_changes[6]),
      abs(row_changes[7]),
      abs(row_changes[8])
    ))
    
  summary_df <- summary_df %>%
    mutate(`Total Remaining Respondents` = nrow(df) - cumsum(`Total Respondents Excluded`)) %>%
    mutate(`Total Remaining Respondents` = replace(`Total Remaining Respondents`, 1, nrow(df)))
  
  summary_df <- summary_df %>%
    select(Criteria, `Total Remaining Respondents`, everything())
  
  #Starting at row #2, add "Filter/Step X: " to the beginning of each Criteria. Start with step 1.
  summary_df$Criteria[2:nrow(summary_df)] <- paste0("Filter/Step ", 1:(nrow(summary_df)-1), ": ", summary_df$Criteria[2:nrow(summary_df)])

  #Create a summary row with the sums, except for "Total Remaining Respondents", which is the last found value for that column
  summary_df <- summary_df %>%
    bind_rows(
      tibble(
        Criteria = "Final",
        `Percentage of Total Respondents Removed` = sum(summary_df$`Percentage of Total Respondents Removed`),
        `Total Respondents Excluded` = sum(summary_df$`Total Respondents Excluded`),
        `Total Remaining Respondents` = summary_df$`Total Remaining Respondents`[[nrow(summary_df)]]
      )
    )
  
  assign("summary_df", summary_df, envir = .GlobalEnv)
  return(summary_df)
}

# Function to generate DiagrammeR graph
# generate_diagram <- function(summary_df) {
  # summary_df <- summary_df %>%
  #   mutate(label = paste0(Criteria, "\nExcluded: ", Excluded, "\nRemaining: ", Remaining))
  # 
  # nodes <- paste0(
  #   "node [shape = box, style = filled, fillcolor = LightSkyBlue]",
  #   paste0("n", 1:nrow(summary_df), " [label = '", summary_df$label, "'];", collapse = "\n")
  # )
  # 
  # edges <- paste0(
  #   paste0("n", 1:(nrow(summary_df)-1), " -> n", 2:nrow(summary_df), ";", collapse = "\n")
  # )
  # 
  # graph <- paste0(
  #   "digraph flowchart {",
  #   nodes, "\n",
  #   edges, "\n",
  #   "}"
  # )
  # 
  # return(grViz(graph))
  
  # design <- tibble::tribble(
  #   ~left,               ~n_left, ~right,              ~n_right,
  #   "Study base",        1000,    "Not sampled",       250,
  #   "Study population",  750,     "Participants with\nmissing exposure data", 100,
  #   "Complete-case set", 650,     "",                  NA_integer_)
  # 
  # # Plot
  # exclusion_flowchart(design, width = 2)
# }


# Format summary table
format_percentage_cell <- function(value) {
  # Ensure value is between 0 and 1 for percentages
  value <- min(max(value, 0), 1)
  # Rescale value to 0-1 range for gradient
  gradientValue <- scales::rescale(value, c(0, 1), c(0, 1))
  # Use the gradient value to interpolate between white and red
  color <- colorRampPalette(c("white", "red"))(100)[as.integer(gradientValue * 99) + 1]
  style <- paste("background-color:", color, "; color: black;") # Set font color to black
  htmltools::span(style = style, scales::percent(value, accuracy = 0.1))
}

# Function to process individual examples based on selected view
process_individual_examples <- function(df, view_select) {
  if (view_select == "Completed survey in 3 minutes or less") {

    step_1_filtered <- df %>%
      select(unique_id, duration) %>%
      mutate(duration = round(as.numeric(duration), 1)) %>%
      arrange(duration)
    
    step_1_filtered <- step_1_filtered %>%
      rename(
        `Row in Dataset` = unique_id,
        `Duration (minutes)` = duration
      )
    
    # Add to global environment
    assign("step_1_filtered", step_1_filtered, envir = .GlobalEnv)
    
    return(step_1_filtered)
    
  } else if (view_select == "Skipped over 25% of survey questions") {
    
    step_2_filtered <- step_1 %>%
      mutate(missing_percentage = round(rowSums(is.na(.)) / ncol(.) * 100),1)
    
    step_2_filtered <- step_2_filtered %>%
      select(missing_percentage, unique_id, everything())
    
    step_2_filtered <- step_2_filtered %>%
      arrange(desc(missing_percentage))
    
    step_2_filtered <- step_2_filtered %>%
      rename(
        `Row in Dataset` = unique_id,
        `Percentage of Missing Values` = missing_percentage
      )
    

    # Add to global environment
    assign("step_2_filtered", step_2_filtered, envir = .GlobalEnv)
    
    return(step_2_filtered)
  }
  
  
  else if (view_select == "Straightlined 15 Responses") {
    
    step_3_values <- step_3_values %>%
      arrange(desc(longstring)) %>%
      rename(`Row in Dataset` = unique_id,
             `Straightline Length` = longstring)
    
    
    # Add to global environment
    assign("step_3_values", step_3_values, envir = .GlobalEnv)
    
    return(step_3_values)
  }
  
  else if (view_select == "Repeated Straightline Behavior") {
    
    step_4_values <- merge(step_4_values, step_5_values,
                           by = "unique_id",
                           all.x = TRUE)
    
    step_4_values <- step_4_values %>%
      arrange(desc(longstring_7_or_more)) %>%
      rename(`Row in Dataset` = unique_id,
             `# of Times Straightlined 7 or More Responses` = longstring_7_or_more,
             `# of Subscales Straightlined` = longstring_3_or_more_scales)
    
    step_4_values <- step_4_values %>%
      select(`Row in Dataset`, 
             `# of Times Straightlined 7 or More Responses`, 
             `# of Subscales Straightlined`,
             everything())
    
    step_4_values <- step_4_values %>% select(-longstring)
    
    # Add to global environment
    assign("step_4_values", step_4_values, envir = .GlobalEnv)
    
    return(step_4_values)
  }
  
  else if (view_select == "Other Repetitive Behavior") {
    step_6_values <- step_6_values %>%
      arrange(desc(repetitive), desc(rp_2)) %>%
      rename(`Row in Dataset` = unique_id,
             `Flagged for Repetitive Behavior` = repetitive,
             `% of Times Previous Response Repeated` = rp_2,
             `% of Times 2nd Previous Response Repeated` = rp_3,
             `% of Times 3rd Previous Response Repeated` = rp_4,
             `% of Times 4th Previous Response Repeated` = rp_5) %>%
      # Divide the variables with "%" in the name by 100 and then format as percentages
      mutate(across(contains("%"), ~ . / 100)) %>%
      mutate(across(contains("%"), ~ scales::percent(., accuracy = 0.1)))
    
    assign("step_6_values", step_6_values, envir = .GlobalEnv)
    
    return(step_6_values)
  }
  
  else if (view_select == "Unrealistic Quantitative Responses") {
    step_7_values <- step_7_values %>%
      arrange(desc(total_hours_per_week)) %>%
      rename(`Row in Dataset` = unique_id,
             `Total Hours per Week` = total_hours_per_week) %>%
      mutate(`Flagged for Unrealistic Hours?` = ifelse( `Total Hours per Week` > 140, "Yes", "No")) %>%
      mutate(`Total Hours per Week` = ifelse(is.na(`Total Hours per Week`), 0, `Total Hours per Week`)) %>%
      select(`Row in Dataset`, `Total Hours per Week`, `Flagged for Unrealistic Hours?`, everything())
    
    assign("step_7_values", step_7_values, envir = .GlobalEnv)
    
    return(step_7_values)
  }
  

    
}

# Function to identify careless responses
identify_careless_responses <- function(df) {
  
  ############################################
  ### Metrics for Main Page of NSSE Survey ###
  ###########################################
  
  # Subsetting and transforming data for the first page of survey
  df_main_page <- df %>%
    select(unique_id, duration, askquest:s_eevents) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) # Convert all columns to numeric
  
  # Extracting unique IDs
  first_page_ids <- df_main_page$unique_id
  #df_main_page <- df_main_page %>% select(-unique_id)
  
  # Perform your specific analysis (placeholder for your functions)
  # longstring, na measures, psychsyn, and mahad analysis
  # Ensure these functions are defined or loaded from respective libraries
  
  #Step 1: Screen low duration
  step_1 <- df_main_page %>%
    filter(as.numeric(duration) > 3) %>%
    select(-duration)
  
  print("Step 1 completed")
  
  #Step 2: Screen for high number of missing values
  step_2 <- step_1 %>%
    filter(rowSums(is.na(step_1)) < 15)
  
  print("Step 2 completed")
  
  #Step 3: Screen for longstrings of 15 or more
  step_3 <- step_2 %>%
    mutate(longstring = longstring(step_2)) 
  
  step_3_values <- step_3 %>%
    select(unique_id, longstring, everything())
  
  step_3 <- step_3 %>%
    filter(longstring < 15)
  
  print("Step 3 completed")
  
  #Step 4: Screen for repeated longstrings of more than 6
  step_4 <- step_3 %>%
    mutate(longstring_7_or_more = longstring_n_times(step_3, threshold = 6)$count_longstr) 
  
  step_4_values <- step_4 %>%
    select(unique_id, longstring_7_or_more, everything())
  
  step_4 <- step_4 %>%
    filter(longstring_7_or_more < 3)
  
  print("Step 4 completed")
  
  #Step 5: Screen for respondents who answer 3 or more scales with 0 variance
  #Calculate longstrings for each question set ("q_set_") and return the number "1" if the longstring equals the number of responses
  # Define question sets
  question_sets <- list(
    q_set_1 = c("askquest", "c_laskhelp", "c_lexplain", "c_lstudy", "c_lproject", "present"),
    q_set_2 = c("r_iintegrate", "r_isocietal", "r_idiverse", "r_iownview", "r_iperspect", "r_inewview", "r_iconnect"),
    q_set_3 = c("memorize", "h_oapply", "h_oanalyze", "h_oevaluate", "h_oform"),
    q_set_4 = c("e_tgoals", "e_torganize", "e_texample", "e_tdraftfb", "e_tfeedback", "e_tcriteria", "e_treview", "e_tprefer", "e_tdemonstrate"),
    q_set_5 = c("d_drace", "d_deconomic", "d_dreligion", "d_dpolitical", "d_dsexorient", "d_dcountry"),
    q_set_6 = c("intern", "leader", "learncom", "abroad", "research", "capstone", "servcourse"),
    q_set_7 = c("empstudy", "s_eacademic", "s_elearnsup", "s_ediverse", "s_esocial", "s_ewellness", "s_enonacad", "s_eactivities", "s_eevents"),
    q_set_8 = c("tmprephrs", "tmcocurrhrs", "tmworkonhrs", "tmworkoffhrs", "tmservicehrs", "tmrelaxhrs", "tmcarehrs", "tmcommutehrs")
  )
  
  # Add variables from question sets to dataframe (if not already present)
  vars_in_question_sets <- unique(unlist(question_sets))
  
  # Convert unique_id in step_4 and df to character
  step_4$unique_id <- as.character(step_4$unique_id)
  df$unique_id <- as.character(df$unique_id)
  
  step_5 <- step_4 %>%
    left_join(df %>% select(unique_id, any_of(vars_in_question_sets)), by = "unique_id", suffix = c("", ".df")) %>%
    select(-ends_with(".df"))
  
  # Function to calculate longstring ratio for a question set
  calculate_longstring_ratio <- function(df, set_name, set_columns) {
    df %>%
      mutate(!!paste0(set_name, "_longstring") := as.numeric(longstring(select(., one_of(set_columns)))) / 
               rowSums(!is.na(select(., one_of(set_columns)))))
  }
  
  # Iterate over all question sets and calculate longstring ratio
  for (i in 1:length(question_sets)) {
    set_name <- paste0("q_set_", i)
    set_columns <- question_sets[[set_name]]
    step_5 <- calculate_longstring_ratio(step_5, set_name, set_columns)
  }
  
  # Calculate the number of question sets with longstring ratio equal to 1
  step_5 <- step_5 %>%
    mutate(longstring_3_or_more_scales = rowSums(select(., starts_with("q_set_")) == 1)) 
  
  step_5_values <- step_5 %>%
    select(unique_id, longstring_3_or_more_scales)
  
  step_5 <- step_5 %>%
    filter(longstring_3_or_more_scales < 3)
  
  print("Step 5 completed")

  
  #Step 6: Screen for 2-value repetitive pattern (e.g. AB-AB-AB)
  # Keep only the columns found in df_main_page. (We'll add the other ones back later.)
  #common_columns <- intersect(names(step_5), names(df_main_page))
  
  # step_6 <- step_5 %>%
  #   select(all_of(common_columns), -unique_id)
  
  step_6 <- df %>%
    select(unique_id, askquest:s_eevents) %>%
    #keep only unique_ids found in step_5
    filter(unique_id %in% step_5$unique_id) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) # Convert all columns to numeric

  # Function to calculate percentage of repeated patterns
  calculate_repeated_pattern_percentage <- function(data, lag) {
    same_count <- rowSums(data == lag(data, n = lag, default = NA), na.rm = TRUE)
    total_count <- rowSums(!is.na(data))
    percentage <- (same_count / total_count) * 100
    return(percentage)
  }
  
  # Calculate percentages for different lags and store in new columns
  step_6 <- step_6 %>%
    mutate(rp_2 = calculate_repeated_pattern_percentage(across(everything()), 2),
           rp_3 = calculate_repeated_pattern_percentage(across(everything()), 3),
           rp_4 = calculate_repeated_pattern_percentage(across(everything()), 4),
           rp_5 = calculate_repeated_pattern_percentage(across(everything()), 5))
  
  # If any of rp_2 to rp_5 is 50% or higher, flag the respondent
  step_6 <- step_6 %>%
    mutate(repetitive = ifelse(rowSums(select(., starts_with("rp_")) >= 60) > 0, 1, 0))
  
  # Save step_6 before filtering
  step_6_values <- step_6 %>%
    select(unique_id, repetitive, contains("rp_"), everything())
  
  # z <- step_6_values %>%
  #   filter(repetitive == 1)
  
  # # Function to plot responses for a single respondent
  # plot_responses <- function(responses, respondent_id, start_question, end_question) {
  #   responses_subset <- responses[start_question:end_question]
  #   responses_long <- data.frame(Question = names(responses_subset), Response = unlist(responses_subset))
  #   responses_long <- responses_long[complete.cases(responses_long), ]
  #   
  #   ggplot(responses_long, aes(x = Response, y = factor(Question, levels = rev(names(responses_subset))))) +
  #     geom_point(color = "blue", size = 3) +
  #     geom_line(aes(group = 1), color = "blue") +
  #     scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  #     theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  #     ggtitle(paste("Responses for Respondent", respondent_id, "(Questions", start_question, "to", end_question, ")")) +
  #     xlab("Response Option") +
  #     ylab("Questions") +
  #     theme_minimal()
  # }
  # 
  # # Loop through the respondents to generate the plots, 25 questions per page
  # questions_per_page <- 25
  # total_questions <- ncol(z)
  # 
  # for (respondent_id in 1:nrow(z)) {
  #   num_pages <- ceiling(total_questions / questions_per_page)
  #   for (page in 1:num_pages) {
  #     start_question <- (page - 1) * questions_per_page + 1
  #     end_question <- min(page * questions_per_page, total_questions)
  #     pdf(paste0("respondent_", respondent_id, "_page_", page, ".pdf"))
  #     print(plot_responses(z[respondent_id, , drop = FALSE], respondent_id, start_question, end_question))
  #     dev.off()
  #   }
  # }
  
  # Filter out respondents with repetitive patterns
  step_6 <- step_6 %>%
    filter(repetitive == 0 | is.na(repetitive) == TRUE)
  
  print("Step 6 completed")
  
  # step_6_filtered <- step_6[, !grepl("^rp_", colnames(step_6))]
  # step_6_filtered <- step_6 %>% select(-unique_id)
  # rep_pattern_algo = rp.patterns(step_6_filtered)
  # indices <- rp.indices(rep_pattern_algo, include.coefs = FALSE)
  # 
  # rp.plot(rep_pattern_algo, obs = 105)
  
  # Step 7: Screen for invalid responses to "hours" question set
  # Create dataframe "df_hours" with variables found in list question_sets q_set_8
  df_hours <- df %>%
    select(unique_id, tmprephrs:tmcommutehrs) %>%
    filter(unique_id %in% step_6$unique_id) %>%
    #select(-unique_id) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.)))) # Convert all columns to numeric
  
  # Calculate total hours per week
  df_hours$total_hours_per_week <- rowSums(select(df_hours, -unique_id), na.rm = TRUE)
  
  # Add df_hours$total_hours_per_week to step_7 based on unique_id
  step_7 <- step_6 %>%
    left_join(df_hours, by = "unique_id")
  
  # Save results
  step_7_values <- step_7 %>%
    select(unique_id, total_hours_per_week, contains("tm"))
  
  # Filter out respondents with unrealistic total hours per week
  step_7 <- step_7 %>%
    mutate(total_hours_per_week = ifelse(is.na(total_hours_per_week), 0, total_hours_per_week)) %>%
    filter(total_hours_per_week <= 140)
  
  print("Step 7 completed")

  # Step 8: Screen for unusual responses to highly correlated items
  step_8 <- df %>%
    select(unique_id, askquest:sameinst) %>%
    #keep only unique_ids found in step_6
    filter(unique_id %in% step_7$unique_id) %>%
    #convert all columns to numeric
    mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%
    #Remove any columns with all NAs
    select(where(~!all(is.na(.))))
  
  step_8_filtered <- step_8 %>%
    select(-unique_id) #-tmworkhrs)
  
  #Scale all variables in step_7_filtered
  step_8_filtered <- step_8_filtered %>%
    mutate(across(everything(), scale))
  
  step_8$mv_syn <- psychsyn(step_8_filtered, critval = .50)
  step_8$mv_mahad <- scale(mahad(step_8_filtered))
  
  #Remove any rows with Mahalanobis distance greater than 3 and synonym correlation below 0
  step_8 <- step_8 %>%
    filter(mv_mahad < 3 & mv_syn > 0)
  
  print("Step 8 completed")
  
  #Compare the number of rows in each step
  steps <- list(df, step_1, step_2, step_3, step_4, step_5, step_6, step_7, step_8)
  
  # Initialize an empty vector to store the changes
  row_changes <- vector("numeric", length = length(steps) - 1)
  
  # Loop through the dataframes to calculate the changes in number of rows
  for (i in 1:(length(steps) - 1)) {
    row_changes[i] <- nrow(steps[[i + 1]]) - nrow(steps[[i]])
  }

  assign("row_changes", row_changes, envir = .GlobalEnv)
  
  # Find last step each respondent was included in
  # Initialize a data frame to store the results
  last_occurrence <- data.frame(unique_id = unique(df$unique_id), last_dataframe = NA)
  
  # Loop over each unique_id in df
  for (i in seq_along(last_occurrence$unique_id)) {
    unique_id <- last_occurrence$unique_id[i]
    # Loop over each dataframe in reverse order
    for (j in length(steps):1) {
      if (unique_id %in% steps[[j]]$unique_id) {
        last_occurrence$last_dataframe[i] <- paste("step", j-1, sep = "_")
        break
      }
    }
  }
  
  # Recode the last_dataframe column
  last_occurrence <- last_occurrence %>%
    mutate(last_dataframe = recode(last_dataframe, 
                                   `step_0` = "Completed survey in 3 minutes or less",
                                   `step_1` = "Skipped over 25% of survey questions",
                                   `step_2` = "Straightlined 15 or more responses in a row",
                                   `step_3` = "Had 3 or more times they straightlined least 7 responses in a row",
                                   `step_4` = "Straightlined 3 or more scales",
                                   `step_5` = "Made repetitive pattern (e.g. AB-AB-AB, ABC-ABC-ABC) 50% or more of the time",
                                   `step_6` = "Unrealistic responses to quantitative question set (e.g. hours per week)",
                                   `step_7` = "Highly unusual responses to highly correlated items",
                                   `step_8` = "")) %>%
    rename(`Reason for Flag` = last_dataframe)
  
  # Add "flagged" column to last_occurrence
  last_occurrence$`Flagged for Low Effort?` <- ifelse(last_occurrence$`Reason for Flag` == "", "No", "Yes")
  
  #Reorder columns
  last_occurrence <- last_occurrence %>%
    select(unique_id, `Flagged for Low Effort?`, `Reason for Flag`)
  
  #Add to df based on unique_id and move it to front
  df <- df %>%
    left_join(last_occurrence, by = "unique_id") %>%
    select(`Flagged for Low Effort?`, `Reason for Flag`, unique_id, everything())

  assign("df", df, envir = .GlobalEnv)
  assign("step_1", step_1, envir = .GlobalEnv)
  assign("step_2", step_2, envir = .GlobalEnv)
  assign("step_3", step_3, envir = .GlobalEnv)
  assign("step_4", step_4, envir = .GlobalEnv)
  assign("step_5", step_5, envir = .GlobalEnv)
  assign("step_6", step_6, envir = .GlobalEnv)
  assign("step_7", step_7, envir = .GlobalEnv)
  assign("step_8", step_8, envir = .GlobalEnv)
  
  
  assign("step_3_values", step_3_values, envir = .GlobalEnv)
  assign("step_4_values", step_4_values, envir = .GlobalEnv)
  assign("step_5_values", step_5_values, envir = .GlobalEnv)
  assign("step_6_values", step_6_values, envir = .GlobalEnv)
  assign("step_7_values", step_7_values, envir = .GlobalEnv)
  #assign("z", z, envir = .GlobalEnv)
  
  return(df)
}

