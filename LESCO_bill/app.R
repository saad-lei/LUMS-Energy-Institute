# Hello
#### Libraries ####
library(bslib)
library(tidyr)
library(dplyr)
#.libPaths(c("/home/eig-8/R/x86_64-pc-linux-gnu-library/4.3", .libPaths()))
library("pdftools")
library(stringr)
library(lubridate)
library(tidyverse)
library(jsonlite)
library(shinyjs)
library(shiny)
library(tesseract)
library(magick)
library(DT)
#### Read csv's ####
df <- read.csv("client_profiles.csv")
annotations <- read.csv("annotations.csv")
categories <- read.csv("categories.csv")
#### Function to extract text elements for  billing summary from bill ####
extract_billing_summary <- function(pdf_path) {
  text <- pdf_text(pdf_path)
  text <- strsplit(text, "\n")[[1]]
  text <- text[text != ""]  # Remove empty lines
  
  result <- c()
  
  for (i in 2:11) {
    text_i <- unlist(strsplit(text[i], "\\s{2,}", perl = TRUE))
    result <- c(result, text_i)
  }
  
  text_12 <- unlist(strsplit(text[12], "\\s{2,}", perl = TRUE))
  # Check if the number of elements is 4
  if (length(text_12) == 5) {
    # Insert "0" before the 4th element
    text_12 <- c(text_12[1:4], "0", text_12[5])
  }
  result <- c(result, text_12)
  
  for (i in 13:15) {
    text_i <- unlist(strsplit(text[i], "\\s{2,}", perl = TRUE))
    result <- c(result, text_i)
  }
  
  text_16 <- unlist(strsplit(text[16], "\\s{2,}", perl = TRUE))
  
  # If QTR TRF is not in bill
  if(length(text_16) > 2) {
    result <- c(result, 0)
    result <- c(result, 0)
    
    text_16 <- unlist(strsplit(text[16], "(\\s{2,17})", perl = TRUE))
    # Check if the length is less than 10
    if (length(text_16) < 10) {
      # Calculate the number of empty elements needed
      empty_elements <- 10 - length(text_16)
      
      # Append empty elements to make it 10
      text_16 <- c(text_16, rep("", empty_elements))
    }
    result <- c(result, text_16)
    
    text_17 <- unlist(strsplit(text[17], "(\\s{2,19})", perl = TRUE))
    # Check if the length is less than 10
    if (length(text_17) < 10) {
      # Calculate the number of empty elements needed
      empty_elements <- 10 - length(text_17)
      
      # Append empty elements to make it 10
      text_17 <- c(text_17, rep("", empty_elements))
    }
    result <- c(result, text_17)
    
    text_19 <- unlist(strsplit(text[19], "(\\s{2,27})", perl = TRUE))
    # Check if the length is less than 5
    if (length(text_19) < 5) {
      # Calculate the number of empty elements needed
      empty_elements <- 5 - length(text_19)
      
      # Append empty elements to make it 5
      text_19 <- c(text_19, rep("", empty_elements))
    }
    result <- c(result, text_19)
    
    text_sec_last <- unlist(strsplit(tail(text, 2)[1], "\\s{2,}", perl = TRUE))
    result <- c(result, text_sec_last)
    
    text_last <- unlist(strsplit(tail(text, 1), "\\s{2,}", perl = TRUE))
    result <- c(result, text_last)
  }
  # If QTR TRF is in bill
  else{
    result <- c(result, text_16)
    text_17 <- unlist(strsplit(text[17], "(\\s{2,19})", perl = TRUE))
    # Check if the length is less than 10
    if (length(text_17) < 10) {
      # Calculate the number of empty elements needed
      empty_elements <- 10 - length(text_17)
      
      # Append empty elements to make it 10
      text_17 <- c(text_17, rep("", empty_elements))
    }
    result <- c(result, text_17)
    
    text_18 <- unlist(strsplit(text[18], "(\\s{2,19})", perl = TRUE))
    # Check if the length is less than 10
    if (length(text_18) < 10) {
      # Calculate the number of empty elements needed
      empty_elements <- 10 - length(text_18)
      
      # Append empty elements to make it 10
      text_18 <- c(text_18, rep("", empty_elements))
    }
    result <- c(result, text_18)
    
    text_20 <- unlist(strsplit(text[20], "(\\s{2,27})", perl = TRUE))
    # Check if the length is less than 5
    if (length(text_20) < 5) {
      # Calculate the number of empty elements needed
      empty_elements <- 5 - length(text_20)
      
      # Append empty elements to make it 5
      text_20 <- c(text_20, rep("", empty_elements))
    }
    result <- c(result, text_20)
    
    text_sec_last <- unlist(strsplit(tail(text, 2)[1], "\\s{2,}", perl = TRUE))
    result <- c(result, text_sec_last)
    
    text_last <- unlist(strsplit(tail(text, 1), "\\s{2,}", perl = TRUE))
    result <- c(result, text_last)
  }
  
  return(result)
}
#### Function to create the billing summary csv ####
create_billing_summary <- function(text) {
  column_names <- c("username", "billingMonth", "customerID", "referenceNumber", "tarrif", "sanctionedLoad", "dateReading", "dateIssue",
                    "dateDue", "billTotal", "billLate", "unitsPeak", "unitsOffpeak", "unitsTotal", "MDI", 
                    "Energy Cost", "costFixed", "LPF Penalty", "Seasonal Charges", "Meter Rent", "Service Rent", "Variable FPA", "costQTRADJ", "Total Cost",
                    "E-Duty", "TV Fee", "GST", "Income Tax", "Extra Tax", "Further Tax", "ITS Tax", "S. Tax", "NJ Surcharge", "Sales Tax", "FC Surcharge",
                    "TR Surcharge", "FPA", "GST. Adj.", "Total Tax", "Deffered Amount", "Outstanding Amount",
                    "Arrear", "Current Bill", "Bill Adjustment", "Installment", "Total FPA", "LP Surcharge", "Costs & Adj", "amountPaid", "paymentDate")
  
  extracted_data <- data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
  colnames(extracted_data) <- column_names
  
  extracted_data$username <- df$user[grepl(paste0("\\b", text[2], "\\b"), df$Customer_ID)]
  extracted_data$billingMonth <- text[4]
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  extracted_data$billingMonth <- dmy(paste0("01-", text[4]))
  extracted_data$billingMonth <- format(extracted_data$billingMonth, "%B-%Y")
  extracted_data$customerID <- text[2]
  extracted_data$referenceNumber <- text[9]
  extracted_data$tarrif <- text[10]
  extracted_data$sanctionedLoad <- as.numeric(text[11])
  date_reading <- dmy(text[5])
  formatted_date_reading <- format(date_reading, "%Y-%m-%d")
  extracted_data$dateReading <- formatted_date_reading
  date_issue <- dmy(text[6])
  formatted_date_issue <- format(date_issue, "%Y-%m-%d")
  extracted_data$dateIssue <- formatted_date_issue
  date_due <- dmy(text[7])
  formatted_date_due <- format(date_due, "%Y-%m-%d")
  extracted_data$dateDue <- formatted_date_due
  extracted_data$billTotal <- as.numeric(text[87])
  extracted_data$billLate <- as.numeric(text[92])
  extracted_data$unitsPeak <- as.numeric(str_extract(text[56], "(?<=\\(P\\)).*"))
  extracted_data$unitsOffpeak <- as.numeric(str_extract(text[56], "(?<=\\(O\\))\\S+"))
  extracted_data$unitsTotal <- extracted_data$unitsPeak + extracted_data$unitsOffpeak
  # Try to calculate the maximum numeric value
  max_number <- tryCatch({
    values <- as.numeric(regmatches(text[58], gregexpr("\\d+", text[58]))[[1]])
    if (length(values) > 0) {
      max(values, na.rm = TRUE)
    } else {
      0
    }
  }, error = function(e) {
    # If an error occurs (e.g., empty vector), assign 0
    0
  })
  extracted_data$MDI <- max_number
  extracted_data$`Energy Cost` <- ifelse(text[62] == "", 0, as.numeric(text[62]))
  extracted_data$costFixed <- ifelse(text[72] == "", 0, as.numeric(text[72]))
  extracted_data$`LPF Penalty` <- ifelse(text[64] == "", 0, as.numeric(text[64]))
  extracted_data$`Seasonal Charges` <- ifelse(text[74] == "", 0, as.numeric(text[74]))
  extracted_data$`Meter Rent` <- ifelse(text[63] == "", 0, as.numeric(text[63]))
  extracted_data$`Service Rent` <- ifelse(text[73] == "", 0, as.numeric(text[73]))
  extracted_data$`Variable FPA` <- 0
  extracted_data$costQTRADJ <- as.numeric(text[60])
  extracted_data$`Total Cost` <- extracted_data$`Energy Cost` + extracted_data$costFixed + extracted_data$`LPF Penalty` + extracted_data$`Seasonal Charges` + extracted_data$`Meter Rent` + extracted_data$`Service Rent`
  extracted_data$`E-Duty` <- 0
  extracted_data$`TV Fee` <- 0
  extracted_data$GST <- ifelse(text[82] == "", 0, as.numeric(str_extract(text[82], "\\d+")))
  extracted_data$`Income Tax` <- ifelse(text[83] == "", 0, as.numeric(text[83]))
  extracted_data$`Extra Tax` <- ifelse(text[67] == "", 0, as.numeric(text[67]))
  extracted_data$`Further Tax` <- ifelse(text[77] == "", 0, as.numeric(text[77]))
  extracted_data$`ITS Tax` <- 0 
  extracted_data$`S. Tax` <- 0
  extracted_data$`NJ Surcharge` <- ifelse(text[81] == "", 0, as.numeric(text[81]))
  extracted_data$`Sales Tax` <- 0
  extracted_data$`FC Surcharge` <- ifelse(text[80] == "", 0, as.numeric(text[80]))
  extracted_data$`TR Surcharge` <- ifelse(text[70] == "", 0, as.numeric(text[70]))
  extracted_data$FPA <- ifelse(text[69] == "", 0, as.numeric(text[69]))
  extracted_data$`GST. Adj.` <- ifelse(text[85] == "", 0, as.numeric(text[85]))
  extracted_data$`Total Tax` <- extracted_data$`Income Tax` + extracted_data$GST + extracted_data$FPA + extracted_data$`Extra Tax` + extracted_data$`Further Tax` + extracted_data$`FC Surcharge` + extracted_data$`NJ Surcharge` + extracted_data$`GST. Adj.`
  extracted_data$`Deffered Amount` <- ifelse(text[65] == "", 0, as.numeric(text[65]))
  extracted_data$`Outstanding Amount` <- 0
  extracted_data$Arrear <- 0
  extracted_data$`Current Bill` <- as.numeric(text[87])
  extracted_data$`Bill Adjustment` <- ifelse(text[66] == "", 0, as.numeric(text[66]))
  extracted_data$Installment <- ifelse(text[76] == "", 0, as.numeric(text[76]))
  extracted_data$`Total FPA` <- ifelse(text[69] == "", 0, as.numeric(text[69]))
  extracted_data$`LP Surcharge` <- 0
  extracted_data$`Costs & Adj` <- 0
  extracted_data$amountPaid <- 0
  extracted_data$paymentDate <- 0
  
  # Modifying column orders
  extracted_data <- extracted_data %>%
    select(-amountPaid, -paymentDate, everything(), amountPaid, paymentDate)
  
  return(extracted_data)
}
#### Function to extract history table from pdf ####
extract_billing_history <- function(pdf_path) {
  text <- pdf_text(pdf_path)
  text <- strsplit(text, "\n")[[1]]
  text <- text[text != ""]  # Remove empty lines
  result <- c()
  
  text_10 <- unlist(strsplit(tail(text,10)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_10[1], "") && identical(text_10[2], "") && identical(text_10[7], "")) {
    text_10 <- text_10[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_10[1], "") && identical(text_10[2], "")) {
    text_10 <- text_10[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_10[1], "") && length(text_10) == 11) {
    text_10 <- text_10[-c(1)]
  } else {
    text_10 <- text_10[-c(1, 8)]
  }
  result <- c(result,text_10)
  
  text_9 <- unlist(strsplit(tail(text,9)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_9[1], "") && identical(text_9[2], "") && identical(text_9[7], "")) {
    text_9 <- text_9[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_9[1], "") && identical(text_9[2], "")) {
    text_9 <- text_9[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_9[1], "") && length(text_9) == 11) {
    text_9 <- text_9[-c(1)]
  } else {
    text_9 <- text_9[-c(1, 8)]
  }
  result <- c(result,text_9)
  
  text_8 <- unlist(strsplit(tail(text,8)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_8[1], "") && identical(text_8[2], "") && identical(text_8[7], "")) {
    text_8 <- text_8[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_8[1], "") && identical(text_8[2], "")) {
    text_8 <- text_8[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_8[1], "") && length(text_8) == 11) {
    text_8 <- text_8[-c(1)]
  } else {
    text_8 <- text_8[-c(1, 8)]
  }
  result <- c(result,text_8)
  
  text_7 <- unlist(strsplit(tail(text,7)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_7[1], "") && identical(text_7[2], "") && identical(text_7[7], "")) {
    text_7 <- text_7[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_7[1], "") && identical(text_7[2], "")) {
    text_7 <- text_7[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_7[1], "") && length(text_7) == 11) {
    text_7 <- text_7[-c(1)]
  } else {
    text_7 <- text_7[-c(1, 8)]
  }
  result <- c(result,text_7)
  
  text_6 <- unlist(strsplit(tail(text,6)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_6[1], "") && identical(text_6[2], "") && identical(text_6[7], "")) {
    text_6 <- text_6[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_6[1], "") && identical(text_6[2], "")) {
    text_6 <- text_6[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_6[1], "") && length(text_6) == 11) {
    text_6 <- text_6[-c(1)]
  } else {
    text_6 <- text_6[-c(1, 8)]
  }
  result <- c(result,text_6)
  
  text_5 <- unlist(strsplit(tail(text,5)[1], "(\\s{2,17})", perl = TRUE))
  if (identical(text_5[1], "") && identical(text_5[2], "") && identical(text_5[7], "")) {
    text_5 <- text_5[-c(1, 2, 3, 4, 5, 7)]
  } else if (identical(text_5[1], "") && identical(text_5[2], "")) {
    text_5 <- text_5[-c(1, 2, 3, 4, 5)]
  } else if (identical(text_5[1], "") && length(text_5) == 11) {
    text_5 <- text_5[-c(1)]
  } else {
    text_5 <- text_5[-c(1, 8)]
  }
  result <- c(result,text_5)
  
  return(result)
}
#### Function to create billing history csv using extracted history table ####
create_billing_history <- function(table) {
  column_names <- c("Month", "MDI", "Units", "Bill", "Payment")
  
  extracted_data <- data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
  colnames(extracted_data) <- column_names
  
  for (i in seq(1, length(table), 5)) {
    # Remove whitespaces from each column
    table[i:(i+4)] <- lapply(table[i:(i+4)], function(x) gsub("\\s", "", x))
    
    # Append the modified row to extracted_data
    extracted_data <- rbind(extracted_data, table[i:(i+4)])
  }
  
  extracted_data <- extracted_data[-1, ]
  
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  extracted_data$Month <- dmy(paste0("01-", extracted_data$Month))
  extracted_data <- extracted_data[order(extracted_data$Month), ]
  extracted_data$Month <- format(extracted_data$Month, "%B-%Y")
  
  return(extracted_data)
}

#### Function for Bill Summary Extraction (new format bill) ####
extract_billing_summary_new <- function(pdf_path){
  text <- pdf_text(pdf_path)
  text <- strsplit(text, "\n")[[1]]
  text <- text[text != ""]  # Remove empty lines
  if(!grepl("FPA", text[21])) {
    # If text[21] does NOT contain "FPA", insert 0 after text[19]
    text <- append(text, 0, 19)  # Append inserts BEFORE the index, so '0' becomes the new text[20]
  }
  result <- c()
  text_1 <- unlist(strsplit(text[1], "(\\s{2,17})", perl = TRUE))
  if(grepl("%", text_1[7])) {
    split_part <- strsplit(text_1[7], "% ", fixed = TRUE)[[1]]
    if(length(split_part) > 1 && nchar(trimws(split_part[2])) > 0) {
      text_1 <- append(text_1, c(paste0(split_part[1], "%"), trimws(split_part[2])), after = 6)
      text_1 <- text_1[-9]
    }
  }
  result <- c(result, text_1)
  text_2nd_last <- unlist(strsplit(tail(text, 2)[1], "(\\s{2,})", perl = TRUE))
  result <- c(result, text_2nd_last)
  text_last <- unlist(strsplit(tail(text, 1), "(\\s{2,})", perl = TRUE))
  result <- c(result, text_last)
  
  column_names <- c("username", "billingMonth", "customerID", "referenceNumber", "tariffType",
                    "sanctionedLoad", "dateIssue", "dateDue", "billTotal",
                    "billLate")
  extracted_data <- data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
  colnames(extracted_data) <- column_names
  extracted_data$username <- df$user[grepl(paste0("\\b", result[2], "\\b"), df$Customer_ID)]
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  extracted_data$billingMonth <- dmy(paste0("01-", result[8]))
  extracted_data$billingMonth <- format(extracted_data$billingMonth, "%B-%Y")
  extracted_data$customerID <- result[2]
  extracted_data$referenceNumber <- result[16]
  extracted_data$tariffType <- result[3]
  extracted_data$sanctionedLoad <- result[6]
  extracted_data$dateIssue <- dmy(result[9])
  extracted_data$dateIssue <- format(extracted_data$dateIssue, "%Y-%m-%d")
  extracted_data$dateDue <- dmy(result[10])
  extracted_data$dateDue <- format(extracted_data$dateDue, "%Y-%m-%d")
  extracted_data$billTotal <- result[12]
  extracted_data$billLate <- result[18]
  
  return(extracted_data)
}
#### Function for Cost and tax values extraction using OCR (new format bill) ####
text_ocr <- function(path) {
  
  # Map category ids to names
  category_names <- setNames(categories$name, categories$id)
  
  # Convert pdf into image
  img <- image_read(path, density = "1000x1000")
  
  # Initialize a data frame to store results
  results <- data.frame(label = character(), value = character(), stringsAsFactors = FALSE)
  
  # Process each annotation to extract text using OCR
  for (i in seq_len(length(annotations$id))) {
    bbox <- as.numeric(unlist(strsplit(annotations$bbox[i], ",")))
    # Define the region of interest in the image
    x <- bbox[1]
    y <- bbox[2]
    width <- bbox[3]
    height <- bbox[4]
    region_of_interest <- sprintf("%dx%d+%d+%d", width, height, x, y)
    roi <- image_crop(img, region_of_interest)
    # Use tesseract to extract text from the defined region
    text_extracted <- tesseract::ocr(roi)
    # Retrieve the label using the category id
    annotations$id <- annotations$id + 1
    label <- categories$name[categories$id == i]
    # Append results to the dataframe
    results <- rbind(results, data.frame(label = label, value = text_extracted))
    # Replace NULL values with 0
    results$value[is.na(results$value)] <- 0
    # Turn values into numeric type
    results$value <- gsub(",", "", results$value)  # Remove commas
    results$value <- as.numeric(results$value)     # Convert to numeric
    annotations$id <- annotations$id - 1
  }
  
  return(results)
}
#### Function for Bill History Extraction (new format bill) ####
extract_billing_history_new <- function(path){
  text <- pdf_text(path)
  text <- strsplit(text, "\n")[[1]]
  text <- text[text != ""]  # Remove empty lines
  # Remove unnecessary line from history table
  text <- text[!grepl("FPA", text)]
  result <- c()
  
  for (i in 8:20){
    # Remove commas from the numerical parts
    text[i] <- gsub(",", "", text[i])
    # Pattern to match month abbreviations
    pattern <- "(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)"
    # Use regexpr to find the starting position of the pattern
    match_pos <- regexpr(pattern, text[i], ignore.case = TRUE)
    # If a month is found, substring from its start position to the end of 'text'
    if (match_pos > 0) {
      cleaned_text <- substring(text[i], match_pos, nchar(text[i]))
    }
    else{
      next
    }
    text_i <- unlist(strsplit(cleaned_text, "\\s{2,}", perl = TRUE))
    result <- c(result, text_i)
  }
  # Remove elements containing "/"
  result <- result[!grepl("/", result)]
  result <- result[!result %in% c("DF", "MC", "EX", "SS")]
  
  # Initialize variables for tracking
  month_pattern <- "^[A-Z]{3}-\\d{2}$" # Regex to identify month abbreviations
  modified_result <- c() # Empty vector to store the modified list
  count <- 0 # Counter for numeric values following a month abbreviation
  # Iterate through the result list
  for (i in 1:length(result)) {
    current_element <- result[i]
    if (grepl(month_pattern, current_element)) { # Check if current element is a month abbreviation
      if (count < 3 && !length(modified_result) == 0) { # If less than 3 numeric values followed the previous month
        # Add zeroes to make it up to 3
        zeroes_to_add <- 3 - count
        for (j in 1:zeroes_to_add) {
          modified_result <- c(modified_result, "0")
        }
      }
      count <- 0 # Reset counter for the next month abbreviation
    } else {
      count <- count + 1 # Increment count for a numeric value
    }
    modified_result <- c(modified_result, current_element) # Add current element to modified list
  }
  # Check for numeric values after the last month abbreviation
  if (count < 3) {
    zeroes_to_add <- 3 - count
    for (j in 1:zeroes_to_add) {
      modified_result <- c(modified_result, "0")
    }
  }
  
  column_names <- c("Month", "Units", "Bill", "Payment")
  extracted_data <- data.frame(matrix(NA, nrow = 1, ncol = length(column_names)))
  colnames(extracted_data) <- column_names
  for (i in seq(1, length(modified_result), 4)) {
    # Remove whitespaces from each column
    modified_result[i:(i+3)] <- lapply(modified_result[i:(i+3)], function(x) gsub("\\s", "", x))
    # Append the modified row to extracted_data
    extracted_data <- rbind(extracted_data, modified_result[i:(i+3)])
  }
  extracted_data <- extracted_data[-1, ]
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  extracted_data$Month <- dmy(paste0("01-", extracted_data$Month))
  extracted_data <- extracted_data[order(extracted_data$Month), ]
  extracted_data$Month <- format(extracted_data$Month, "%B-%Y")
  
  return(extracted_data)
}
#### UI ####
ui <- navbarPage(
  title = "Bill Verification",
  theme = bs_theme(bootswatch = "default"),
  nav_spacer(),
  # First tab for "Bill Extraction"
  tabPanel(
    "Bill Extraction",
    sidebarLayout(
      sidebarPanel(
        fileInput('file_input', 'Upload PDF', accept = c('.pdf')),
        div(style = "display: flex; justify-content: center; align-items: center; flex-direction: column;",
            actionButton("verification", "Run", style = "background-color: #65A89B; margin-bottom: 10px;"),
            useShinyjs(),  # Initialize shinyjs
            hidden(div(id = "loadSpinner", class = "loader")),  # Add hidden spinner
            uiOutput("bill_type"),
            tags$style(HTML("
              .loader {
                border: 16px solid #f3f3f3;
                border-radius: 50%;
                border-top: 16px solid #3498db;
                width: 60px;
                height: 60px;
                animation: spin 2s linear infinite;
              }
              @keyframes spin {
                0% { transform: rotate(0deg); }
                100% { transform: rotate(360deg); }
              }
            "))
        ),
        actionButton("save", "Save", style = "background-color: #65A89B; margin-top: 10px;")  # Save button
      ),
      mainPanel(
        fluidRow(
          uiOutput("pdfview")
        ),
        fluidRow(
          card(
            card_header("Summary"),
            height = "auto",
            full_screen = TRUE,
            DT::dataTableOutput("summary_table")
          )
        ),
        fluidRow(
          card(
            card_header("History"),
            height = "auto",
            full_screen = TRUE,
            DT::dataTableOutput("history_table")
          )
        )
      )
    )
  ),
  
  # Second tab for "Manual Editing"
  tabPanel(
    "Manual Editing",
    fluidRow(
      accordion(
        id = "manualinfo",
        open = TRUE,
        accordion_panel(
          "Summary",
          fluidRow(
            DTOutput("manual_summary_table")
          ),
          fluidRow(
            column(
              width = 3,
              actionButton("manual_save_summary", "Save Summary")
            )
          )
        ),
        accordion_panel(
          "History",
          fluidRow(
            DTOutput("manual_history_table")
          ),
          fluidRow(
            column(width = 4, actionButton("add_row", "Add Row")),
            column(width = 4, actionButton("remove_row", "Remove Row")),
            column(width = 4, actionButton("manual_save_history", "Save History"))
          )
        )
      )
    )
  )
)

#### Server ####
server <- function(input, output,session) {
  summary_df <- reactiveVal()
  history_df <- reactiveVal()
  
  # Display uploaded pdf
  observe({
    req(input$file_input)
    file.copy(input$file_input$datapath, "www/bill.pdf", overwrite = TRUE)
    
    # Generate a unique identifier to prevent caching
    random_string <- paste(sample(c(0:9, letters, LETTERS), 10, replace = TRUE), collapse = "")
    
    output$pdfview <- renderUI({
      src <- paste0("bill.pdf?nocache=", random_string)
      tags$iframe(style = "height:1100px; width:100%", src = src)
    })
  })
  
  observeEvent(input$verification, {
    if (is.null(input$file_input)) {
      # Show notification if no file is uploaded
      showNotification("Please upload a PDF file first!", type = "warning")
    } else {
      # Show the spinner when the process starts
      shinyjs::show("loadSpinner")
      
      # Display radio button input dynamically
      output$bill_type <- renderUI({
        radioButtons("bill_option", "Select Bill Type", choices = c("Single Bill", "2 Meter-Bill 1", "2 Meter-Bill 2"), selected = "Single Bill")
      })
      
      # Check to see whether bill is old format or new format
      check <- pdf_text("www/bill.pdf")
      check <- strsplit(check, "\n")[[1]]
      if (grepl("WEB BILL", check[1], fixed = TRUE)) {
        # Extract summary text from the uploaded PDF
        text <- extract_billing_summary("www/bill.pdf")
        # Create billing summary data frame
        summary_df(create_billing_summary(text))
        # Extract history table text
        table <- extract_billing_history("www/bill.pdf")
        # Create billing history data frame
        history_df(create_billing_history(table))
        
      } else {
        # Create df for bill summary
        summary_df1 <- extract_billing_summary_new("www/bill.pdf")
        
        # Create 2nd df for bill summary
        summary_df2 <- text_ocr("www/bill.pdf")
        
        # Transform summary df2 from long to wide format, it's a single row
        summary_df2_wide <- summary_df2 %>% 
          pivot_wider(names_from = label, values_from = value) %>%
          slice(1)
        
        # Combine the transformed summary df2 with summary df1 by binding columns
        summary_data <- bind_cols(summary_df1, summary_df2_wide)
        
        summary_data <- summary_data %>%
          mutate(dateReading = NA, unitsPeak = 0, unitsOffpeak = 0, MDI = 0, costFixed = 0, `LPF Penalty` = 0, 
                 `Seasonal Charges` = 0, `Variable FPA` = 0, `ITS Tax` = 0, `S. Tax` = 0, `NJ Surcharge` = 0,
                 `Sales Tax` = 0, `TR Surcharge` = 0, `Deffered Amount` = 0, `Outstanding Amount` = 0,
                 `Bill Adjustment` = 0, `Costs & Adj` = 0, amountPaid = 0, paymentDate = 0)
        
        summary_data <- summary_data %>%
          select(1:13, MDI, everything())
        
        summary_data <- summary_data %>%
          rename(tarrif = tariffType)
        
        summary_data <- summary_data %>%
          rename(`Energy Cost` = costElectricity)
        
        summary_data <- summary_data %>%
          rename(`Meter Rent` = costMeter)
        
        summary_data <- summary_data %>%
          rename(`Service Rent`    = costService)
        
        summary_data <- summary_data %>%
          rename(FPA = costFPA)
        
        summary_data <- summary_data %>%
          rename(`FC Surcharge` = costFC_surcharge)
        
        summary_data <- summary_data %>%
          rename(`Total Cost` = costTotal)
        
        summary_data <- summary_data %>%
          rename(`E-Duty` = taxElectricity_duty)
        
        summary_data <- summary_data %>%
          rename(`TV Fee` = taxTV_fee)
        
        summary_data <- summary_data %>%
          rename(GST = taxGST)
        
        summary_data <- summary_data %>%
          rename(`Income Tax` = taxIncome)
        
        summary_data <- summary_data %>%
          rename(`Extra Tax` = taxExtra)
        
        summary_data <- summary_data %>%
          rename(`Further Tax` = taxFurther)
        
        summary_data <- summary_data %>%
          rename(`Total Tax` = taxTotal)
        
        summary_data <- summary_data %>%
          rename(Arrear = arrear)
        
        summary_data <- summary_data %>%
          rename(`Current Bill` = currentBill)
        
        summary_data <- summary_data %>%
          rename(Installment = installment)
        
        summary_data <- summary_data %>%
          rename(`Total FPA` = totalFPA)
        
        summary_data <- summary_data %>%
          rename(`GST. Adj.` = taxFPA_GST)
        
        summary_data <- summary_data %>%
          rename(`LP Surcharge` = LP_surcharge)
        
        summary_data <- summary_data %>%
          select(-taxRS, - taxFPA_extra, -taxFPA_further, -taxFPA_income, -taxFPA_ED, -taxFPA_RS, -subsidies)
        
        new_order <- c("username", "billingMonth", "customerID", "referenceNumber", "tarrif", "sanctionedLoad", 
                       "dateReading", "dateIssue","dateDue", "billTotal", "billLate", "unitsPeak", "unitsOffpeak",
                       "unitsTotal", "MDI", "Energy Cost", "costFixed", "LPF Penalty", "Seasonal Charges", 
                       "Meter Rent", "Service Rent", "Variable FPA", "costQTRADJ", "Total Cost","E-Duty", 
                       "TV Fee", "GST", "Income Tax", "Extra Tax", "Further Tax", "ITS Tax", "S. Tax", 
                       "NJ Surcharge", "Sales Tax", "FC Surcharge","TR Surcharge", "FPA", "GST. Adj.", 
                       "Total Tax", "Deffered Amount", "Outstanding Amount","Arrear", "Current Bill", 
                       "Bill Adjustment", "Installment", "Total FPA", "LP Surcharge", "Costs & Adj", "amountPaid", "paymentDate")
        
        summary_data <- summary_data %>%
          select(all_of(new_order))
        
        summary_df(summary_data)
        
        # Bill History Extraction
        history_df(extract_billing_history_new("www/bill.pdf"))
      }
      
      # Render summary and history tables
      output$summary_table <- DT::renderDataTable({
        req(summary_df())
        datatable(summary_df(), editable = TRUE)
      })
      output$history_table <- DT::renderDataTable({
        req(history_df())
        datatable(history_df(), editable = TRUE)
      })
      
      showModal(
        modalDialog(
          title = "Success!",
          "Dataframes have been created.",
          easyClose = TRUE
        )
      )
      # Hide the spinner
      shinyjs::hide("loadSpinner")
    }
  })
  
  observeEvent(input$summary_table_cell_edit, {
    info <- input$summary_table_cell_edit
    str(info) # for debugging
    summary_data <- summary_df()
    summary_data[info$row, info$col] <- info$value
    summary_df(summary_data)
  })
  
  observeEvent(input$history_table_cell_edit, {
    info <- input$history_table_cell_edit
    str(info) # for debugging
    history_data <- history_df()
    history_data[info$row, info$col] <- info$value
    history_df(history_data)
  })
  
  # Save edited data
  observeEvent(input$save, {
    summary_data <- summary_df()
    history_data <- history_df()
    
    # Append "_1" or "_2" to filenames based on the selected bill option
    suffix <- if (input$bill_option == "2 Meter-Bill 1") {
      "_1"
    } else if (input$bill_option == "2 Meter-Bill 2") {
      "_2"
    } else {
      ""
    }
    
    summary_data$billingMonth <- sapply(summary_data$billingMonth, function(month_year) {
      parts <- strsplit(month_year, "-")[[1]]
      month <- parts[1]
      year <- parts[2]
      month_abbr <- toupper(substr(month, 1, 3))
      year_abbr <- substr(year, 3, 4)
      paste0(month_abbr, "_", year_abbr)
    })
    
    # Paths to folders where csv will be placed
    base_path <- "/srv/shiny-server/Clients Usage Data/Electricity Bills/"
    user_path <- paste0(base_path, summary_data$username)
    
    # Create folder if does not exist
    if (!file.exists(user_path)) {
      dir.create(user_path, recursive = TRUE)
    }
    
    # File name for summary csv
    summary_file_name <- paste(sub("S/O.*", "", summary_data$username), summary_data$billingMonth, "billing_summary", sep = "_")
    summary_file_name <- paste0(summary_file_name, suffix)
    write.csv(summary_data, file = file.path(user_path, paste0(summary_file_name, ".csv")), row.names = FALSE)
    
    # File name for history csv
    history_file_name <- paste(sub("S/O.*", "", summary_data$username), summary_data$billingMonth, "billing_history", sep = "_")
    history_file_name <- paste0(history_file_name, suffix)
    write.csv(history_data, file = file.path(user_path, paste0(history_file_name, ".csv")), row.names = FALSE)
    
    # Save pdf
    pdf_file_name <- paste(sub("S/O.*", "", summary_data$username), summary_data$billingMonth, "bill", sep = "_")
    # Define the path to save the file
    file_path <- paste0(user_path, "/", pdf_file_name, ".pdf")
    # Save the file
    file.copy("www/bill.pdf", file_path)
    
    showModal(
      modalDialog(
        title = "Success!",
        "CSV files have been saved.",
        easyClose = TRUE
      )
    )
  })
  
  # Define default values for the first table (Billing Summary)
  manual_summary_data <- data.frame(
    username = "user123",
    billingMonth = "August-2024",
    customerID = 0,
    referenceNumber = "REF123",
    tarrif = "Tariff-X",
    sanctionedLoad = 0,
    dateReading = "2024-08-01",
    dateIssue = "2024-08-05",
    dateDue = "2024-08-25",
    billTotal = 0,
    billLate = 0,
    unitsPeak = 0,
    unitsOffpeak = 0,
    unitsTotal = 0,
    MDI = 0,
    `Energy Cost` = 0,
    costFixed = 0,
    `LPF Penalty` = 0,
    `Seasonal Charges` = 0,
    `Meter Rent` = 0,
    `Service Rent` = 0,
    `Variable FPA` = 0,
    costQTRADJ = 0,
    `Total Cost` = 0,
    `E-Duty` = 0,
    `TV Fee` = 0,
    GST = 0,
    `Income Tax` = 0,
    `Extra Tax` = 0,
    `Further Tax` = 0,
    `ITS Tax` = 0,
    `S. Tax` = 0,
    `NJ Surcharge` = 0,
    `Sales Tax` = 0,
    `FC Surcharge` = 0,
    `TR Surcharge` = 0,
    FPA = 0,
    `GST. Adj.` = 0,
    `Total Tax` = 0,
    `Deffered Amount` = 0,
    `Outstanding Amount` = 0,
    Arrear = 0,
    `Current Bill` = 0,
    `Bill Adjustment` = 0,
    Installment = 0,
    `Total FPA` = 0,
    `LP Surcharge` = 0,
    `Costs & Adj` = 0,
    amountPaid = 0,
    paymentDate = "0",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Reactive value to hold the original summary table
  manual_summary_table_data <- reactiveVal(manual_summary_data)
  
  # Transpose the dataframe for display in the table
  transpose_data <- function(data) {
    data.frame(
      Field = names(data),
      Value = unlist(data, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }
  
  # Reactive to hold the transposed data for display
  manual_summary_transposed <- reactive({
    transpose_data(manual_summary_table_data())
  })
  
  # Render the transposed summary table
  output$manual_summary_table <- renderDT({
    datatable(manual_summary_transposed(), editable = TRUE)
  }, server = FALSE)
  
  # Capture and save edits made in the transposed table
  observeEvent(input$manual_summary_table_cell_edit, {
    info <- input$manual_summary_table_cell_edit
    str(info)  # Check the edited info structure in the console
    
    # Update the transposed data with the new value
    transposed_data <- manual_summary_transposed()
    transposed_data[info$row, "Value"] <- info$value
    
    # Save the updated transposed data back into the original format
    updated_data <- manual_summary_table_data()
    updated_data[[info$row]] <- info$value  # Use the row index to update the correct field
    manual_summary_table_data(updated_data)  # Update the reactive data
  })
  
  # Save summary table to CSV
  observeEvent(input$manual_save_summary, {
    data <- manual_summary_table_data()
    username <- data$username
    billingMonth <- data$billingMonth
    month <- toupper(substr(billingMonth, 1, 3))
    year <- substr(billingMonth, nchar(billingMonth) - 1, nchar(billingMonth))
    
    # Paths to folders where csv will be placed
    base_path <- "/srv/shiny-server/Clients Usage Data/Electricity Bills/"
    user_path <- paste0(base_path, username)
    
    # Create folder if it does not exist
    if (!file.exists(user_path)) {
      dir.create(user_path, recursive = TRUE)
    }
    
    filename <- paste0(user_path, "/", username, "_", month, "_", year, "_billing_summary.csv")
    write.csv(data, filename, row.names = FALSE)
    showModal(modalDialog("Summary table saved!"))
  })
  
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
  # Extract current month and year
  current_date <- Sys.Date()
  
  # Generate 12 months in ascending order starting from the previous month
  months <- sapply(0:11, function(i) format(rollback(current_date) %m-% months(i), "%B-%Y"))
  
  # Define the default values for Units, Bill, and Payment
  units <- c(7130, 7980, 6040, 4800, 1710, 8800, 6490, 5990, 9590, 6951, 6466, 5920)
  bill <- c(298020, 661550, 212980, 446880, 718900, 379360, 281290, 627250, 524290, 369875, 303702, 300000)
  payment <- c(298020, 661550, 212980, 464360, 718900, 379360, 281290, 627250, 524290, 369875, 303702, 300000)
  
  # Create the data frame
  manual_history_data <- data.frame(
    Month = rev(months),  # Reverse to have months in ascending order
    MDI = rep(0, 12),     # MDI values as zero
    Units = units,
    Bill = bill,
    Payment = payment,
    stringsAsFactors = FALSE
  )
  
  # Reactive value to hold the history table
  manual_history_table_data <- reactiveVal(manual_history_data)
  
  # Render the history table
  output$manual_history_table <- renderDT({
    datatable(manual_history_table_data(), editable = TRUE)
  }, server = FALSE)
  
  # Capture and save edits made in the history table
  observeEvent(input$manual_history_table_cell_edit, {
    info <- input$manual_history_table_cell_edit
    str(info)  # Check the edited info structure in console
    data <- manual_history_table_data()
    
    # Update the value at the specific cell edited
    data[info$row, info$col] <- info$value
    manual_history_table_data(data)  # Update the reactive data
  })
  
  # Add a new row to the history table
  observeEvent(input$add_row, {
    data <- manual_history_table_data()
    new_row <- data.frame(Month = "", MDI = 0, Units = 0, Bill = 0, Payment = 0, stringsAsFactors = FALSE)
    data <- rbind(data, new_row)
    manual_history_table_data(data)
  })
  
  # Remove the last row from the history table
  observeEvent(input$remove_row, {
    data <- manual_history_table_data()
    if (nrow(data) > 1) {
      data <- data[-nrow(data), ]
      manual_history_table_data(data)
    }
  })
  
  # Save history table to CSV
  observeEvent(input$manual_save_history, {
    data <- manual_history_table_data()
    username <- manual_summary_table_data()$username
    billingMonth <- manual_summary_table_data()$billingMonth
    month <- toupper(substr(billingMonth, 1, 3))
    year <- substr(billingMonth, nchar(billingMonth) - 1, nchar(billingMonth))
    
    # Paths to folders where csv will be placed
    base_path <- "/srv/shiny-server/Clients Usage Data/Electricity Bills/"
    user_path <- paste0(base_path, username)
    
    # Create folder if it does not exist
    if (!file.exists(user_path)) {
      dir.create(user_path, recursive = TRUE)
    }
    
    filename <- paste0(user_path, "/", username, "_", month, "_", year, "_billing_history.csv")
    write.csv(data, filename, row.names = FALSE)
    showModal(modalDialog("History table saved!"))
  })
}
#### Shiny App ####
shinyApp(ui = ui, server = server)