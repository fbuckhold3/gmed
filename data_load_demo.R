devtools::load_all()
rdm_token <- '601B0B68946F06A3396E0C3FA591DB1E'

# ============================================================================
# REAL DATA DEMO APP FOR ASSESSMENT MODULE
# demo_real_data.R
# ============================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(plotly)
library(DT)

# ============================================================================
# REAL DATA LOADING
# ============================================================================

# IMPORTANT: Load your data functions first!
# Uncomment and modify these lines based on how you load your functions:
# 
# library(gmed)  # If functions are in gmed package
# source("your_data_functions.R")  # If functions are in a script
# devtools::load_all()  # If working in package development

cat("📋 Checking for required functions...\n")

# Check if required functions are available
required_functions <- c("load_data_by_forms", "filter_archived_residents", 
                        "add_level_at_time_to_forms", "prepare_milestone_app_data")

missing_functions <- required_functions[!sapply(required_functions, exists)]

if (length(missing_functions) > 0) {
  cat("❌ Missing functions:", paste(missing_functions, collapse = ", "), "\n")
  cat("💡 Please load your functions first. Example:\n")
  cat("   library(gmed)  # or\n")
  cat("   source('R/rdm_data_functions.R')  # or\n") 
  cat("   devtools::load_all()  # if in package development\n")
  stop("Required functions not loaded. Please load them and restart the demo.")
} else {
  cat("✅ All required functions found\n")
}

# Set your RDM token - REPLACE WITH YOUR ACTUAL TOKEN
rdm_token <- '601B0B68946F06A3396E0C3FA591DB1E'  # <-- PUT YOUR TOKEN HERE

# Alternative: keep using environment variable if you prefer
# rdm_token <- Sys.getenv("RDM_TOKEN")

if (rdm_token == "" || rdm_token == "YOUR_ACTUAL_TOKEN_HERE") {
  cat("❌ Please set your actual RDM token in the code\n")
  cat("💡 Replace 'YOUR_ACTUAL_TOKEN_HERE' with your token, or use:\n")
  cat("   Sys.setenv(RDM_TOKEN = 'your_token')\n")
  stop("RDM_TOKEN not set. Please set it and restart the demo.")
} else {
  cat("✅ RDM_TOKEN found\n")
}

cat("🔄 Loading RDM 2.0 data...\n")

# Load data using your functions
data <- load_data_by_forms(rdm_token = rdm_token)
cat("✅ Raw data loaded\n")

# List available forms
forms_list <- list_forms(data)
cat("📋 Available forms:", paste(forms_list, collapse = ", "), "\n")

# Modified workflow: Calculate medians BEFORE archive filtering for historical data
cat("🔄 Modified workflow: Calculate milestone medians from historical data, then filter for active data...\n")

# Step 1: Calculate milestone medians from ALL residents (including archived)
cat("📊 Calculating milestone medians from complete historical data...\n")
historical_milestone_medians <- calculate_all_milestone_medians(data)
cat("✅ Historical milestone medians calculated\n")

# Step 2: Filter archived residents for active data
cat("🔄 Now filtering archived residents for active dataset...\n")
clean_data <- filter_archived_residents(data, verbose = TRUE)

# Step 3: Add levels to active residents only
data_with_levels <- add_level_at_time_to_forms(clean_data)  
cat("✅ Added levels to active residents\n")

# Step 4: Prepare milestone app data from active residents (but keep historical medians separate)
cat("📊 Preparing milestone app data from active residents...\n")
mile_data <- prepare_milestone_app_data(data_with_levels)
cat("✅ Prepared milestone data from active residents\n")

# Step 5: Store historical medians separately for comparison
milestone_medians_historical <- historical_milestone_medians
cat("💾 Historical medians stored separately\n")

cat("📈 Historical medians calculated from: ALL residents (including archived)\n")
cat("👥 Active milestone data from:", nrow(mile_data$residents), "active residents\n")
cat("🎯 Apps can use historical medians for broader comparisons\n")

# Extract residents and forms
residents <- data_with_levels$forms$resident_data
all_forms <- data_with_levels$forms

if (is.null(residents)) {
  # Try alternative names
  residents <- all_forms$residents %||% 
    all_forms$demographic_data %||%
    all_forms[[1]]  # Fallback to first form
  cat("⚠️ Used alternative form for resident data\n")
}

# Check what columns are available
if (!is.null(residents)) {
  cat("📋 Resident data columns:", paste(names(residents), collapse = ", "), "\n")
  
  # Add Level column if it doesn't exist
  if (!"Level" %in% names(residents)) {
    # Try common alternative column names
    level_alternatives <- c("level", "resident_level", "training_level", "pgy", "PGY", "year")
    found_level_col <- level_alternatives[level_alternatives %in% names(residents)][1]
    
    if (!is.na(found_level_col)) {
      residents$Level <- residents[[found_level_col]]
      cat("📝 Using", found_level_col, "as Level column\n")
    } else {
      residents$Level <- "Unknown"
      cat("⚠️ No Level column found, using 'Unknown'\n")
    }
  }
  
  # Add name column if it doesn't exist
  if (!"name" %in% names(residents)) {
    name_alternatives <- c("Name", "resident_name", "full_name", "first_name")
    found_name_col <- name_alternatives[name_alternatives %in% names(residents)][1]
    
    if (!is.na(found_name_col)) {
      residents$name <- residents[[found_name_col]]
      cat("📝 Using", found_name_col, "as name column\n")
    } else {
      residents$name <- paste("Resident", residents$record_id)
      cat("⚠️ No name column found, using 'Resident [ID]'\n")
    }
  }
}

# Combine all assessment data
all_forms <- data_with_levels$forms
assessment_data <- bind_rows(all_forms, .id = "source_form")

cat("📊 Final dataset:", nrow(assessment_data), "records from", length(all_forms), "forms\n")
cat("👥 Residents available:", nrow(residents), "\n")

data_loaded <- TRUE
