url <- "https://redcapsurvey.slu.edu/api/"

rdm_token <- '601B0B68946F06A3396E0C3FA591DB1E'

devtools::load_all()

# Load data organized by forms
data <- load_data_by_forms(rdm_token = rdm_token)



test <- data$raw_data

test %>% 
  filter(record_id == '88') %>%
  select(ass_date) -> t1

# See what forms are available
list_forms(data)

# Access specific forms
acgme_data <- data$forms$acgme_miles
ccc_data <- data$forms$ccc_review

# In your app - ONE API call, then process
data <- load_data_by_forms(rdm_token = rdm_token)  # Single API call
mile_data <- prepare_milestone_app_data(data)       # Process existing data
clean_data <- filter_archived_residents_all(data)
data_with_levels <- add_level_at_time_to_forms(clean_data)
# Applies to: assessment, faculty_evaluation, questions
# 

