# CLAUDE.md - AI Assistant Guide for GMED Package

**Last Updated**: 2025-11-15
**Package**: gmed - Graduate Medical Education Analytics Platform
**Version**: 0.0.0.9000
**Primary Language**: R
**Repository**: fbuckhold3/gmed

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Architecture & Structure](#architecture--structure)
3. [Development Workflow](#development-workflow)
4. [Code Conventions](#code-conventions)
5. [Key Technical Patterns](#key-technical-patterns)
6. [Testing Strategy](#testing-strategy)
7. [Common Tasks](#common-tasks)
8. [Important Considerations](#important-considerations)

---

## Project Overview

### Purpose

**gmed** is a sophisticated medical education analytics platform built for **Washington University School of Medicine's Department of Internal Medicine** (SSM Health/SLUCare system). It provides:

- **ACGME Milestone Tracking**: Comprehensive competency assessment across 6 domains (PC, MK, SBP, PBLI, PROF, ICS)
- **REDCap Integration**: Full API integration for data collection and submission
- **Interactive Dashboards**: Shiny-based web applications for residents, coaches, and administrators
- **Assessment Visualization**: Plus/Delta feedback, progression charts, spider plots
- **Period-Based Tracking**: Three scheduling types (Intern Intro, Mid Review, End Review)

### Domain Context

- **Users**: Medical residents (Intern, PGY2, PGY3), coaches, program directors
- **Competency Framework**: 6 ACGME competencies with 21 total subcompetencies
- **Academic Calendar**: July 1 - June 30
- **REDCap Version**: RDM 2.0 with repeating instruments
- **Institution**: SSM Health/SLUCare system

---

## Architecture & Structure

### Directory Layout

```
gmed/
├── R/                      # Package source code (33 modules, ~12,000 lines)
│   ├── milestone_*.R       # Milestone processing (1,989 lines in core)
│   ├── assessment_*.R      # Assessment visualization and processing
│   ├── mod_*.R             # Shiny modules (UI/server pairs)
│   ├── redcap_*.R          # REDCap API integration
│   ├── load_*.R            # Data loading workflows
│   ├── period_*.R          # Period/time calculations
│   └── ui_*.R              # UI components and theming
├── inst/
│   ├── assessment_viewer/  # Standalone Shiny app
│   └── www/                # Web assets (CSS, JS, icons)
│       ├── css/            # SSM Health themed stylesheets
│       ├── js/             # JavaScript components
│       └── milestones/     # Competency PNG icons (20+)
├── man/                    # R documentation (auto-generated)
├── tests/                  # testthat framework
├── docs/                   # Reference documentation
├── examples/               # Usage examples
├── DESCRIPTION             # Package metadata
├── NAMESPACE               # Exported functions (156 exports)
└── README.md               # Basic project info
```

### Key Modules by Function

#### Data Infrastructure
- **`load_rdm_simple.R`** (70 lines) - Primary data loader with sensible defaults
- **`load_rdm_complete.R`** - Complete loader preserving historical medians
- **`rdm_data_functions.R`** (877 lines) - REDCap API integration core

#### Milestone Processing
- **`milestone_functions.R`** (1,989 lines) - **LARGEST MODULE**: Spider plots, progression charts, data-dictionary-driven workflows
- **`milestone_data_workflow.R`** - Median calculations for all milestone forms
- **`milestone_mapping.R`** - Field mapping and competency detection
- **`milestone_module.R`** (759 lines) - Interactive Shiny milestone rating

#### Assessment & Visualization
- **`assessment_viz_functions.R`** (440 lines) - Combined assessment/faculty charts
- **`assessment_viz_module.R`** (431 lines) - Dashboard UI module
- **`assessment_helpers.R`** (414 lines) - Filtering and processing utilities

#### REDCap Submission
- **`redcap_submission.R`** (1,149 lines) - **SECOND LARGEST**: Complete submission workflow (OVERWRITE/ADDITIVE patterns)
- **`submit_self_eval_data.R`** - Self-evaluation submissions
- **`submission_patterns.R`** - Submission utilities

#### Shiny Modules
- **`mod_career_goals.R`** (523 lines) - Career planning with period comparison
- **`mod_plus_delta_table.R`** (485 lines) - Plus/Delta feedback display
- **`mod_questions_viz.R`** (453 lines) - Assessment questions visualization
- **`mod_assessment_detail_viz.R`** - Detailed assessment views
- **`mod_assessment_viz_wrapper.R`** - Composite assessment module

#### UI & Theming
- **`ui_components.R`** (386 lines) - Reusable components (cards, selectors, badges)
- **`theme_manager.R`** (80 lines) - SSM Health color palette and CSS loading

#### Period & Time
- **`period_mapping.R`** (487 lines) - Academic calendar, period calculations
- **`level_at_time_processor.R`** (454 lines) - Historical level determination
- **`period_calculation.R`** - Period determination utilities

---

## Development Workflow

### Package Development

This is a standard R package using **roxygen2** for documentation and **testthat** for testing.

#### Essential Commands

```r
# Load package for development
devtools::load_all()

# Generate documentation from roxygen comments
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()

# Install package locally
devtools::install()
```

### Git Workflow

#### Branch Naming Convention
All feature branches use the pattern: `claude/<description>-<session-id>`

**Examples from history:**
- `claude/fix-acgme-milestone-median-01H8sEaArnRrNyW9QHkVo6EZ`
- `claude/fix-milestone-period-hardcoding-01WV9W4Qqzxn2iazZbwEGwno`

#### Current Branch
Development is on: `claude/claude-md-mi0tk0fqjs4utb76-01HFM9JN8EnBYB3PJKhy7fYH`

#### Commit Message Style
Based on recent commits, messages should be:
- **Descriptive**: Clearly state what was changed
- **Fix-focused**: Start with "Fix" for bug fixes
- **Action-oriented**: Use imperative mood ("Add", "Update", "Fix")

**Examples:**
```
Fix ACGME milestone spider plot and progression chart period field detection
Add debug output to trace period column name through join operation
Fix create_milestone_workflow_from_dict to preserve original period field names
```

#### Git Operations

**Push with retry:**
```bash
# Always use -u flag for new branches starting with 'claude/'
git push -u origin <branch-name>

# If network errors occur, retry up to 4 times with exponential backoff (2s, 4s, 8s, 16s)
```

**Fetch specific branches:**
```bash
git fetch origin <branch-name>
# Retry on network failures with same backoff strategy
```

### REDCap Development

#### Environment Configuration

Required environment variables or `config.yml` entries:
- **`RDM_TOKEN`** - REDCap API token for main database
- **`FAC_TOKEN`** - REDCap API token for faculty evaluations
- **`ACCESS_CODE`** - Demo access code for testing

Default API URL: `https://redcapsurvey.slu.edu/api/`

#### Testing REDCap Connection

```r
# Test API connection
test_redcap_connection()

# Load data with defaults
app_data <- load_rdm_simple()

# Load complete data preserving historical medians
app_data <- load_rdm_complete()
```

---

## Code Conventions

### Naming Conventions

#### Functions
- **snake_case** throughout
- Descriptive names indicating purpose
- Examples: `calculate_milestone_medians()`, `process_summative_data()`, `get_current_period()`

#### Shiny Modules
- **UI function**: `mod_*_ui(id, ...)`
- **Server function**: `mod_*_server(id, ...)`
- All modules use namespace: `ns <- NS(id)`

#### Variables & Columns
- Lowercase with underscores
- REDCap field patterns:
  - **Program milestones**: `rep_pc1`, `rep_mk2`, `rep_sbp3`, etc.
  - **Self-eval milestones**: `rep_pc1_self`, `rep_mk2_self`, etc.
  - **ACGME milestones**: `acgme_pc1`, `acgme_mk2`, etc.
  - **Period fields**: Form-specific, e.g., `prog_mile_period`, `cc_period`
  - **Date fields**: Form-specific, e.g., `prog_mile_date`, `cc_date`

#### REDCap Patterns
- **Coach codes**: Numeric codes mapped to names via `convert_coach_codes_to_names()`
- **Assessment types**: `cc_intern_*`, `cc_pgy2_*`, `cc_pgy3_*` prefixes
- **Instances**: Mapped to periods (7 = Intern Intro, 1 = Mid Review, 2 = End Review)

### Roxygen Documentation

All exported functions must have roxygen2 documentation:

```r
#' @title Function Title
#' @description Detailed description of what the function does
#'
#' @param param1 Description of parameter 1
#' @param param2 Description of parameter 2
#'
#' @return Description of return value
#' @export
#'
#' @examples
#' \dontrun{
#'   result <- my_function(param1, param2)
#' }
```

### File Organization

Each R file should:
1. Start with roxygen2 title/description block
2. Use `NULL` name for file-level documentation
3. Group related functions with section headers
4. Include extensive comments for complex logic

**Example:**
```r
#' @title Milestone Assessment Functions for GMED - RDM 2.0 Version
#' @description Functions for milestone visualization, assessment, and data processing
#' @name milestone_functions
NULL

# ============================================================================
# CORE DATA PROCESSING FUNCTIONS
# ============================================================================
```

---

## Key Technical Patterns

### 1. Data Dictionary-Driven Development

**Core Philosophy**: Use REDCap's data dictionary to auto-detect field patterns rather than hardcoding.

```r
# Extract milestone configurations from data dictionary
configs <- extract_milestone_configs_from_dict(data_dict, verbose = TRUE)

# Auto-detect period and date columns
period_cols <- form_fields %>%
  filter(grepl("period", field_name, ignore.case = TRUE)) %>%
  pull(field_name)
```

**Why**: REDCap forms can have varying field names. Dictionary-driven approach is more robust.

### 2. Period Mapping System

Three period types across the academic year (July 1 - June 30):
- **Intern Intro** (July-September) → Instance 7
- **Mid Review** (October-February) → Instance 1
- **End Review** (March-June) → Instance 2

**Key Functions:**
```r
get_current_period()           # Current period based on system date
calculate_pgy_and_period()     # Resident level and period
map_to_milestone_period()      # Convert app period to instance
```

### 3. Historical Level Calculation

Assessments need to know the resident's level **at the time of assessment**, not current level.

```r
# Add historical level to assessment data
data_with_levels <- add_level_at_time_to_assessments(
  assessment_data = assessments,
  residents = residents
)
```

**Implementation**: `level_at_time_processor.R:454` calculates PGY level at any past date.

### 4. Median Calculation for Benchmarking

Historical medians are calculated **before** filtering archived residents.

```r
# Calculate medians for all milestone forms
medians <- calculate_all_milestone_medians(
  milestone_data = milestone_data,
  residents = residents,
  data_dict = data_dict
)
```

**Critical**: Filter archived residents AFTER calculating medians to preserve benchmark data.

### 5. REDCap Submission Patterns

Two primary patterns for data submission:

#### OVERWRITE Pattern
Replaces existing data for a specific instance.
```r
submit_overwrite_data(
  record_id = record_id,
  instance = instance,
  data = data_to_submit
)
```

#### ADDITIVE Pattern
Finds next available instance and adds new record.
```r
submit_additive_data(
  record_id = record_id,
  data = data_to_submit,
  form_name = form_name
)
```

**Location**: `redcap_submission.R:1149`

### 6. Shiny Module Pattern

Standard pattern for all Shiny modules:

```r
# UI Function
mod_example_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # UI elements with ns() wrapped IDs
    selectInput(ns("selector"), "Label", choices = ...)
  )
}

# Server Function
mod_example_server <- function(id, reactive_data) {
  moduleServer(id, function(input, output, session) {
    # Server logic
    # Return reactive values
    return(reactive({
      # computed value
    }))
  })
}
```

### 7. UI Component System

Reusable components with consistent SSM Health styling:

```r
# Card container
gmed_card(
  title = "Card Title",
  # content
)

# Selector container
gmed_selector_container(
  label = "Select Option",
  selectizeInput(...)
)

# Status badge
gmed_status_badge(status = "complete", text = "Complete")
```

**Location**: `ui_components.R:386`

### 8. Competency Framework

Six ACGME competencies with specific subcompetency counts:

| Code | Competency | Subcompetencies |
|------|-----------|-----------------|
| **PC** | Patient Care | 6 |
| **MK** | Medical Knowledge | 3 |
| **SBP** | Systems-Based Practice | 3 |
| **PBLI** | Practice-Based Learning | 2 |
| **PROF** | Professionalism | 4 |
| **ICS** | Interpersonal & Communication Skills | 3 |

**Pattern Detection:**
```r
# REP program: rep_pc1, rep_pc2, ..., rep_pc6, rep_mk1, ..., rep_ics3
# REP self: rep_pc1_self, rep_pc2_self, ...
# ACGME: acgme_pc1, acgme_pc2, ...
```

---

## Testing Strategy

### Framework
- **testthat** (edition 3)
- Configuration in `DESCRIPTION`: `Config/testthat/edition: 3`

### Test Location
- Unit tests: `tests/testthat/`
- Manual testing: `shiny_testing.R`, `data_load_demo.R`
- Module examples: `examples/test_assessment_module.R`

### Running Tests

```r
# Run all tests
devtools::test()

# Test specific file
testthat::test_file("tests/testthat/test-milestones.R")
```

### Testing with REDCap

For manual testing with live REDCap connection:

```r
# Set environment variables
Sys.setenv(RDM_TOKEN = "your_token")
Sys.setenv(FAC_TOKEN = "your_token")

# Load data
source("data_load_demo.R")

# Or run Shiny test app
source("shiny_testing.R")
```

**Important**: Never commit tokens or credentials. Use `.gitignore` (already configured).

---

## Common Tasks

### Adding a New Shiny Module

1. **Create the module file**: `R/mod_new_feature.R`

```r
#' @title New Feature Module
#' @description Module for new feature functionality
#' @name mod_new_feature
NULL

#' New Feature UI
#' @param id Module namespace ID
#' @export
mod_new_feature_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # UI elements
  )
}

#' New Feature Server
#' @param id Module namespace ID
#' @param reactive_param Reactive parameter
#' @export
mod_new_feature_server <- function(id, reactive_param) {
  moduleServer(id, function(input, output, session) {
    # Server logic
  })
}
```

2. **Document with roxygen2**:
```r
devtools::document()
```

3. **Add to NAMESPACE** (auto-generated by roxygen2)

4. **Test the module**: Create example in `examples/`

### Adding a New Milestone Field Pattern

1. **Update pattern detection** in `milestone_functions.R`:

```r
# Add new pattern to extract_milestone_configs_from_dict()
milestone_patterns <- c(
  "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$",
  "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$",
  "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$",
  "^new_pattern_here$"  # Add your pattern
)
```

2. **Add pattern-specific processing logic**

3. **Test with data dictionary**:
```r
configs <- extract_milestone_configs_from_dict(data_dict, verbose = TRUE)
```

### Adding a New Period Type

1. **Update period mapping** in `period_mapping.R`:

```r
create_universal_period_mapping <- function() {
  tibble::tribble(
    ~period_label,     ~instance, ~start_month, ~end_month,
    "Intern Intro",    7,         7,            9,
    "Mid Review",      1,         10,           2,
    "End Review",      2,         3,            6,
    "New Period",      8,         4,            5  # Add new period
  )
}
```

2. **Update period helpers** in `period_helpers.R`

3. **Test period calculations**:
```r
get_current_period()
calculate_pgy_and_period(grad_year = 2025)
```

### Adding a New UI Component

1. **Add to `ui_components.R`**:

```r
#' Create GMED Custom Component
#'
#' @param param1 Description
#' @export
gmed_custom_component <- function(param1, ...) {
  shiny::div(
    class = "gmed-custom-component",
    # Component structure
  )
}
```

2. **Add CSS** to `inst/www/css/components.css` if needed

3. **Document and export**:
```r
devtools::document()
```

### Fixing Period Field Detection Issues

**Recent pattern** (from commit history): Period field names can vary by form.

**Best practice**: Use data dictionary to detect period columns dynamically.

```r
# DON'T hardcode period field names
data <- data %>% select(record_id, prog_mile_period, ...)

# DO detect from data dictionary
period_cols <- form_fields %>%
  filter(grepl("period", field_name, ignore.case = TRUE)) %>%
  pull(field_name)

# Preserve original field names through joins
left_join(individual_data, median_data, by = c("original_period_col" = "period"))
```

**Reference**: Recent fixes in commits `80e89da` and `e693507`

---

## Important Considerations

### Security & Credentials

1. **Never commit credentials**
   - REDCap tokens belong in environment variables or `config.yml` (gitignored)
   - Access codes should be validated through `authenticate_resident()`

2. **Authentication pattern**:
```r
# Validate access code
resident_info <- authenticate_resident(
  access_code = access_code,
  residents = residents
)
```

### Performance Considerations

1. **Data Loading**
   - Use `load_rdm_simple()` for fast loading with defaults
   - Use `load_rdm_complete()` when historical medians are needed
   - Consider caching for Shiny apps with many users

2. **Large Datasets**
   - Milestone data can be large (6 competencies × 21 subcompetencies × residents × periods)
   - Use `filter()` early in processing pipelines
   - Consider pagination for DataTables

3. **Shiny Reactivity**
   - Use `reactive()` for computed values
   - Use `eventReactive()` for user-triggered updates
   - Debounce user inputs when appropriate

### REDCap API Best Practices

1. **Rate Limiting**: Be mindful of API call frequency
2. **Error Handling**: Always wrap API calls in `try()` or `tryCatch()`
3. **Data Validation**: Validate data structure before submission

```r
# Good pattern
result <- tryCatch(
  submit_to_redcap(data),
  error = function(e) {
    message("REDCap submission failed: ", e$message)
    return(NULL)
  }
)
```

### Documentation Standards

1. **All exported functions** must have roxygen2 docs
2. **Complex algorithms** should have inline comments
3. **Breaking changes** should be noted in commit messages
4. **Data dictionary references** should be documented for field mappings

### Milestone Data Quality

1. **Median Calculation Order**:
   - Calculate medians FIRST
   - Filter archived residents AFTER
   - Reason: Need historical data for benchmarking

2. **Period Field Handling**:
   - Never hardcode period field names
   - Use data dictionary detection
   - Preserve original field names through joins

3. **Competency Validation**:
   - Verify all 6 competencies are present
   - Check subcompetency counts match framework
   - Validate score ranges (typically 1-9 scale)

### Common Pitfalls to Avoid

1. **Hardcoding field names** - Use data dictionary detection
2. **Filtering before median calculation** - Calculate medians first
3. **Ignoring historical levels** - Use `add_level_at_time_to_assessments()`
4. **Forgetting namespace in Shiny modules** - Always use `ns()` for IDs
5. **Not handling missing data** - REDCap can have NAs, use `na.omit()` or filters
6. **Mixing period naming conventions** - Use mapping functions consistently

---

## Quick Reference

### Essential Functions

#### Data Loading
```r
app_data <- load_rdm_simple()              # Primary loader
app_data <- load_rdm_complete()            # With historical medians
```

#### Milestone Processing
```r
configs <- extract_milestone_configs_from_dict(data_dict)
medians <- calculate_all_milestone_medians(milestone_data, residents, data_dict)
plot <- create_enhanced_milestone_spider_plot(data, medians, competency = "PC")
```

#### Period/Level Calculation
```r
period <- get_current_period()
level <- calculate_resident_level(grad_year = 2025)
data <- add_level_at_time_to_assessments(assessment_data, residents)
```

#### REDCap Submission
```r
submit_overwrite_data(record_id, instance, data)
submit_additive_data(record_id, data, form_name)
```

#### UI Components
```r
gmed_card(title = "Title", ...)
gmed_selector_container(label = "Label", ...)
gmed_status_badge(status = "complete")
```

### Package Exports (156 Functions)

**Full list in**: `NAMESPACE` file

**Major categories**:
- Data Loading: 5 functions
- Milestone Processing: 15+ functions
- Assessment Visualization: 10+ functions
- Shiny Modules: 16 UI/server pairs
- REDCap Submission: 12+ functions
- UI Components: 10+ functions
- Utilities: 40+ helpers

### Color Palette (SSM Health)

```r
colors <- ssm_colors()

# Primary: #003d5c (Dark Blue)
# Secondary: #0066a1 (Standard Blue)
# Success: #00a651 (Green)
# Warning: #ff8c00 (Orange)
# Danger: #dc3545 (Red)
```

### File Size Reference

Largest modules to be aware of:
1. **`milestone_functions.R`** - 1,989 lines
2. **`redcap_submission.R`** - 1,149 lines
3. **`rdm_data_functions.R`** - 877 lines
4. **`milestone_module.R`** - 759 lines
5. **`datatable_components.R`** - 722 lines

---

## Resources

### Documentation
- **REDCap Reference**: `docs/redcap_reference_guide.md`
- **Package Manual**: Generated by `R CMD Rd2pdf` or `?function_name`
- **Examples**: `examples/` directory

### External Dependencies

**Core packages** (from DESCRIPTION):
- `dplyr` - Data manipulation
- `httr` - REDCap API calls
- `readr` - Data reading
- `jsonlite` - JSON handling
- `DT` - DataTables
- `reactable` - Reactive tables
- `htmltools` - HTML generation
- `plotly` - Interactive plots
- `rlang` - R language tools
- `config` - Configuration management

**Suggested packages**:
- `knitr`, `rmarkdown` - Documentation
- `testthat` - Testing framework

### Getting Help

When working on this codebase:
1. **Read the data dictionary reference** (`docs/redcap_reference_guide.md`)
2. **Check recent commits** for similar fixes (especially period/milestone related)
3. **Use `?function_name`** for documentation
4. **Test with demo data** before REDCap submission
5. **Consult this CLAUDE.md** for patterns and conventions

---

## Changelog

### 2025-11-15
- Initial creation of CLAUDE.md
- Comprehensive analysis of codebase structure
- Documentation of all 33 R modules
- Git workflow patterns from recent commits
- REDCap integration patterns documented
- UI component system documented
