# GMED Function Usage Analysis Instructions

## Overview

I've created `analyze_gmed_usage.R` - a comprehensive script that will scan all your local repositories and identify exactly which gmed functions are being used.

## Quick Start

### Step 1: Copy the script to your gmed directory

```bash
# The script is ready in the cloud environment
# You'll need to pull it from the branch I created
cd /Users/fredbuckhold/Documents/GitHub/gmed
git fetch origin claude/remove-console-logging-01MmXpQw8nRXvLwjKEMNwuk8
git checkout claude/remove-console-logging-01MmXpQw8nRXvLwjKEMNwuk8
```

### Step 2: Run the analysis

```bash
# From your gmed directory
Rscript analyze_gmed_usage.R
```

### Step 3: Review the results

The script will generate three files:

1. **gmed_usage_report.txt** - Comprehensive human-readable report
2. **gmed_usage_report.csv** - Spreadsheet with function usage matrix
3. **gmed_unused_functions.txt** - Simple list of functions not found in any repo

## What the Script Does

The script will:

1. ✅ Load all 173 exported functions from your gmed NAMESPACE
2. ✅ Scan all 8 repositories for R files
3. ✅ Extract all `gmed::function_name()` calls
4. ✅ Identify which files use `library(gmed)`
5. ✅ Create a usage matrix showing which functions are used in which repos
6. ✅ Categorize functions as:
   - **MULTIPLE REPOS** - Used in 2+ projects (keep these!)
   - **SINGLE REPO** - Used in only 1 project (review these)
   - **UNUSED** - Not found in any repo (candidates for deprecation)

## Expected Output

### Console Output
```
=============================================================================
GMED FUNCTION USAGE ANALYZER
=============================================================================

Checking gmed directory...
Loading gmed NAMESPACE...
Found 173 exported functions in gmed

Analyzing repositories...
=============================================================================

Analyzing imslu.ccc.dashboard ...
  Found 45 R files
  Found 12 files using gmed
  Found 23 unique gmed functions

[... continues for all repos ...]

=============================================================================
ANALYSIS COMPLETE!
=============================================================================

SUMMARY:
  Total exported functions:      173
  Functions used in repos:       51
  Functions NOT used anywhere:   122
  Usage rate:                    29.5%

OUTPUT FILES:
  1. gmed_usage_report.txt - Detailed text report
  2. gmed_usage_report.csv - CSV matrix of function usage
  3. gmed_unused_functions.txt - List of unused functions
```

### Report Example

The `gmed_usage_report.txt` will show:

```
REPOSITORY ANALYSIS
-----------------------------------------------------------------------------

imslu.ccc.dashboard
  R files found:              45
  Files using gmed:           12
  Unique gmed functions used: 23

  Functions used:
    - load_rdm_complete
    - create_milestone_spider_plot_final
    - extract_milestone_configs_from_dict
    [... etc ...]

-----------------------------------------------------------------------------

UNUSED FUNCTIONS (122 functions)
-----------------------------------------------------------------------------
The following functions are exported but not found in any analyzed repo:

  - old_function_1
  - legacy_helper_2
  [... etc ...]

RECOMMENDATION: Review these functions for potential deprecation.
```

## What to Do Next

### 1. Review the Text Report
Open `gmed_usage_report.txt` and read through:
- Which repos use which functions
- Functions used in multiple repos (these are critical!)
- Functions used in only one repo (might be project-specific)
- Unused functions (candidates for deprecation)

### 2. Review the CSV in Excel/R
Open `gmed_usage_report.csv` for an interactive view:
- Sort by `total_repos` column to see most-used functions
- Filter by `status` to see UNUSED, SINGLE REPO, or MULTIPLE REPOS
- Each repo has a column showing ✓ if the function is used

### 3. Make Decisions

#### ✅ SAFE TO KEEP (Functions used in multiple repos)
These are your core functions - **do NOT deprecate**

#### ⚠️ REVIEW NEEDED (Functions used in single repo)
- If the repo is important → keep the function
- If the repo is archived/legacy → consider deprecation
- If easily replaced → consider deprecation

#### 🚨 CANDIDATES FOR DEPRECATION (Unused functions)
Before removing, check:
- Is it used in internal scripts not in these 8 repos?
- Is it part of a public API that external users might use?
- Is it recently added and just not adopted yet?
- Is it a helper that shouldn't have been exported?

### 4. Compare with My Previous Analysis

I previously analyzed these repos via GitHub's web interface and found:
- **51 actively used functions** (web analysis)
- **94 potentially unused functions** (web analysis)

The local analysis should be more accurate because it can:
- See uncommitted local changes
- Search through all file types (.R, .Rmd, etc.)
- Find usage in commented code or development branches

### 5. Validate Console Logging Changes

After you review the usage analysis, you can confidently merge the changes I made because:
- I only modified functions that ARE being used (based on web analysis)
- The changes are backward compatible (just cleaner output)
- No function signatures were changed
- All changes are in the `verbose` parameter handling

## Troubleshooting

### "Directory not found" errors
If you see errors about directories not being found, update the script's `REPOS` list to match your actual directory names.

### "No gmed:: calls found"
Some repos might use `library(gmed)` and call functions directly without `gmed::`. The script handles this by:
- Detecting `library(gmed)` usage
- Flagging files that load gmed
- You may need to manually review these files for implicit function calls

### Script takes a long time
This is normal! The script is scanning thousands of files. Expect:
- Small repos (<50 R files): ~5 seconds
- Medium repos (50-200 R files): ~15 seconds
- Large repos (200+ R files): ~30 seconds
- **Total for 8 repos: ~2-5 minutes**

## Configuration

If your directory structure is different, edit the top of `analyze_gmed_usage.R`:

```r
# Configuration
GITHUB_DIR <- "/Users/fredbuckhold/Documents/GitHub"  # Change this if needed
GMED_DIR <- file.path(GITHUB_DIR, "gmed")

REPOS <- c(
  "imslu.ccc.dashboard",           # Change repo names if needed
  "imslu-resident-self-assessment",
  # ... etc
)
```

## Questions?

After running the analysis, share the `gmed_usage_report.txt` with me and I can help you:
- Interpret the results
- Decide which functions to deprecate
- Create a deprecation plan
- Update the gmed package accordingly
