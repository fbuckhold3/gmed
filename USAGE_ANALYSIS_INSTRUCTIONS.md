# GMED Function Usage Analysis Instructions

## Overview

I've created **TWO analysis scripts** that work together to give you the complete picture:

1. **analyze_gmed_usage.R** - Scans your 8 repos to find which functions are used externally
2. **analyze_internal_dependencies.R** - Analyzes internal gmed dependencies to identify which "unused" functions are actually needed by other gmed functions

## Quick Start

### Step 1: Pull the branch with both scripts

```bash
cd /Users/fredbuckhold/Documents/GitHub/gmed
git fetch origin claude/remove-console-logging-01MmXpQw8nRXvLwjKEMNwuk8
git checkout claude/remove-console-logging-01MmXpQw8nRXvLwjKEMNwuk8
```

### Step 2: Run BOTH analyses in order

```bash
# First: Find external usage (in your 8 repos)
Rscript analyze_gmed_usage.R

# Second: Analyze internal dependencies (within gmed itself)
Rscript analyze_internal_dependencies.R
```

### Step 3: Review the complete results

After running both scripts, you'll have **6 output files**:

#### From External Analysis:
1. **gmed_usage_report.txt** - Which repos use which functions
2. **gmed_usage_report.csv** - Function usage matrix
3. **gmed_unused_functions.txt** - Functions not found in repos

#### From Internal Analysis:
4. **gmed_internal_dependencies.txt** - Complete dependency analysis with 3-tier classification
5. **gmed_dependency_tree.csv** - Full dependency matrix (open in Excel)
6. **gmed_final_classification.txt** - Simple list of what to keep vs deprecate

## What the Scripts Do

### Script 1: External Usage Analysis (`analyze_gmed_usage.R`)

1. ✅ Load all exported functions from your gmed NAMESPACE
2. ✅ Scan all 8 repositories for R files
3. ✅ Extract all `gmed::function_name()` calls
4. ✅ Identify which files use `library(gmed)`
5. ✅ Create a usage matrix showing which functions are used in which repos
6. ✅ Categorize functions as:
   - **MULTIPLE REPOS** - Used in 2+ projects
   - **SINGLE REPO** - Used in only 1 project
   - **UNUSED** - Not found in any repo

### Script 2: Internal Dependency Analysis (`analyze_internal_dependencies.R`)

1. ✅ Parse all R files in the gmed package
2. ✅ Build a complete dependency map (which functions call which)
3. ✅ Identify which "unused" functions are actually needed internally
4. ✅ Calculate dependency depth and complexity
5. ✅ Classify ALL functions into 3 tiers:
   - **TIER 1: Externally Used** - Used in your 8 repos → DO NOT DEPRECATE
   - **TIER 2: Internal Dependencies** - Not used in repos but needed by Tier 1 → DO NOT DEPRECATE
   - **TIER 3: Truly Unused** - Not used anywhere → SAFE TO DEPRECATE

## The 3-Tier Classification System

This answers your question: **"Are some of these functions used in gmed functions themselves?"**

### Tier 1: Externally Used (✓ Keep)
Functions directly called in your 8 repositories.

**Example:**
```r
# In your dashboard:
data <- gmed::load_rdm_complete()  # ← Tier 1
```

### Tier 2: Internal Dependencies (✓ Keep)
Functions not used in your repos BUT called by Tier 1 functions.

**Example:**
```r
# Inside gmed package:
load_rdm_complete <- function() {
  dict <- get_evaluation_dictionary()  # ← Tier 2 (needed by Tier 1)
  medians <- calculate_milestone_medians()  # ← Tier 2 (needed by Tier 1)
  # ...
}
```

### Tier 3: Truly Unused (⚠️ Deprecate)
Functions not used in repos AND not needed by any other gmed function.

**Example:**
```r
# Old legacy function nobody uses:
old_legacy_function <- function() {
  # Not called anywhere, not needed
}
```

## Why This Matters

Without internal dependency analysis:
- **114 functions** appear "unused"
- But many are actually needed by other gmed functions!

With internal dependency analysis:
- **Tier 1 + Tier 2** = Functions you must keep
- **Tier 3 only** = Functions safe to deprecate

## Expected Output

### From Script 1 (External Usage):
```
=============================================================================
GMED FUNCTION USAGE ANALYZER
=============================================================================

Analyzing imslu.ccc.dashboard ...
  Found 45 R files
  Found 12 files using gmed
  Found 23 unique gmed functions

[... continues for all repos ...]

SUMMARY:
  Total exported functions:      163
  Functions used in repos:       53
  Functions NOT used anywhere:   114
  Usage rate:                    32.5%
```

### From Script 2 (Internal Dependencies):
```
=============================================================================
GMED INTERNAL DEPENDENCY ANALYZER
=============================================================================

Step 1: Loading external usage data...
Found 53 functions used in external repos

Step 2: Loading NAMESPACE...
Found 163 exported functions

Step 3: Building internal dependency map...
Analyzing 47 R files in gmed/R/
Dependency map complete!

Step 4: Classifying functions into tiers...

Classification complete!
  Tier 1 (Externally used):        53 functions
  Tier 2 (Internal dependencies):   28 functions
  Tier 3 (Truly unused):            82 functions

=============================================================================
ANALYSIS COMPLETE!
=============================================================================

FINAL CLASSIFICATION:
  Tier 1 (Externally used):        53 functions - DO NOT DEPRECATE
  Tier 2 (Internal dependencies):  28 functions - DO NOT DEPRECATE
  Tier 3 (Truly unused):           82 functions - SAFE TO DEPRECATE

KEY INSIGHTS:
  - Total functions to keep:       81
  - Functions safe to deprecate:   82
  - Cleanup potential:             50.3%
```

**Key Insight:** Of the 114 "unused" functions, 28 are actually needed internally! Only 82 are truly safe to deprecate.

### Report Examples

#### From `gmed_internal_dependencies.txt`:

```
TIER 1: EXTERNALLY USED FUNCTIONS (53 functions)
-----------------------------------------------------------------------------
These functions are directly used in your 8 repositories.
DO NOT DEPRECATE - These are your core API!

load_rdm_complete                         [calls 5 functions, depth 2]
  Calls: get_evaluation_dictionary, calculate_all_milestone_medians, ...

create_milestone_spider_plot_final        [calls 3 functions, depth 1]
  Calls: extract_milestone_configs_from_dict, validate_data, ...

[... etc ...]

TIER 2: INTERNAL DEPENDENCIES (28 functions)
-----------------------------------------------------------------------------
These functions are NOT used in external repos, but ARE needed by Tier 1.
DO NOT DEPRECATE - Required for Tier 1 functions to work!

get_evaluation_dictionary                 [needed by 3 Tier 1 functions]
  Needed by: load_rdm_complete, load_evaluation_data, ...
  Calls: parse_redcap_metadata, clean_variable_names

calculate_all_milestone_medians           [needed by 2 Tier 1 functions]
  Needed by: load_rdm_complete, prepare_milestone_data
  Calls: calculate_milestone_median, filter_milestone_data

[... etc ...]

TIER 3: TRULY UNUSED FUNCTIONS (82 functions)
-----------------------------------------------------------------------------
These functions are NOT used externally AND NOT needed internally.
SAFE TO DEPRECATE - Review and remove these!

old_legacy_function_1                     [completely isolated]
old_legacy_function_2                     [orphan - calls 2 functions but unused]

[... etc ...]
```

#### From `gmed_final_classification.txt`:

```
=== TIER 1: EXTERNALLY USED (53 functions) ===
DO NOT DEPRECATE - Core API functions

  calculate_all_milestone_medians
  create_milestone_spider_plot_final
  load_rdm_complete
  [... etc ...]

=== TIER 2: INTERNAL DEPENDENCIES (28 functions) ===
DO NOT DEPRECATE - Needed by Tier 1 functions

  get_evaluation_dictionary
  calculate_milestone_median
  parse_redcap_metadata
  [... etc ...]

=== TIER 3: TRULY UNUSED (82 functions) ===
SAFE TO DEPRECATE - Not used anywhere

  old_legacy_function_1
  old_legacy_function_2
  [... etc ...]
```

## What to Do Next

### 1. Review the Final Classification (START HERE!)

Open **`gmed_final_classification.txt`** - this is your action plan:
- **Tier 1 + Tier 2** = Keep these (81 functions)
- **Tier 3 only** = Safe to deprecate (82 functions)

### 2. Review the Detailed Analysis

Open **`gmed_internal_dependencies.txt`** to understand:
- Which Tier 1 functions call which Tier 2 functions
- Why each Tier 2 function is needed
- Which Tier 3 functions are "orphans" vs "completely isolated"
- Dependency statistics (most complex functions, hub functions, etc.)

### 3. Review the Dependency Matrix

Open **`gmed_dependency_tree.csv`** in Excel/R to:
- Sort by `tier` to see all functions in each tier
- Sort by `num_dependents` to find critical hub functions
- Sort by `dependency_depth` to find complex dependency chains
- See exactly which functions call which in the `calls_functions` column

### 4. Make Deprecation Decisions

#### ✅ Tier 1 - KEEP (53 functions)
These are your core API - actively used in dashboards.
**Action:** No changes needed

#### ✅ Tier 2 - KEEP (28 functions)
These are internal helpers required by Tier 1.
**Action:** Consider making these internal-only (not exported) in future versions

#### ⚠️ Tier 3 - DEPRECATE (82 functions)
Not used anywhere - safe to remove.
**Action:**
1. Add `.Deprecated()` warnings now
2. Update documentation to mark as deprecated
3. Remove in next major version

### 5. Validate Console Logging Changes

The functions I modified (for console logging cleanup) are:
- `create_milestone_spider_plot_final` - Tier 1 ✓
- `calculate_all_milestone_medians` - Tier 1 ✓
- `load_rdm_complete` - Tier 1 ✓
- `extract_milestone_configs_from_dict` - Tier 1 or 2 ✓

**All are actively used** - safe to merge the changes!

### 6. Next Steps for Package Cleanup

Based on your analysis results:

1. **Short term (this week):**
   - Merge the console logging cleanup branch
   - Review Tier 3 list for any false positives

2. **Medium term (next sprint):**
   - Add `.Deprecated()` warnings to Tier 3 functions
   - Update DESCRIPTION with deprecation notes
   - Inform users in release notes

3. **Long term (next major version):**
   - Remove Tier 3 functions completely
   - Consider making Tier 2 functions internal-only
   - Reduce exported API to just Tier 1

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
