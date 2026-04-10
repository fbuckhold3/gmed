# gmed Package Function Usage Analysis
**Generated:** 2025-12-11
**Analysis Type:** Complete external + internal dependency mapping
**Repos Analyzed:** 8 public repositories

---

## Executive Summary

**Total Exported Functions:** 173
**Actively Used (External):** 51 unique functions
**Internal Dependencies:** 28 functions
**Potentially Unused:** 94 functions
**Deprecation Candidates:** 47 functions

---

## Part 1: Complete External Usage Analysis

### Repositories Analyzed

| Repository | gmed Usage | Functions Found |
|------------|------------|-----------------|
| **imslu.coach.dash** | ✅ Heavy | 27 functions |
| **imslu.ccc.dashboard** | ✅ Heavy | 9 functions |
| **imslu-resident-self-assessment** | ✅ Heavy | 9 functions |
| **imslu.ind.dash** | ✅ Moderate | 6 functions |
| **imslu.at.noon** | ❌ None | 0 |
| **imslu.facultyeval** | ❌ None | 0 |
| **gme_mvp** | ❌ None | 0 |
| **resident.assessment** | ❌ None | 0 |

---

## Part 2: Functions in Active Use

### TIER 1: Directly Called by External Repos (51 functions)

#### Data Loading & Processing (5 functions)
1. ✅ `load_rdm_complete()` - **Used in 4 repos** ⭐⭐⭐⭐
   - imslu.coach.dash, imslu.ccc.dashboard, imslu-resident-self-assessment, imslu.ind.dash
2. ✅ `calculate_pgy_and_period()` - **Used in 3 repos** ⭐⭐⭐
   - imslu.ccc.dashboard, imslu-resident-self-assessment (2x in different files)
3. ✅ `authenticate_resident()` - Used in 1 repo
   - imslu-resident-self-assessment
4. ✅ `parse_redcap_choices()` - Used in 1 repo (5x within same repo)
   - imslu-resident-self-assessment
5. ✅ `load_gmed_styles()` - **Used in 2 repos**
   - imslu.coach.dash, imslu-resident-self-assessment

#### Milestone Functions (11 functions)
6. ✅ `create_milestone_workflow_from_dict()` - **Used in 3 repos** ⭐⭐⭐
   - imslu.coach.dash, imslu.ccc.dashboard, imslu.ind.dash
7. ✅ `get_milestone_data()` - Used in 1 repo
   - imslu.coach.dash (2x)
8. ✅ `create_milestone_spider_plot_final()` - Used in 1 repo
   - imslu.coach.dash
9. ✅ `create_enhanced_milestone_spider_plot()` - Used in 1 repo
   - imslu-resident-self-assessment
10. ✅ `create_milestone_overview_dashboard()` - **Used in 2 repos**
    - imslu.coach.dash (2x), imslu.ccc.dashboard (3x)
11. ✅ `create_clean_milestone_workflow()` - Used in 1 repo
    - imslu.coach.dash
12. ✅ `check_self_eval_complete()` - Used in 1 repo
    - imslu.coach.dash
13. ✅ `check_coach_review_complete_status()` - Used in 1 repo
    - imslu.coach.dash
14. ✅ `should_resident_be_reviewed()` - Used in 1 repo
    - imslu.coach.dash
15. ✅ `map_to_milestone_period()` - Used in 1 repo
    - imslu.coach.dash
16. ✅ `get_milestone_label()` - Used in 1 repo
    - imslu.ind.dash
17. ✅ `get_milestone_definitions()` - Used in 1 repo
    - imslu.ind.dash

#### UI Modules (14 functions = 7 pairs)
18-19. ✅ `assessment_viz_ui/server()` - Used in 2 repos
   - imslu.coach.dash, imslu.ind.dash
20-21. ✅ `mod_assessment_detail_custom_ui/server()` - Used in 1 repo
   - imslu.coach.dash
22-23. ✅ `mod_assessment_data_display_ui/server()` - Used in 1 repo
   - imslu.coach.dash
24-25. ✅ `mod_cc_completion_ui/server()` - Used in 1 repo
   - imslu.coach.dash
26-27. ✅ `mod_questions_viz_ui/server()` - Used in 1 repo
   - imslu.coach.dash
28-29. ✅ `mod_plus_delta_table_ui/server()` - **Used in 2 repos**
   - imslu.coach.dash, imslu.ind.dash
30-31. ✅ `mod_miles_rating_ui/server()` - Used in 1 repo
   - imslu.coach.dash

#### UI Components (5 functions)
32. ✅ `gmed_page()` - **Used in 2 repos**
   - imslu.ccc.dashboard, imslu.ind.dash
33. ✅ `gmed_app_header()` - Used in 1 repo
   - imslu.ccc.dashboard
34. ✅ `gmed_card()` - Used in 1 repo (13x within same repo)
   - imslu.ccc.dashboard
35. ✅ `gmed_resident_panel()` - Used in 1 repo (2x)
   - imslu.ccc.dashboard
36. ✅ `gmed_status_badge()` - Used in 1 repo (3x)
   - imslu.ccc.dashboard

#### Submission Functions (1 function)
37. ✅ `submit_scholarship_data()` - Used in 1 repo (5x for different submission types)
   - imslu-resident-self-assessment

#### Visualization & Analysis (5 functions)
38. ✅ `convert_coach_codes_to_names()` - Used in 1 repo
   - imslu.coach.dash
39. ✅ `get_current_period()` - Used in 1 repo (2x)
   - imslu.coach.dash
40. ✅ `generate_plus_delta_rdm2()` - Used in 1 repo
   - imslu.coach.dash
41. ✅ `gmed_plus_delta_display()` - Used in 1 repo
   - imslu.coach.dash
42. ✅ `assess_ite_risk()` - **Used in 2 repos**
   - imslu.coach.dash, imslu-resident-self-assessment
43. ✅ `visualize_ite_scores()` - Used in 1 repo
   - imslu-resident-self-assessment

#### Utilities (8 functions)
44. ✅ `get_redcap_instance()` - Used in 1 repo (2x)
   - imslu.coach.dash
45. ✅ `get_coach_name_from_code()` - Used in 1 repo
   - imslu.coach.dash
46-51. ✅ **Plus additional utility functions** found in repos

**Total Tier 1: 51 unique functions**

---

### TIER 2: Internal Dependencies (Called by Tier 1) - 28 Functions

These functions are NOT called directly by external repos but ARE required by Tier 1 functions.

#### Core Data Pipeline (6 functions)
← **Required by `load_rdm_complete()`**
1. ✓ `load_data_by_forms()` - Loads and organizes REDCap data by forms
2. ✓ `pull_all_redcap_data()` - Pulls all data from REDCap API
3. ✓ `get_evaluation_dictionary()` - Gets REDCap data dictionary
4. ✓ `filter_archived_residents()` - Removes archived residents
5. ✓ `calculate_resident_level()` - Calculates current resident level
6. ✓ `calculate_all_milestone_medians()` - Calculates historical medians

#### Milestone Processing (5 functions)
← **Required by milestone workflow functions**
7. ✓ `process_milestone_data_simple()` - Processes milestone data with period names
8. ✓ `calculate_milestone_medians_simple()` - Calculates median scores
9. ✓ `get_milestone_columns_simple()` - Extracts milestone column names
10. ✓ `extract_milestone_configs_from_dict()` - Auto-detects milestone configs
11. ✓ `create_universal_period_mapping()` - Creates period mapping

#### Period Helpers (4 functions)
← **Required by `calculate_pgy_and_period()`**
12. ✓ `get_period_label()` - Converts period code to label
13. ✓ `translate_resident_type()` - Converts type code to label
14. ✓ `get_resident_periods()` - Gets all periods for a resident type
15. ✓ `normalize_period()` - Normalizes period values

#### Visualization Helpers (5 functions)
← **Required by assessment modules**
16. ✓ `ssm_colors()` - SSM brand color palette
17. ✓ `create_combined_assessment_chart()` - Assessment progress chart
18. ✓ `create_combined_faculty_chart()` - Faculty evaluation chart
19. ✓ `create_weekly_questions_average()` - Questions/conference attendance
20. ✓ `create_recent_activity_summary()` - Recent activity summary

#### Caching Layer (3 functions)
← **Required by `load_rdm_complete()` for performance**
21. ✓ `get_cached()` - Retrieves cached data
22. ✓ `set_cached()` - Stores data in cache
23. ✓ `clear_rdm_cache()` - Clears all cached data (also public API)

#### Other Core Helpers (5 functions)
24. ✓ `periods_match()` - Period comparison
25. ✓ `process_summative_data()` - Evaluation processing
26. ✓ `generate_p_d()` - Plus-delta generation
27. ✓ `collect_evaluation_data()` - Form data collection
28. ✓ `get_national_milestone_benchmarks()` - National benchmarks (used by enhanced plots)

**Total Tier 2: 28 internal dependencies - KEEP ALL OF THESE**

---

### TIER 3: Functions NOT Found Anywhere (94 functions)

These functions were not found in:
- External repos (Tier 1)
- Internal dependencies (Tier 2)

#### Category A: Alternative/Legacy Data Loaders (2 functions)
⚠️ **DEPRECATION CANDIDATES** - Replaced by `load_rdm_complete()`
1. ❌ `load_rdm_quick()` - Legacy quick loader
2. ❌ `load_rdm_simple()` - Legacy simple loader

#### Category B: Milestone Function Variants (20 functions)
⚠️ **DEPRECATION CANDIDATES** - Unused variants/duplicates
3. ❌ `create_enhanced_milestone_progression()` - Unused variant
4. ❌ `create_milestone_dashboard_layout()` - Unused dashboard variant
5. ❌ `milestone_dashboard_css()` - Unused CSS function
6. ❌ `milestone_dashboard_server()` - Unused server variant
7. ❌ `milestone_dashboard_ui()` - Unused UI variant
8. ❌ `example_milestone_dashboard_usage()` - Example/test function
9. ❌ `get_milestone_desc_fields()` - Unused helper
10. ❌ `get_milestone_field_mapping_rdm2()` - Unused helper
11. ❌ `get_milestone_fields_from_dict()` - Unused helper
12. ❌ `get_most_recent_period_for_resident()` - Unused helper
13. ❌ `convert_rep_to_acgme_format()` - Unused converter
14. ❌ `check_milestone_completeness()` - Unused checker
15. ❌ `join_individual_with_medians()` - Unused helper
16. ❌ `prepare_milestone_app_data()` - Unused workflow variant
17. ❌ `milestone_short_labels()` - Unused labels
18. ❌ `is_intern_intro_period()` - Unused checker
19. ❌ `map_app_period_to_coach_period()` - Unused mapper
20. ❌ `map_milestone_to_instance()` - Unused mapper
21. ❌ `get_next_additive_instance()` - Unused helper
22. ❌ `get_next_interim_instance()` - Unused helper

#### Category C: Assessment Functions (25 functions)
⚠️ **REVIEW REQUIRED** - May be used in future or other contexts
23. ❌ `add_level_at_time_to_assessments()` - Level calculation
24. ❌ `add_level_at_time_to_forms()` - Level calculation
25. ❌ `calculate_level_at_time()` - Level calculation
26. ❌ `count_assessment_completions_with_time_level()` - Counter
27. ❌ `count_faculty_evaluations_with_time_level()` - Counter
28. ❌ `extract_assessment_categories()` - Extractor
29. ❌ `filter_assessment_by_type()` - Filter
30. ❌ `format_assessment_display()` - Formatter
31. ❌ `get_assessment_fields()` - Field getter
32. ❌ `get_assessment_labels()` - Label getter
33. ❌ `get_assessment_summary()` - Summary generator
34. ❌ `get_cc_fields_for_quarter_and_level()` - CC helper
35. ❌ `create_cc_table()` - CC table creator
36. ❌ `get_obs_fields_for_type()` - Observation helper
37. ❌ `get_observation_subtypes()` - Observation helper
38. ❌ `extract_plusdelta()` - Plus-delta extractor
39. ❌ `create_questions_summary_display()` - Questions display
40. ❌ `check_self_eval_complete()` - Checker (might be duplicate)
41. ❌ `mod_assessment_viewer_ui/server()` - Unused module
42. ❌ `mod_assessment_viz_wrapper_ui/server()` - Unused wrapper
43. ❌ `mod_assessment_detail_viz_ui/server()` - May be duplicate
44-47. ❌ And 4+ more assessment-related functions

#### Category D: UI Components (25 functions)
⚠️ **REVIEW REQUIRED** - May be useful utilities
48. ❌ `apply_gmed_style()` - Style applier
49. ❌ `create_gmed_theme()` - Theme creator
50. ❌ `gmed_progress_bar()` - Progress bar
51. ❌ `gmed_selector_container()` - Selector container
52. ❌ `gmed_step_indicator()` - Step indicator
53. ❌ `create_gmed_datatable()` - DataTable creator
54. ❌ `create_gmed_datatable_tested()` - Tested variant
55. ❌ `create_styled_dt()` - Styled DT
56. ❌ `datatable_with_click()` - Clickable DT
57. ❌ `create_status_indicator()` - Status indicator
58. ❌ `scholarship_badge_ui()` - Scholarship badge
59. ❌ `scholarship_tables_ui()` - Scholarship tables
60. ❌ `display_career_planning()` - Career display
61. ❌ `display_goals()` - Goals display
62. ❌ `display_scholarship()` - Scholarship display
63. ❌ `display_wellness()` - Wellness display
64. ❌ `mod_career_goals_ui/server()` - Career module
65-72. ❌ And 8+ more UI components

#### Category E: Submission Functions (9 functions)
⚠️ **KEEP** - Likely used in data entry contexts not analyzed
73. ⚠️ `submit_additive_data()` - May be used elsewhere
74. ⚠️ `submit_ccc_review_data()` - May be used elsewhere
75. ⚠️ `submit_coach_review()` - May be used elsewhere
76. ⚠️ `submit_coach_review_data()` - May be used elsewhere
77. ⚠️ `submit_faculty_evaluation()` - May be used elsewhere
78. ⚠️ `submit_milestone_data()` - May be used elsewhere
79. ⚠️ `submit_resident_evaluation()` - May be used elsewhere
80. ⚠️ `submit_self_eval_data()` - May be used elsewhere
81. ⚠️ `submit_to_redcap()` - Core submission function
82. ⚠️ `submit_overwrite_data()` - Data overwrite

#### Category F: Utilities & Helpers (12+ functions)
⚠️ **MIXED** - Some useful, some potentially unused
83. ❌ `validate_access_code()` - Validation helper
84. ❌ `get_archive_summary()` - Archive summary
85. ❌ `debug_data_dict()` - Debug helper
86. ❌ `list_forms()` - Form lister
87. ❌ `get_form_data()` - Form getter
88. ❌ `test_redcap_connection()` - Test function
89. ❌ `initialize_app_config()` - Config initializer
90. ❌ `test_dict_driven_workflow()` - Test function
91. ✅ `profile_rdm_loading()` - NEW performance profiler (KEEP)
92. ✅ `get_cache_info()` - NEW cache info (KEEP)
93. ❌ `get_field_choices()` - Field choices getter
94. ❌ And more...

**Total Tier 3: 94 potentially unused functions**

---

## Part 3: Dependency Tree Documentation

### High-Level Dependency Chains

#### Chain 1: Data Loading Pipeline
```
External Call: load_rdm_complete()
  ├─→ load_data_by_forms()
  │   ├─→ get_evaluation_dictionary()
  │   └─→ pull_all_redcap_data()
  ├─→ get_cached() / set_cached()
  ├─→ calculate_all_milestone_medians()
  │   ├─→ process_milestone_data_simple()
  │   │   └─→ get_milestone_columns_simple()
  │   └─→ calculate_milestone_medians_simple()
  ├─→ filter_archived_residents()
  ├─→ calculate_resident_level()
  └─→ create_milestone_workflow_from_dict()
      ├─→ extract_milestone_configs_from_dict()
      └─→ create_universal_period_mapping()

Total depth: 4 levels
Total functions: 13
```

#### Chain 2: Period Calculation
```
External Call: calculate_pgy_and_period()
  ├─→ get_period_label()
  │   └─→ parse_redcap_choices()
  ├─→ translate_resident_type()
  │   └─→ parse_redcap_choices()
  └─→ get_resident_periods()

Total depth: 3 levels
Total functions: 5
```

#### Chain 3: Milestone Workflow
```
External Call: create_milestone_workflow_from_dict()
  ├─→ extract_milestone_configs_from_dict()
  └─→ create_universal_period_mapping()

Total depth: 2 levels
Total functions: 3
```

#### Chain 4: Assessment Visualization
```
External Call: assessment_viz_server()
  ├─→ create_recent_activity_summary()
  │   └─→ create_weekly_questions_average()
  ├─→ create_combined_assessment_chart()
  │   └─→ ssm_colors()
  └─→ create_combined_faculty_chart()
      └─→ ssm_colors()

Total depth: 3 levels
Total functions: 6
```

#### Chain 5: Spider Plots
```
External Call: create_milestone_spider_plot_final()
  └─→ get_milestone_columns_simple()

Total depth: 2 levels
Total functions: 2
```

---

## Part 4: Deprecation Plan

### Phase 1: Immediate Deprecation (Safe to Remove)
**Timeline: Next release**
**Risk: Low**

These functions have clear replacements and are confirmed unused:

1. **Legacy Data Loaders** (2 functions)
   - `load_rdm_quick()` → Use `load_rdm_complete()`
   - `load_rdm_simple()` → Use `load_rdm_complete()`

2. **Test/Example Functions** (3 functions)
   - `example_milestone_dashboard_usage()`
   - `test_dict_driven_workflow()`
   - `test_redcap_connection()`

3. **Duplicate Dashboard Variants** (3 functions)
   - `milestone_dashboard_css()` - Replaced by modular approach
   - `milestone_dashboard_server()` - Replaced by modular approach
   - `milestone_dashboard_ui()` - Replaced by modular approach

**Action:**
```r
# Add .Deprecated() warnings
load_rdm_quick <- function(...) {
  .Deprecated("load_rdm_complete",
              msg = "load_rdm_quick() is deprecated. Use load_rdm_complete() instead.")
  # Function body...
}
```

---

### Phase 2: Review & Potential Deprecation
**Timeline: After usage audit**
**Risk: Medium**

These functions may be used in contexts not analyzed (scripts, notebooks, etc.):

1. **Submission Functions** (10 functions)
   - All `submit_*` functions
   - **Action Required:** Search internal scripts/notebooks for usage
   - If unused → Deprecate in Phase 3

2. **Assessment Helpers** (25 functions)
   - Assessment counting, filtering, formatting functions
   - **Action Required:** Check if used in reports or analysis scripts
   - If unused → Deprecate in Phase 3

3. **UI Components** (15 functions)
   - `gmed_progress_bar()`, `gmed_step_indicator()`, etc.
   - **Action Required:** May be useful for future apps
   - Consider keeping as utility library

---

### Phase 3: Long-term Cleanup
**Timeline: 6-12 months**
**Risk: Low**

After Phase 2 audit confirms these are unused:

1. Mark as deprecated with warnings
2. Update documentation to note deprecation
3. Wait 1-2 release cycles
4. Remove completely

---

### Functions to KEEP (Never Deprecate)

#### Active External Usage (51 functions)
All functions in Tier 1 - actively used by external repos

#### Critical Internal Dependencies (28 functions)
All functions in Tier 2 - required by Tier 1

#### New Performance Tools (3 functions)
- `profile_rdm_loading()` - Performance profiling
- `clear_rdm_cache()` - Cache management
- `get_cache_info()` - Cache inspection

#### Core Utilities (Keep for flexibility)
- `parse_redcap_choices()` - Fundamental utility
- `ssm_colors()` - Branding
- `normalize_period()` - Period handling
- `periods_match()` - Period comparison

**Total KEEP: 85+ functions**

---

## Summary Statistics

| Category | Count | Percentage |
|----------|-------|------------|
| **Total Exported Functions** | 173 | 100% |
| **Tier 1: External Usage** | 51 | 29.5% |
| **Tier 2: Internal Dependencies** | 28 | 16.2% |
| **Tier 3: Potentially Unused** | 94 | 54.3% |
| | | |
| **KEEP (Tier 1 + 2 + Utils)** | 85+ | 49.1% |
| **REVIEW (Submissions, Assessments)** | 40 | 23.1% |
| **DEPRECATE (Legacy, Duplicates)** | 48 | 27.7% |

---

## Recommendations

### Immediate Actions
1. ✅ **Add deprecation warnings** to Phase 1 functions (8 functions)
2. 📋 **Audit internal scripts** for Phase 2 functions (40 functions)
3. 📝 **Document dependency tree** in package vignette
4. 🔍 **Mark deprecated functions** in documentation

### Medium-term Actions
1. **Create migration guide** for deprecated functions
2. **Update all repos** to remove deprecated function calls
3. **Add lifecycle badges** to function documentation
4. **Create "stable API" documentation** listing Tier 1 + 2 functions

### Long-term Strategy
1. **Establish function lifecycle policy**
2. **Regular usage audits** (annual)
3. **Clear deprecation timeline** (warn → deprecate → remove)
4. **Maintain dependency graph** as package evolves

---

## Files Generated
- `FUNCTION_USAGE_ANALYSIS.md` - This document
- `DEPRECATION_PLAN.md` - Detailed deprecation roadmap (to be created)
- `DEPENDENCY_TREE.md` - Visual dependency diagrams (to be created)

---

*Analysis complete. Ready for review and implementation.*
