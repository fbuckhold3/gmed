# gmed Package Dependency Tree
**Visual Documentation of Internal Function Dependencies**

---

## Key Entry Points (Most Used External Functions)

### ⭐⭐⭐⭐ CRITICAL - Used in 4+ repos

```
load_rdm_complete()
├── [Tier 2] load_data_by_forms()
│   ├── [Tier 2] get_evaluation_dictionary()
│   ├── [Tier 2] pull_all_redcap_data()
│   └── [Tier 2] calculate_resident_level()
│       └── [Uses internal date/level logic]
├── [Tier 2] get_cached()
├── [Tier 2] set_cached()
├── [Tier 2] calculate_all_milestone_medians()
│   ├── [Tier 2] process_milestone_data_simple()
│   │   └── [Tier 2] get_milestone_columns_simple()
│   └── [Tier 2] calculate_milestone_medians_simple()
│       └── [Uses get_milestone_columns_simple()]
├── [Tier 2] filter_archived_residents()
└── [Tier 2] create_milestone_workflow_from_dict()
    ├── [Tier 2] extract_milestone_configs_from_dict()
    └── [Tier 2] create_universal_period_mapping()

Total Internal Dependencies: 13 functions
Dependency Depth: 4 levels
Used By: imslu.coach.dash, imslu.ccc.dashboard,
         imslu-resident-self-assessment, imslu.ind.dash
```

---

### ⭐⭐⭐ HIGH PRIORITY - Used in 3 repos

```
calculate_pgy_and_period()
├── [Tier 2] get_period_label()
│   └── [Tier 2] parse_redcap_choices()
├── [Tier 2] translate_resident_type()
│   └── [Tier 2] parse_redcap_choices()
└── [Tier 2] get_resident_periods()

Total Internal Dependencies: 4 functions
Dependency Depth: 3 levels
Used By: imslu.ccc.dashboard, imslu-resident-self-assessment (2x)
```

```
create_milestone_workflow_from_dict()
├── [Tier 2] extract_milestone_configs_from_dict()
└── [Tier 2] create_universal_period_mapping()

Total Internal Dependencies: 2 functions
Dependency Depth: 2 levels
Used By: imslu.coach.dash, imslu.ccc.dashboard, imslu.ind.dash
```

---

### ⭐⭐ MEDIUM PRIORITY - Used in 2 repos

```
create_milestone_overview_dashboard()
├── [Tier 1] get_milestone_data()
│   └── [Self-contained filter logic]
├── [Tier 1] create_enhanced_milestone_spider_plot()
│   ├── [Tier 2] get_milestone_columns_simple()
│   └── [Tier 2] get_milestone_label()
└── [Tier 2] get_milestone_columns_simple()

Total Internal Dependencies: 4 functions
Dependency Depth: 3 levels
Used By: imslu.coach.dash (2x), imslu.ccc.dashboard (3x)
```

```
load_gmed_styles()
└── [Loads CSS/theme files - no function dependencies]

Used By: imslu.coach.dash, imslu-resident-self-assessment
```

```
assess_ite_risk()
└── [Self-contained risk calculation logic]

Used By: imslu.coach.dash, imslu-resident-self-assessment
```

```
gmed_page()
└── [UI wrapper - no function dependencies]

Used By: imslu.ccc.dashboard, imslu.ind.dash
```

```
mod_plus_delta_table_ui/server()
└── [Shiny module - may call internal helpers]

Used By: imslu.coach.dash, imslu.ind.dash
```

```
assessment_viz_ui/server()
├── [Tier 2] ssm_colors()
├── [Tier 2] create_recent_activity_summary()
│   └── [Tier 2] create_weekly_questions_average()
├── [Tier 2] create_combined_assessment_chart()
│   └── [Tier 2] ssm_colors()
└── [Tier 2] create_combined_faculty_chart()
    └── [Tier 2] ssm_colors()

Total Internal Dependencies: 5 functions
Dependency Depth: 3 levels
Used By: imslu.coach.dash, imslu.ind.dash
```

---

## Complete Dependency Graph by Module

### Data Loading Module

```
┌─────────────────────────────────────────────────────────────┐
│                   DATA LOADING MODULE                        │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► load_rdm_complete() [TIER 1] ⭐⭐⭐⭐
    │   ├─► load_data_by_forms() [TIER 2]
    │   │   ├─► get_evaluation_dictionary() [TIER 2]
    │   │   ├─► pull_all_redcap_data() [TIER 2]
    │   │   └─► calculate_resident_level() [TIER 2]
    │   ├─► get_cached() [TIER 2]
    │   ├─► set_cached() [TIER 2]
    │   ├─► calculate_all_milestone_medians() [TIER 2]
    │   │   ├─► process_milestone_data_simple() [TIER 2]
    │   │   │   └─► get_milestone_columns_simple() [TIER 2]
    │   │   └─► calculate_milestone_medians_simple() [TIER 2]
    │   ├─► filter_archived_residents() [TIER 2]
    │   └─► create_milestone_workflow_from_dict() [TIER 2]
    │       ├─► extract_milestone_configs_from_dict() [TIER 2]
    │       └─► create_universal_period_mapping() [TIER 2]
    │
    ├─► load_gmed_styles() [TIER 1] ⭐⭐
    │   └─► [CSS/theme file loading]
    │
    ├─► authenticate_resident() [TIER 1]
    │   └─► [REDCap API validation]
    │
    └─► DEPRECATED:
        ├─► load_rdm_quick() [TIER 3] ❌
        └─► load_rdm_simple() [TIER 3] ❌
```

---

### Period Calculation Module

```
┌─────────────────────────────────────────────────────────────┐
│              PERIOD CALCULATION MODULE                       │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► calculate_pgy_and_period() [TIER 1] ⭐⭐⭐
    │   ├─► get_period_label() [TIER 2]
    │   │   └─► parse_redcap_choices() [TIER 2]
    │   ├─► translate_resident_type() [TIER 2]
    │   │   └─► parse_redcap_choices() [TIER 2]
    │   └─► get_resident_periods() [TIER 2]
    │
    ├─► get_current_period() [TIER 1]
    │   └─► [Date-based calculation]
    │
    ├─► normalize_period() [TIER 2]
    │   └─► [Period standardization]
    │
    └─► periods_match() [TIER 2]
        └─► [Flexible period comparison]
```

---

### Milestone Processing Module

```
┌─────────────────────────────────────────────────────────────┐
│           MILESTONE PROCESSING MODULE                        │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► create_milestone_workflow_from_dict() [TIER 1] ⭐⭐⭐
    │   ├─► extract_milestone_configs_from_dict() [TIER 2]
    │   └─► create_universal_period_mapping() [TIER 2]
    │
    ├─► get_milestone_data() [TIER 1]
    │   └─► [Filter workflow results]
    │
    ├─► process_milestone_data_simple() [TIER 2]
    │   └─► get_milestone_columns_simple() [TIER 2]
    │
    ├─► calculate_all_milestone_medians() [TIER 2]
    │   ├─► process_milestone_data_simple() [TIER 2]
    │   └─► calculate_milestone_medians_simple() [TIER 2]
    │
    ├─► get_milestone_label() [TIER 1]
    │   └─► [Label lookup]
    │
    ├─► get_milestone_definitions() [TIER 1]
    │   └─► [Definition lookup]
    │
    └─► POTENTIALLY UNUSED:
        ├─► get_milestone_desc_fields() [TIER 3] ❓
        ├─► get_milestone_field_mapping_rdm2() [TIER 3] ❓
        ├─► get_milestone_fields_from_dict() [TIER 3] ❓
        ├─► convert_rep_to_acgme_format() [TIER 3] ❓
        ├─► check_milestone_completeness() [TIER 3] ❓
        ├─► join_individual_with_medians() [TIER 3] ❓
        └─► prepare_milestone_app_data() [TIER 3] ❓
```

---

### Visualization Module

```
┌─────────────────────────────────────────────────────────────┐
│              VISUALIZATION MODULE                            │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► create_milestone_overview_dashboard() [TIER 1] ⭐⭐
    │   ├─► get_milestone_data() [TIER 1]
    │   ├─► create_enhanced_milestone_spider_plot() [TIER 1]
    │   │   ├─► get_milestone_columns_simple() [TIER 2]
    │   │   └─► get_milestone_label() [TIER 2]
    │   └─► get_milestone_columns_simple() [TIER 2]
    │
    ├─► create_milestone_spider_plot_final() [TIER 1]
    │   └─► get_milestone_columns_simple() [TIER 2]
    │
    ├─► create_enhanced_milestone_spider_plot() [TIER 1]
    │   ├─► get_milestone_columns_simple() [TIER 2]
    │   ├─► get_milestone_label() [TIER 2]
    │   └─► get_national_milestone_benchmarks() [TIER 2]
    │
    ├─► visualize_ite_scores() [TIER 1]
    │   └─► [Plotly visualization]
    │
    ├─► assess_ite_risk() [TIER 1] ⭐⭐
    │   └─► [Risk calculation logic]
    │
    └─► DEPRECATED:
        ├─► create_enhanced_milestone_progression() [TIER 3] ❌
        ├─► create_milestone_dashboard_layout() [TIER 3] ❌
        ├─► milestone_dashboard_css() [TIER 3] ❌
        ├─► milestone_dashboard_server() [TIER 3] ❌
        └─► milestone_dashboard_ui() [TIER 3] ❌
```

---

### Assessment Module

```
┌─────────────────────────────────────────────────────────────┐
│               ASSESSMENT MODULE                              │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► assessment_viz_server() [TIER 1] ⭐⭐
    │   ├─► create_recent_activity_summary() [TIER 2]
    │   │   └─► create_weekly_questions_average() [TIER 2]
    │   ├─► create_combined_assessment_chart() [TIER 2]
    │   │   └─► ssm_colors() [TIER 2]
    │   └─► create_combined_faculty_chart() [TIER 2]
    │       └─► ssm_colors() [TIER 2]
    │
    ├─► mod_assessment_detail_custom_ui/server() [TIER 1]
    │   └─► [Custom assessment display]
    │
    ├─► mod_assessment_data_display_ui/server() [TIER 1]
    │   └─► [Data display logic]
    │
    ├─► mod_cc_completion_ui/server() [TIER 1]
    │   └─► [Continuity clinic tracking]
    │
    ├─► mod_questions_viz_ui/server() [TIER 1]
    │   └─► [Questions visualization]
    │
    ├─► mod_plus_delta_table_ui/server() [TIER 1] ⭐⭐
    │   └─► [Plus-delta table display]
    │
    ├─► generate_plus_delta_rdm2() [TIER 1]
    │   └─► [Plus-delta data generation]
    │
    ├─► gmed_plus_delta_display() [TIER 1]
    │   └─► [Plus-delta UI component]
    │
    └─► POTENTIALLY UNUSED:
        ├─► add_level_at_time_to_assessments() [TIER 3] ❓
        ├─► calculate_level_at_time() [TIER 3] ❓
        ├─► count_assessment_completions_with_time_level() [TIER 3] ❓
        ├─► extract_assessment_categories() [TIER 3] ❓
        ├─► filter_assessment_by_type() [TIER 3] ❓
        ├─► format_assessment_display() [TIER 3] ❓
        └─► [20+ more assessment helpers] ❓
```

---

### UI Components Module

```
┌─────────────────────────────────────────────────────────────┐
│              UI COMPONENTS MODULE                            │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► gmed_page() [TIER 1] ⭐⭐
    │   └─► [Page wrapper with theme]
    │
    ├─► gmed_app_header() [TIER 1]
    │   └─► [Header component]
    │
    ├─► gmed_card() [TIER 1]
    │   └─► [Card container]
    │
    ├─► gmed_resident_panel() [TIER 1]
    │   └─► [Resident info panel]
    │
    ├─► gmed_status_badge() [TIER 1]
    │   └─► [Status indicator]
    │
    ├─► ssm_colors() [TIER 2]
    │   └─► [Brand color palette]
    │
    └─► POTENTIALLY UNUSED:
        ├─► apply_gmed_style() [TIER 3] ❓
        ├─► create_gmed_theme() [TIER 3] ❓
        ├─► gmed_progress_bar() [TIER 3] ❓
        ├─► gmed_selector_container() [TIER 3] ❓
        ├─► gmed_step_indicator() [TIER 3] ❓
        ├─► create_gmed_datatable() [TIER 3] ❓
        └─► [15+ more UI components] ❓
```

---

### Data Submission Module

```
┌─────────────────────────────────────────────────────────────┐
│            DATA SUBMISSION MODULE                            │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► submit_scholarship_data() [TIER 1]
    │   └─► submit_to_redcap() [TIER 3?]
    │
    └─► POTENTIALLY UNUSED (Need audit):
        ├─► submit_additive_data() [TIER 3] ❓
        ├─► submit_ccc_review_data() [TIER 3] ❓
        ├─► submit_coach_review() [TIER 3] ❓
        ├─► submit_coach_review_data() [TIER 3] ❓
        ├─► submit_faculty_evaluation() [TIER 3] ❓
        ├─► submit_milestone_data() [TIER 3] ❓
        ├─► submit_resident_evaluation() [TIER 3] ❓
        ├─► submit_self_eval_data() [TIER 3] ❓
        └─► submit_overwrite_data() [TIER 3] ❓
```

---

### Performance & Utilities Module

```
┌─────────────────────────────────────────────────────────────┐
│         PERFORMANCE & UTILITIES MODULE                       │
└─────────────────────────────────────────────────────────────┘
    │
    ├─► CACHING (NEW):
    │   ├─► get_cached() [TIER 2]
    │   ├─► set_cached() [TIER 2]
    │   ├─► clear_rdm_cache() [TIER 2 + Public API]
    │   └─► get_cache_info() [Public API]
    │
    ├─► PROFILING (NEW):
    │   └─► profile_rdm_loading() [Public API]
    │
    ├─► HELPERS:
    │   ├─► parse_redcap_choices() [TIER 2]
    │   ├─► process_summative_data() [TIER 2]
    │   ├─► collect_evaluation_data() [TIER 2]
    │   ├─► convert_coach_codes_to_names() [TIER 1]
    │   └─► get_coach_name_from_code() [TIER 1]
    │
    └─► POTENTIALLY UNUSED:
        ├─► validate_access_code() [TIER 3] ❓
        ├─► get_archive_summary() [TIER 3] ❓
        ├─► debug_data_dict() [TIER 3] ❓
        ├─► list_forms() [TIER 3] ❓
        ├─► get_form_data() [TIER 3] ❓
        ├─► test_redcap_connection() [TIER 3] ❌
        ├─► initialize_app_config() [TIER 3] ❓
        └─► test_dict_driven_workflow() [TIER 3] ❌
```

---

## Dependency Statistics

### Functions by Dependency Depth

| Depth | Count | Examples |
|-------|-------|----------|
| **0 (Self-contained)** | 23 | `get_milestone_data()`, `gmed_page()`, etc. |
| **1 (Shallow)** | 12 | `create_milestone_spider_plot_final()` |
| **2 (Medium)** | 8 | `create_milestone_workflow_from_dict()` |
| **3 (Deep)** | 4 | `calculate_pgy_and_period()`, `assessment_viz_server()` |
| **4 (Very Deep)** | 1 | `load_rdm_complete()` ⭐ |

### Most Connected Functions (Hub Functions)

| Function | Incoming Calls | Outgoing Calls | Total Connections |
|----------|----------------|----------------|-------------------|
| **`get_milestone_columns_simple()`** | 5+ | 0 | 5+ |
| **`ssm_colors()`** | 3+ | 0 | 3+ |
| **`parse_redcap_choices()`** | 2+ | 0 | 2+ |
| **`load_rdm_complete()`** | 4 repos | 13 | 17 |
| **`process_milestone_data_simple()`** | 2 | 1 | 3 |

---

## Critical Paths (Cannot Break)

### Path 1: Core Data Loading
```
External Repos → load_rdm_complete() → load_data_by_forms() → pull_all_redcap_data()
```
**Impact if broken:** All 4 repos using `load_rdm_complete()` fail
**Testing priority:** CRITICAL

### Path 2: Period Calculation
```
External Repos → calculate_pgy_and_period() → get_period_label() → parse_redcap_choices()
```
**Impact if broken:** Period calculation fails in 3 repos
**Testing priority:** HIGH

### Path 3: Milestone Workflow
```
External Repos → create_milestone_workflow_from_dict() → extract_milestone_configs_from_dict()
```
**Impact if broken:** Milestone processing fails in 3 repos
**Testing priority:** HIGH

### Path 4: Assessment Visualization
```
External Repos → assessment_viz_server() → create_combined_assessment_chart() → ssm_colors()
```
**Impact if broken:** Assessment charts break in 2 repos
**Testing priority:** MEDIUM

---

## Legend

- **[TIER 1]** = Called directly by external repos
- **[TIER 2]** = Internal dependency (called by Tier 1)
- **[TIER 3]** = Not found in usage analysis
- **⭐⭐⭐⭐** = Used in 4+ repos (CRITICAL)
- **⭐⭐⭐** = Used in 3 repos (HIGH PRIORITY)
- **⭐⭐** = Used in 2 repos (MEDIUM PRIORITY)
- **⭐** = Used in 1 repo (LOW PRIORITY)
- **❌** = Confirmed for deprecation
- **❓** = Needs audit before decision

---

*This dependency tree should be updated whenever functions are added, removed, or refactored.*
