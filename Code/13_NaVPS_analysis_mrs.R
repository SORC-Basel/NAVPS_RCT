# -----------------------------------------------------------------------------
# Script Name: 13_NaVPS_analysis_mrs.R
# Purpose: Analysis - MRS
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
#
# Input dataframe: dta
# -----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# "Clinical outcome (mRS), grouped as favourable (0–2), unfavourable (3–5), and mortality (6)."

# t_mrs_admission <- summarize_categorical(
#     df = dta,
#     x = admission_mrs_short,
#     group = randomisation_group_fct,
#     include_N_total = "all",
#     summary_title = "Modified Ranking Scale - Admission",
#     test_type = "fisher"
# )

 t_mrs_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_mrs_short,
    group = randomisation_group_fct,
    include_N_total = "all", include_zero_categories = TRUE,
    summary_title = "Modified Rankin Scale - 48–120h post-operation",
    test_type = "fisher"
)

 t_mrs_discharge <- summarize_categorical(
    df = dta,
    x = discharge_mrs_short,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Modified Rankin Scale - Discharge",
    test_type = "fisher"
)

# t_mrs_fu1 <- summarize_categorical(
#     df = dta,
#     x = fu1_mrs_short,
#     group = randomisation_group_fct,
#     include_N_total = "all",include_zero_categories = TRUE,
#     summary_title = "Modified Rankin Scale - 1st Follow-up",
#     test_type = "fisher"
# )

 t_mrs_fu1 <- summarize_categorical(
    df =  dta[!is.na(dta$fu1_mrs_short),],
    x = fu1_mrs_short,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = FALSE,
    summary_title = "Modified Rankin Scale - 1st Follow-up",
    test_type = "fisher"
)
 
# t_mrs_fu2 <- summarize_categorical(
#     df = dta,
#     x = fu2_mrs_short,
#     group = randomisation_group_fct,
#     include_N_total = "all",include_zero_categories = TRUE,
#     summary_title = "Modified Rankin Scale - 2nd Follow-up",
#     test_type = "fisher"
# )

 t_mrs_fu2 <-summarize_categorical(
    df = dta[!is.na(dta$fu2_mrs_short),],
    x = fu2_mrs_short,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = FALSE,
    summary_title = "Modified Rankin Scale - 2nd Follow-up",
    test_type = "fisher"
)

 t_mrs <- rbind(
    t_mrs_postop,
    t_mrs_discharge,
    t_mrs_fu1,
    t_mrs_fu2
)

