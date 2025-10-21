# -----------------------------------------------------------------------------
# Script Name: 12_NaVPS_analysis_revisions.R
# Purpose: Analysis - Revision Surgery
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
#
# Input dataframe: dta
# -----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Revision surgery (yes/no) and reason for revision
# ----------------------------------------------------------------------------
 t_revision_surgery_day0 <- summarize_categorical(
    df = dta,
    x = opd0_revision_surgery_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Day 0 Revision surgery",
    test_type = "none"
)

# summary(
#     m_revision_day0 <- glm(
#         opd0_revision_surgery_fct ~ randomisation_group_fct,
#         family = binomial,
#         data = dta
#     )
# )
#  simulateResiduals(fittedModel = m_revision_day0, plot = T)
#  check_model(m_revision_day0)

 summary(
    m_revision_day0 <- logistf(
        opd0_revision_surgery_fct ~ randomisation_group_fct,
        data = dta
    )
)
 
# t_revision_day0_crude <- table_regression(
#     m_revision_day0,
#     include_intercept = FALSE,
#     coef_names = c("Stereotactic navigation" = "Day 0")
# )
# 
 t_revision_day0 <- tidy(m_revision_day0)[2,]
 t_revision_day0$term <- "Ultrasound (vs Stereotactic navigation) - Day 0"


# Post-operation 
 t_revision_surgery_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_rs_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "48-120h post operation Revision surgery",
    test_type = "none"
)


 summary(
    m_revision_postop <- logistf(
        pop_48_72h_rs_fct ~ randomisation_group_fct,
        family = binomial,
        data = dta
    )
)
 t_revision_postop <- tidy(m_revision_postop)[2,]
 t_revision_postop$term <- "Ultrasound (vs Stereotactic navigation) - 48-120h post operation"


# Discharge
 t_revision_surgery_discharge <- summarize_categorical(
    df = dta,
    x = discharge_rs_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Discharge Revision surgery",
    test_type = "none"
)


 summary(
    m_revision_discharge <- logistf(
        discharge_rs_fct ~ randomisation_group_fct,
        family = binomial,
        data = dta
    )
)
 t_revision_discharge <- tidy(m_revision_discharge)[2,]
 t_revision_discharge$term <- "Ultrasound (vs Stereotactic navigation) - Discharge"


# Follow-up
 t_revision_surgery_fu1 <- summarize_categorical(
    df = dta,
    x = fu1_rs_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Follow-up 1 Revision surgery",
    test_type = "none"
)

# t_revision_surgery_fu1_reason <- summarize_categorical(dta[dta$fu1_other_reason != "",], fu1_other_reason,  group = randomisation_group_fct)
 summary(
    m_revision_fu1 <- logistf(
        fu1_rs_fct ~ randomisation_group_fct,
        family = binomial,
        data = dta
    )
)
 t_revision_fu1 <- tidy(m_revision_fu1)[2,]
 t_revision_fu1$term <- "Ultrasound (vs Stereotactic navigation) - 1st Follow-up"



# Follow-up 2
 t_revision_surgery_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_rs_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Follow-up 2 Revision surgery",
    test_type = "none"
)


# t_revision_surgery_fu2_reason <- summarize_categorical(dta[dta$fu2_other_reason != "",], fu2_other_reason,  group = randomisation_group_fct)
 summary(
    m_revision_fu2 <- logistf(
        fu2_rs_fct ~ randomisation_group_fct,
        family = binomial,
        data = dta
    )
)
 t_revision_fu2 <- tidy(m_revision_fu2)[2,]
 t_revision_fu2$term <- "Ultrasound (vs Stereotactic navigation) - 2nd Follow-up"


# ----------------------------------------------------------------------------
# Reasons for Revision 
# ----------------------------------------------------------------------------


 combine_revisionsurgery_reason <- function(x, title = NULL) {
    # Reason labels
    reason_labels <- c(
        "bleeding",
        "infection",
        "obstruction",
        "misplacement",
        "disconnection",
        "distal dislocation",
        "proximal dislocation",
        "other"
    )
    checkbox_vars <- paste0(x, 1:8)
    
    dta$reason <- apply(dta[checkbox_vars], 1, function(row) {
        row <- as.character(row)  # <-- Make sure it's a character vector
        paste(reason_labels[which(row == "Checked")], collapse = ", ")
    })
    dta$reason[dta$reason == ""] <- NA
    
    t_reason <-  summarize_categorical(dta[!is.na(dta$reason),], 
                                       reason,  
                                       group = randomisation_group_fct, include_zero_categories = TRUE, 
                                       summary_title = title
                                        )
    
    t_reason[t_reason == "0 (NaN)"] <- "0 (0)"
    t_reason
}


 t_day0_rs_reason <- summarize_categorical(dta[dta$op_day_0_indication != "",], 
                                          op_day_0_indication,  
                                          group = randomisation_group_fct, 
                                          include_zero_categories = TRUE,
                                          summary_title = "Day 0 - Reasons for revision surgery"
                                          )
 t_day0_rs_reason[t_day0_rs_reason == "0 (NaN)"] <- "0 (0)"


 t_pop_rs_reason <- combine_revisionsurgery_reason("pop_48_72h_rs_reason_", "48-120h post operation - Reasons for revision surgery")
# t_discharge_rs_reason <- combine_revisionsurgery_reason("discharge_reas_for_rev_", "Discharge - Reasons for revision surgery")
 t_fu1_rs_reason <- combine_revisionsurgery_reason("fu1_reas_for_rev_", "Follow-up 1 - Reasons for revision surgery")
 t_fu2_rs_reason <- combine_revisionsurgery_reason("fu2_reas_for_rev_", "Follow-up 2 - Reasons for revision surgery")


# ----------------------------------------------------------------------------
# Combine Tables 
# ----------------------------------------------------------------------------


 t_revision_surgery <- rbind(
    t_revision_surgery_day0,
    t_revision_surgery_postop,
    t_revision_surgery_discharge,
    t_revision_surgery_fu1,
    t_revision_surgery_fu2
)


 t_revision_surgery_reason <- rbind(
    # t_day0_rs_reason,
    t_pop_rs_reason,
    # t_discharge_rs_reason,
    t_fu1_rs_reason,
    t_fu2_rs_reason
)


 t_revision_surgery_regression <- rbind(
    t_revision_day0,
    t_revision_postop,
    t_revision_discharge,
    t_revision_fu1,
    t_revision_fu2
)


 t_forest_revision_surgery <- rbind(
    c("Revision surgery", "", "", "", ""),
    t_revision_day0,
    t_revision_postop,
    t_revision_discharge,
    t_revision_fu1,
    t_revision_fu2
)
 t_forest_revision_surgery$term[-1] <- c("Day 0", "48-120h post operation", "Discharge", "1st Follow-up", "2nd Follow-up")









# ----------------------------------------------------------------------------
# Rate of infections
# ----------------------------------------------------------------------------
 t_revision_surgery_infection <- summarize_categorical(dta, 
                      revision_any_infection,  
                      group = randomisation_group_fct, 
                      include_zero_categories = TRUE,
                      summary_title = "Revision surgeries due to infection", 
                      test_type = "fisher"
)
