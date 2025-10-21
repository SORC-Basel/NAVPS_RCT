# -----------------------------------------------------------------------------
# Script Name: 11_NaVPS_analysis_complications.R
# Purpose: Analysis - Complications
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
#
# Input dataframe: dta
# -----------------------------------------------------------------------------

## Any Complications
 t_complication_any <- summarize_categorical(
    df = dta,
    x = complications_any_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - Any time",
    test_type = "none"
)
 summary(
    m_complications_any <- glm(
        complications_any_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
 )

 assumptions_complications_any_crude <- simulateResiduals(fittedModel = m_complications_any, plot = F)
 t_complications_any_crude <- table_regression(
    m_complications_any,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)


## Intra operative Complications
 t_complication_intraop <- summarize_categorical(
    df = dta,
    x = opd0_opcomplications_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - Intraoperative",
    test_type = "none"
)

 summary(
    m_complications_intraop <- glm(
        opd0_opcomplications_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
)
 assumptions_complications_intraop_crude <- simulateResiduals(fittedModel = m_complications_intraop, plot = F)
 t_complications_intraop_crude <- table_regression(
    m_complications_intraop,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)





## Post operative Complications
 t_complication_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_complications_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - 48-120h post operation",
    test_type = "none"
)

 summary(
    m_complications_postop <- glm(
        pop_48_72h_complications_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
)
 assumptions_complications_postop_crude <- simulateResiduals(fittedModel = m_complications_postop, plot = F)
 t_complications_postop_crude <- table_regression(
    m_complications_postop,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)




## Discharge Complications
 t_complication_discharge <- summarize_categorical(
    df = dta,
    x = discharge_compl_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - Discharge",
    test_type = "none"
)

 summary(
    m_complications_discharge <- glm(
        discharge_compl_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
)
 assumptions_complications_discharge_crude <- simulateResiduals(fittedModel = m_complications_discharge, plot = F)
 t_complications_discharge_crude <- table_regression(
    m_complications_discharge,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)




## 1st Follow-up Complications
 t_complication_fu1 <- summarize_categorical(
    df = dta,
    x = fu1_compl_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - 1st Follow-up",
    test_type = "none"
)
 summary(
    m_complications_fu1 <- glm(
        fu1_compl_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
)
 assumptions_complications_fu1_crude <- simulateResiduals(fittedModel = m_complications_fu1, plot = F)
 t_complications_fu1_crude <- table_regression(
    m_complications_fu1,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)




## 2nd Follow-up Complications
 t_complication_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_compl_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Complications - 2nd Follow-up",
    test_type = "none"
)
 summary(
    m_complications_fu2 <- glm(
        fu2_compl_ny ~ randomisation_group_stereo_ref,
        family = binomial,
        data = dta
    )
)
 assumptions_complications_fu2_crude <- simulateResiduals(fittedModel = m_complications_fu2, plot = F)
 t_complications_fu2_crude <- table_regression(
    m_complications_fu2,
    include_intercept = FALSE,
    coef_names = coef_names_overall
)

# simulationOutput <- simulateResiduals(fittedModel = m_complications_fu2)
# plot(simulationOutput, asFactor = T)
# plotResiduals(simulationOutput, quantreg = T)
# testDispersion(simulationOutput)





# ----------------------------------------------------------------------------
# Combine Tables 
# ----------------------------------------------------------------------------

 t_complication <- rbind(
    t_complication_any,
    t_complication_intraop,
    t_complication_postop,
    t_complication_discharge,
    t_complication_fu1,
    t_complication_fu2
)



# Regression table
 t_complication_crude <- rbind(t_complications_any_crude,
                              t_complications_intraop_crude, 
                              t_complications_postop_crude, 
                              t_complications_discharge_crude,
                              t_complications_fu1_crude,
                              t_complications_fu2_crude)

 t_complication_crude$Coefficients <- c("Ultrasound (vs Stereotactic navigation) - Any time", 
                                       "Ultrasound (vs Stereotactic navigation) - Intraoperative", 
                                       "Ultrasound (vs Stereotactic navigation) - 48-120h post operation", 
                                       "Ultrasound (vs Stereotactic navigation) - Discharge", 
                                       "Ultrasound (vs Stereotactic navigation) - 1st Follow-up", 
                                       "Ultrasound (vs Stereotactic navigation) - 2nd Follow-up")



 t_forest_complication <- rbind(
    c("Complications", "", "", "", ""),
    t_complications_any_crude,
    t_complications_intraop_crude, 
    t_complications_postop_crude, 
    t_complications_discharge_crude,
    t_complications_fu1_crude,                             
    t_complications_fu2_crude)
 t_forest_complication$Coefficients[-1] <- c("Any time", "Intraoperative", "48-120h post operation", "Discharge", "1st Follow-up", "2nd Follow-up")




# -------------------------------------------------------------------------------
# Type of complications
# -------------------------------------------------------------------------------
 t_type_complication_postop <- summarize_categorical(
    df = dta[!is.na(dta$type_complication_pop_48_72h),],
    x = type_complication_pop_48_72h,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Type of Complications - 48-120h post operation",
    test_type = "none"
)

 t_type_complication_discharge <- summarize_categorical(
    df = dta[!is.na(dta$type_complication_discharge),],
    x = type_complication_discharge,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Type of Complications - Discharge",
    test_type = "none"
)

 t_type_complication_fu1 <- summarize_categorical(
    df = dta[!is.na(dta$type_complication_fu1),],
    x = type_complication_fu1,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Type of Complications - 1st Follow-up",
    test_type = "none"
)

 t_type_complication_fu2 <- summarize_categorical(
    df = dta[!is.na(dta$type_complication_fu2),],
    x = type_complication_fu2,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Type of Complications - 2nd Follow-up",
    test_type = "none"
)



 t_type_complication <- rbind(
    t_type_complication_postop,
    t_type_complication_discharge,
    t_type_complication_fu1,
    t_type_complication_fu2
)
