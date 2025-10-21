# -----------------------------------------------------------------------------
# Script Name: 10_NaVPS_analysis_evans.R
# Purpose: Analysis - Evan’s Index
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
# Input dataframe: dta
# -----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Evan’s Index pre- and postoperatively (absolute change)
# ----------------------------------------------------------------------------
t_evans_diff_postop_median <- summarize_continuous(
    df = dta,
    x = evans_diff_postop,
    group = randomisation_group_fct ,
    summary_title = "Change in Evans index (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)

summary(m_evans_diff_postop_crude <- lm(evans_diff_postop ~ randomisation_group_stereo_ref, dta))
pval_m_evans_diff_postop_crude <- drop1(m_evans_diff_postop_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_evans_diff_postop_crude)
simulationOutput <- simulateResiduals(fittedModel = m_evans_diff_postop_crude, plot = T)
check_model(m_evans_diff_postop_crude)

 t_evans_diff_postop_crude <- table_regression(
    m_evans_diff_postop_crude,
    include_intercept = FALSE,
    coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Absolute change" )
)



 t_evans_diff_fu2_median <- summarize_continuous(
    df = dta,
    x = evans_diff_fu2,
    group = randomisation_group_fct ,
    summary_title = "Change in Evans index (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


summary(m_evans_diff_fu2_crude <- lm(evans_diff_fu2 ~ randomisation_group_stereo_ref, dta))
pval_m_evans_diff_fu2_crude <- drop1(m_evans_diff_fu2_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_evans_diff_fu2_crude)
simulationOutput <- simulateResiduals(fittedModel = m_evans_diff_fu2_crude, plot = F)
# plot(simulationOutput)

 t_evans_diff_fu2_crude <- table_regression(
    m_evans_diff_fu2_crude,
    include_intercept = FALSE,
    coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Absolute change" )
)


# ----------------------------------------------------------------------------
# Evans index realtive change
# ----------------------------------------------------------------------------
t_evans_diff_rel_postop_median <- summarize_continuous(
    df = dta,
    x = evans_diff_rel_postop,
    group = randomisation_group_fct ,
    summary_title = "Relative change in Evans index (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


summary(m_evans_diff_rel_postop_crude <- lm(evans_diff_rel_postop ~ randomisation_group_stereo_ref, dta))
pval_m_evans_diff_rel_postop_crude <- drop1(m_evans_diff_postop_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_evans_diff_rel_postop_crude)
simulationOutput <- simulateResiduals(fittedModel = m_evans_diff_rel_postop_crude, plot = F)
# plot(simulationOutput)

 t_evans_diff_rel_postop_crude <- table_regression(
    m_evans_diff_rel_postop_crude,
    include_intercept = FALSE,
    coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Relative change" )
)






 t_evans_diff_rel_fu2_median <- summarize_continuous(
    df = dta,
    x = evans_diff_rel_fu2,
    group = randomisation_group_fct ,
    summary_title = "Relative change in Evans index (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


summary(m_evans_diff_rel_fu2_crude <- lm(evans_diff_rel_fu2 ~ randomisation_group_stereo_ref, dta))
pval_m_evans_diff_rel_fu2_crude <- drop1(m_evans_diff_rel_fu2_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_evans_diff_rel_fu2_crude)
simulationOutput <- simulateResiduals(fittedModel = m_evans_diff_rel_fu2_crude, plot = F)
# plot(simulationOutput)
 t_evans_diff_rel_fu2_crude <- table_regression(
    m_evans_diff_rel_fu2_crude,
    include_intercept = FALSE,
    coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Relative change" )
)



# ----------------------------------------------------------------------------
# Evans index improvement - Categories
# ----------------------------------------------------------------------------
## Post OP
 t_evans_improve_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_ei_improved_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Evans index improved",
    test_type = "none"
)


summary(m_evans_improve_postop <- MASS::polr(pop_48_72h_ei_improved_fct ~ randomisation_group_stereo_ref, data = dta, method = "logistic"))
 p_evans_improve_posto_ci <- confint(m_evans_improve_postop)
 p_evans_improve_posto_p <- drop1(m_evans_improve_postop, test = "Chisq")
 t_evans_improve_posto_crude <- data.frame(
    Coefficients = c("Ultrasound (vs Stereotactic navigation)"), 
    "Odds Ratios"  = round(exp(m_evans_improve_postop$coefficients), 3),
    # lower = exp(p_evans_improve_posto_ci[1]),
    # upper = exp(p_evans_improve_posto_ci[2]),

    CI = paste(
        round(exp(p_evans_improve_posto_ci[1]), 3),
        "-" ,
        round(exp(p_evans_improve_posto_ci[2]),3)),
    "P-value" = nice_pvalues(p_evans_improve_posto_p$`Pr(>Chi)`[2])
)

## Follow-up
 t_evans_improve_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_ei_improved_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Evans index improved",
    test_type = "none"
)

summary(m_evans_improve_fu2 <- MASS::polr(fu2_ei_improved_fct ~ randomisation_group_stereo_ref, data = dta, method = "logistic"))
 p_evans_improve_fu2_ci <- confint(m_evans_improve_fu2)
 p_evans_improve_fu2_p <- drop1(m_evans_improve_fu2, test = "Chisq")
 t_evans_improve_fu2_crude <- data.frame(
    Coefficients = c("Ultrasound (vs Stereotactic navigation)"), 
    "Odds Ratios"  = round(exp(m_evans_improve_fu2$coefficients), 3),
    # lower = exp(p_evans_improve_fu2_ci[1]),
    # upper = exp(p_evans_improve_fu2_ci[2]),
    CI = paste(
        round(exp(p_evans_improve_fu2_ci[1]), 3),
        "-" ,
        round(exp(p_evans_improve_fu2_ci[2]),3)),
    "P-value" = nice_pvalues(p_evans_improve_fu2_p$`Pr(>Chi)`[2])
)


# ----------------------------------------------------------------------------
# Combine Tables 
# ----------------------------------------------------------------------------

 t_evans <- rbind(
    c("48-120h post operation", "", "", "", ""),
    # t_evans_diff_postop_mean,
    t_evans_diff_postop_median,
    # t_evans_diff_rel_postop_mean,
    t_evans_diff_rel_postop_median,
    
    c(" ", "", "", "", ""),
    c("2nd Follow-up", "", "", "", ""),
    # t_evans_diff_fu2_mean,
    t_evans_diff_fu2_median,
    # t_evans_diff_rel_fu2_mean,
    t_evans_diff_rel_fu2_median
)

                           

t_evans_regression  <- rbind(
    c("48-120h post operation", "", "", "", ""),
    rbind(t_evans_diff_postop_crude, t_evans_diff_rel_postop_crude),
    c("2nd Follow-up", "", "", "", ""),
    rbind( t_evans_diff_fu2_crude, t_evans_diff_rel_fu2_crude )
)



 t_forest_evans  <- rbind(
    c("Absolute change", "", "", "", ""),
    rbind(t_evans_diff_postop_crude, 
          t_evans_diff_fu2_crude ), 
    c("Relative change", "", "", "", ""),
    rbind(t_evans_diff_rel_postop_crude, 
          t_evans_diff_rel_fu2_crude))


 t_forest_evans$Coefficients[2:3] <- c("48-120h post operation","2nd Follow-up" )
 t_forest_evans$Coefficients[5:6] <- c("48-120h post operation","2nd Follow-up" )


##  Evens index improvement - Categories
 t_evans_improve_crude <- rbind(
    c("48-120h post operation", "", "", "", ""),
    t_evans_improve_postop,
    
    c(" ", "", "", "", ""),
    c("2nd Follow-up", "", "", "", ""),
    t_evans_improve_fu2
)
# Regression table
 t_evans_improve_regression <- rbind(
    c("48-120h post operation", "", "", "", ""),
    t_evans_improve_posto_crude,
    c("2nd Follow-up", "", "", "", ""),
    t_evans_improve_fu2_crude)


# Regression table
 t_forest_evans_improve <- rbind(
    c("Evens index improvement", "", "", "", ""),
    t_evans_improve_posto_crude,
    t_evans_improve_fu2_crude)

t_forest_evans_improve$Coefficients[2:3] <- c("48-120h post operation", "2nd Follow-up" )
