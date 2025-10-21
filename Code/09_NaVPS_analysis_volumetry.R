# -----------------------------------------------------------------------------
# Script Name: 09_NaVPS_analysis_volumetry.R
# Purpose: Analysis - Volumetry of side
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
#
# Input dataframe: dta
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Volumetry of side ventricles pre- and postoperatively in cm3 (number)
# -----------------------------------------------------------------------------
# Absolute Differences 
t_volumetry_diff_postop_median <- summarize_continuous(
    df = dta,
    x = volumetry_diff_postop,
    group = randomisation_group_fct ,
    summary_title = "Absolute change (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


summary(m_volumetry_diff_postop_crude <- create_regression("volumetry_diff_postop", model = "linear", adjustment = "crude"))
assumptions_volumetry_diff_postop_crude <- simulateResiduals(fittedModel = m_volumetry_diff_postop_crude, plot = T)
check_model(m_volumetry_diff_postop_crude)

pval_volumetry_diff_postop <- drop1(m_volumetry_diff_postop_crude,  test = "Chisq")$`Pr(>Chi)`[2]
t_volumetry_diff_postop_crude <- table_regression(
    m_volumetry_diff_postop_crude,
    coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Absolute change"),
    include_intercept = FALSE
)



# 2nd follow-up
t_volumetry_diff_fu2_median <- summarize_continuous(
    df = dta,
    x = volumetry_diff_fu2,
    group = randomisation_group_fct ,
    summary_title = "Absolute change (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


summary(m_volumetry_diff_fu2_crude <- lm(volumetry_diff_fu2 ~ randomisation_group_stereo_ref, dta))
assumptions_volumetry_diff_fu2_crude <- simulateResiduals(fittedModel = m_volumetry_diff_fu2_crude, plot = T)
check_model(m_volumetry_diff_fu2_crude)
# pval_volumetry_diff_fu2 <- drop1(m_volumetry_diff_fu2_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_volumetry_diff_fu2_crude)
t_volumetry_diff_fu2_crude <- table_regression(m_volumetry_diff_fu2_crude, 
                                               coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Absolute change" ),
                                               include_intercept = FALSE)


# Frequency Tables
 t_volumetry_diff <- rbind(
    t_volumetry_diff_postop_median,
    t_volumetry_diff_fu2_median
)

# Regression table
t_volumetry_diff_crude <- rbind(t_volumetry_diff_postop_crude, t_volumetry_diff_fu2_crude)



# -----------------------------------------------------------------------------
# Volumetry of side ventricles pre- and postoperatively in cm3 (relative change)
# -----------------------------------------------------------------------------
t_volumetry_diff_rel_postop_median <- summarize_continuous(
    df = dta,
    x = volumetry_diff_rel_postop,
    group = randomisation_group_fct ,
    summary_title = "Relative change (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)
summary(m_volumetry_diff_rel_postop_crude <- lm(volumetry_diff_rel_postop ~ randomisation_group_stereo_ref, dta))
assumptions_volumetry_diff_rel_postop_crude <- simulateResiduals(fittedModel = m_volumetry_diff_rel_postop_crude, plot = T)
check_model(m_volumetry_diff_rel_postop_crude)


pval_volumetry_diff_rel_postop <- drop1(m_volumetry_diff_rel_postop_crude,  test = "Chisq")$`Pr(>Chi)`[2]
# plot(m_volumetry_diff_rel_postop_crude)
t_volumetry_diff_rel_postop_crude <- table_regression(m_volumetry_diff_rel_postop_crude, 
                                                      coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Relative change" ),
                                                      include_intercept = FALSE)


# 2nd follow-up
 t_volumetry_diff_rel_fu2_median <- summarize_continuous(
    df = dta,
    x = volumetry_diff_rel_fu2,
    group = randomisation_group_fct ,
    # summary_title = "Relative change in volumetry of side ventricles (%) (Median & IQR)",
    summary_title = "Relative change (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)
 summary(m_volumetry_diff_rel_fu2_crude <- lm(volumetry_diff_rel_fu2 ~ randomisation_group_stereo_ref, dta))
 assumptions_volumetry_diff_rel_fu2_crude <- simulateResiduals(fittedModel = m_volumetry_diff_rel_fu2_crude, plot = F)
 check_model(m_volumetry_diff_rel_fu2_crude)



 pval_volumetry_diff_rel_fu2 <- drop1(m_volumetry_diff_rel_fu2_crude,  test = "Chisq")$`Pr(>Chi)`[2]
 t_volumetry_diff_rel_fu2_crude <- table_regression(m_volumetry_diff_rel_fu2_crude, 
                                                  coef_names = c("randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation) - Relative change" ),  
												  include_intercept = FALSE)


# Frequncey Tables
 t_volumetry_diff_rel <- rbind(
    t_volumetry_diff_rel_postop_median,
    t_volumetry_diff_rel_fu2_median
)

# Regression table
 t_volumetry_diff_rel_crude <- rbind(t_volumetry_diff_rel_postop_crude, t_volumetry_diff_rel_fu2_crude)





# ----------------------------------------------------------------------------
# Ventricle improvement 
# ----------------------------------------------------------------------------
 t_volumetry_improve_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_v_reduciton_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Ventricle width reduction",
    test_type = "none"
)
 summary(m_ventricle_pop_crude <- MASS::polr(pop_48_72h_v_reduciton_fct ~ randomisation_group_stereo_ref, data = dta, method = "logistic"))
 m_ventricle_pop_crude_ci <- confint(m_ventricle_pop_crude)
 tstat_ventricle_pop_crude <- summary(m_ventricle_pop_crude)$coef[1, "t value"]
 t_ventricle_pop_crude <- data.frame(
    Coefficients = c("Ultrasound (vs Stereotactic navigation)"), 
    "Odds Ratios"  = round(exp(m_ventricle_pop_crude$coefficients),3),
    # lower = exp(m_ventricle_pop_crude_ci[1]),
    # upper = exp(m_ventricle_pop_crude_ci[2]),
    CI = paste(round(exp(m_ventricle_pop_crude_ci[1]),3),
               "-", 
               round(exp(m_ventricle_pop_crude_ci[2]) ,3)),
    "P-value" = nice_pvalues(2*pnorm(abs(tstat_ventricle_pop_crude), lower.tail = F))
)



 t_volumetry_improve_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_ventricle_red_fct,
    group = randomisation_group_fct,
    include_N_total = "all",include_zero_categories = TRUE,
    summary_title = "Ventricle width reduction",
    test_type = "none"
)
 summary(m_ventricle_fu2_crude <- MASS::polr(fu2_ventricle_red_fct ~ randomisation_group_stereo_ref, data = dta, method = "logistic"))
 m_ventricle_fu2_crude_ci <- confint(m_ventricle_fu2_crude)
 tstat_ventricle_fu2_crude <- summary(m_ventricle_fu2_crude)$coef[1, "t value"]

 t_ventricle_fu2_crude <- data.frame(
    Coefficients = c("Ultrasound (vs Stereotactic navigation)"), 
    "Odds Ratios"    = round(exp(m_ventricle_fu2_crude$coefficients),3),
    # lower = exp(m_ventricle_fu2_crude_ci[1]),
    # upper = exp(m_ventricle_fu2_crude_ci[2]),
    CI = paste(round(exp(m_ventricle_fu2_crude_ci[1]), 3),
               "-" ,
               round(exp(m_ventricle_fu2_crude_ci[2]), 3)),
    "P-value" = nice_pvalues(2*pnorm(abs(tstat_ventricle_fu2_crude), lower.tail = F))
)



# Regression table
 t_volumetry_ventricle_crude <- rbind(
    c("48-120h post operation", "", "", "", ""),
    t_ventricle_pop_crude, 
    c("2nd Follow-up", "", "", "", ""),
    t_ventricle_fu2_crude)


# Regression table
 t_forest_volumetry_ventricle_improve <- rbind(
    c("Ventricle improvement", "", "", "", ""),
    t_ventricle_pop_crude, 
    t_ventricle_fu2_crude)
 t_forest_volumetry_ventricle_improve$Coefficients[2:3] <- c("48-120h post operation","2nd Follow-up")




t_volumetry <- rbind(
    c("48-120h post operation", "", "", "", ""),
    # t_volumetry_diff_postop_mean,
    t_volumetry_diff_postop_median,
    # t_volumetry_diff_rel_postop_mean,
    t_volumetry_diff_rel_postop_median,
    
    c(" ", "", "", "", ""),
    c("2nd Follow-up ", "", "", "", ""),
    # t_volumetry_diff_fu2_mean,
    t_volumetry_diff_fu2_median,
    # t_volumetry_diff_rel_fu2_mean,
    t_volumetry_diff_rel_fu2_median
)

 t_volumetry_improve <- rbind(
    c("48-120h post operation", "", "", "", ""),
    t_volumetry_improve_postop,

    c(" ", "", "", "", ""),
    c("2nd Follow-up ", "", "", "", ""),
    t_volumetry_improve_fu2
)


# Regression Tables - All
t_volumetry_regression  <- rbind(
    c("48-120h post operation", "", "", "", ""),
    rbind(t_volumetry_diff_postop_crude, t_volumetry_diff_rel_postop_crude), 
    c("2nd Follow-up", "", "", "", ""),
    rbind(t_volumetry_diff_fu2_crude, t_volumetry_diff_rel_fu2_crude))

t_forest_volumetry  <- rbind(
    c("Absolute change", "", "", "", ""),
    rbind(t_volumetry_diff_postop_crude, 
          t_volumetry_diff_fu2_crude ), 
    c("Relative change", "", "", "", ""),
    rbind(t_volumetry_diff_rel_postop_crude, 
          t_volumetry_diff_rel_fu2_crude))


 t_forest_volumetry$Coefficients[2:3] <- c("48-120h post operation","2nd Follow-up" )
 t_forest_volumetry$Coefficients[5:6] <- c("48-120h post operation","2nd Follow-up" )
