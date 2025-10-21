# -----------------------------------------------------------------------------
# Script Name: 08_NaVPS_analysis_shunts.R
# Purpose: Analysis - Puncture attempts and Shunt placement
# Author: Florian Halbeisen
# Date Created: 2025-04-03
# -----------------------------------------------------------------------------
#
# Input dataframe: dta
# -----------------------------------------------------------------------------

# REVIEW: General
# - Consistency: use TRUE/FALSE (not T/F) throughout. Confirm English spelling variants per journal.
# - Output: consider saving model summaries/tables using write.csv or flextable for appendix.


# -----------------------------------------------------------------------------
# Number of puncture attempts
# -----------------------------------------------------------------------------
# hist(dta$opd0_amount)

t_punctures <- summarize_categorical(
    df = dta,
    x = opd0_amount,
    group = randomisation_group_fct,
    include_N_total = "all",
	include_zero_categories = TRUE,
    summary_title = "Number of puncture attempts",
    test_type = "none"
)


summary(m_punctattempt_crude <- glm(opd0_amount ~ randomisation_group_stereo_ref, family="poisson", data=dta))
# AER::dispersiontest(m_punctattempt_crude, alternative = "two.sided")
# AER::dispersiontest(m_punctattempt_crude, alternative = c("less")) # Underdispersion --> Conway–Maxwell–Poisson regression? Or quasipoission?
# DHARMa::testDispersion(m_punctattempt_crude)
# assumptions_punctures_crude <- DHARMa::simulateResiduals(fittedModel = m_punctattempt_crude, plot = T)
# check_model(m_punctattempt_crude)


summary(m_punctattempt_crude_com <- glmmTMB(opd0_amount~randomisation_group_stereo_ref, family="compois", data=dta))
# confint(m_punctattempt_crude_com, method = "profile")  # Profile likelihood CI
assumptions_aunctures_crude_com <-DHARMa::simulateResiduals(fittedModel = m_punctattempt_crude_com, plot = T)
# testDispersion(m_punctattempt_crude_com)
# AIC(m_punctattempt_crude_com, m_punctattempt_crude)
# performance::check_model(m_punctattempt_crude_com)
# performance::check_residuals(m_punctattempt_crude_com)


# Table
t_punctattempt_crude_com <- broom.mixed::tidy(m_punctattempt_crude_com, conf.int = TRUE, exponentiate = TRUE)
t_punctattempt_crude_com$p.value <- nice_pvalues(t_punctattempt_crude_com$p.value)
t_punctattempt_crude_com$CI <- paste0(round(t_punctattempt_crude_com$conf.low, 3), 
                                      " - ", 
                                      round(t_punctattempt_crude_com$conf.high, 3))
t_punctattempt_crude_com <- t_punctattempt_crude_com[,c("term", "estimate", "CI", "p.value")]
t_punctattempt_crude_com <- t_punctattempt_crude_com[-1,]

t_punctattempt_crude_com$term <- "Ultrasound (vs Stereotactic navigation)"

# -----------------------------------------------------------------------------
# Puncture attempts binary - Yes / No
# -----------------------------------------------------------------------------

t_punctures_yn <- summarize_categorical(
    df = dta,
    x = punctures_yn,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Multiple puncture attempts",
    test_type = "none"
)

summary(
    m_punctures_yn_crude <- glm(
        punctures_ny ~ randomisation_group_stereo_ref,     # Change Ref
        family = "binomial",
        data = dta
    )
)
assumptions_punctures_yn_crude <- DHARMa::simulateResiduals(fittedModel = m_punctures_yn_crude, plot = T)


t_punctures_yn_crude <- table_regression(
    m_punctures_yn_crude,
    coef_names = coef_names_overall,
    lrt_p = TRUE,
    title_category = "multi_category",
    include_intercept = FALSE
)


t_forest_punctures_yn <- rbind(
        c("Multiple puncture attempts needed", "", "", "", ""),
        t_punctures_yn_crude)
t_forest_punctures_yn$Coefficients[2] <- c("Multiple puncture attempts needed")



# -----------------------------------------------------------------------------
# Catheter placement  - optimal vs. not optimal
# -----------------------------------------------------------------------------
# Rate der verschiedenen Sunt Fehllage Gradierungen
# 48-120h pot op

t_shunt_position_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_c_position_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Catheter position - 48-120h post operation",
    test_type = "none"
)

summary(
    m_optimal_pop48_72_crude <- glm(
        pop_48_72h_c_position_fct_rev ~ randomisation_group_stereo_ref,
        family = "binomial",
        data = dta
    )
)
assumptions_optimal_pop48_72_crude <- DHARMa::simulateResiduals(fittedModel = m_optimal_pop48_72_crude, plot = F)
t_optimal_pop48_72_crude <- table_regression(
    m_optimal_pop48_72_crude,
    include_intercept = FALSE,
    title_category = "multi_category",
    coef_names = coef_names_overall
)





# 2nd follow-up
t_shunt_position_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_cath_pos_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Catheter position - 2nd follow-up",
    test_type = "none"
)

summary(
    m_optimal_fu2_crude <- glm(
        fu2_cath_pos_fct_rev ~ randomisation_group_stereo_ref,
        family = "binomial",
        data = dta
    )
)
assumptions_optimal_fu2_crude <- DHARMa::simulateResiduals(fittedModel = m_optimal_fu2_crude, plot = F)
t_optimal_fu2_crude <- table_regression(
    m_optimal_fu2_crude,
    include_intercept = FALSE,
    title_category = "multi_category",
    # coef_names = c("Stereotactic navigation" = "2nd follow-up")
    coef_names = coef_names_overall
)




## Frequency Tables
 t_shunt_position <- rbind(t_shunt_position_postop, t_shunt_position_fu2)


## Combined Regression Analysis
 t_optimal_regression_crude <- rbind(
    c("48-120h post operation", "", "", ""),
    t_optimal_pop48_72_crude,
    c("2nd follow-up", "", "", ""),
    t_optimal_fu2_crude
)


## Data for Forestplot
 t_forest_optimal <- rbind(c("Optimal catheter placement", rep("", 3)),
                          t_optimal_pop48_72_crude,
                          t_optimal_fu2_crude)
 t_forest_optimal$Coefficients[2:3] <- c("48-120h post operation", "2nd follow-up")



# -----------------------------------------------------------------------------
# Catheter placement  - grade I to IV
# -----------------------------------------------------------------------------
# Post op
 t_shunt_position_grading_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_cath_grading_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Catheter position grading - 48-120h post operation",
    test_type = "none"
)

# Ordered logistic regression
summary(
    m_shunt_position_grading_postop_crude <- MASS::polr(
        pop_48_72h_cath_grading_fct ~ randomisation_group_stereo_ref,
        data = dta,
        method = "logistic"
    )
)
shunt_position_grading_postop_crude_ci <- confint(m_shunt_position_grading_postop_crude)
t_shunt_position_grading_postop_crude_pvalue <- drop1(m_shunt_position_grading_postop_crude, test = "Chisq")
t_shunt_position_grading_postop_crude <- data.frame(
    # Coefficients = c("Shunt position grading - 48-120h post operation"),
    Coefficients = c(" Ultrasound (vs Stereotactic navigation)"),
    OR    = round(exp(m_shunt_position_grading_postop_crude$coefficients),3),
    CI    = paste0( round(exp(shunt_position_grading_postop_crude_ci[1]),3),
                    " - ",
                    round(exp(shunt_position_grading_postop_crude_ci[2]),3)),
    pvalue = nice_pvalues(t_shunt_position_grading_postop_crude_pvalue$`Pr(>Chi)`[2])
)




# 2nd Follow-up
 t_shunt_position_grading_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_cath_pos_grad_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Catheter position grading - 2nd follow-up",
    test_type = "none"
)

summary(
    m_shunt_position_grading_fu2_crude <- MASS::polr(
        fu2_cath_pos_grad_fct ~ randomisation_group_stereo_ref,
        data = dta,
        method = "logistic"
    )
)
shunt_position_grading_fu2_crude_ci <- confint(m_shunt_position_grading_fu2_crude)
shunt_position_grading_fu2_crude_pvalue <- drop1(m_shunt_position_grading_fu2_crude, test = "Chisq")
 t_shunt_position_grading_fu2_crude <- data.frame(
    Coefficients = c("Ultrasound (vs Stereotactic navigation)"),
    OR    = round(exp(m_shunt_position_grading_fu2_crude$coefficients),3),
    CI    = paste0( round(exp(shunt_position_grading_fu2_crude_ci[1]),3),
                    " - ",
                    round(exp(shunt_position_grading_fu2_crude_ci[2]),3)),
    pvalue = nice_pvalues(shunt_position_grading_fu2_crude_pvalue$`Pr(>Chi)`[2])
)




# Frequncey Tables
 t_shunt_position_grad <- rbind(t_shunt_position_grading_postop,
                               t_shunt_position_grading_fu2)

# Regression Tables
 t_shunt_grading_crude <- rbind(
    # c("Catheter placement - Grade", rep("", 3)),
    c("48-120h post operation", rep("", 3)),
    t_shunt_position_grading_postop_crude,
    c("2nd follow-up", rep("", 3)),
    t_shunt_position_grading_fu2_crude
)



## Data for Forestplot
 t_forest_shunt_grading <- rbind(c("Catheter placement - Grading", rep("", 3)),
                          t_shunt_position_grading_postop_crude,
                          t_shunt_position_grading_fu2_crude)
 t_forest_shunt_grading$Coefficients[2:3] <- c("48-120h post operation", "2nd follow-up")


# ----------------------------------------------------------------------------
# Rate Shunt Fehllage (ja/nein)
# Any Dysfunction
 t_shunt_dyfunction_any <- summarize_categorical(
    df = dta,
    x = shunt_dysfunction_any_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    summary_title = "Shunt dysfunction - Any time",
    test_type = "none"
)

summary(
    m_shunt_dysfunction_any <- glm(
        shunt_dysfunction_any_ny ~ randomisation_group_stereo_ref,   # Reverse Reference
        family = binomial,
        data = dta
    )
)
assumptions_shunt_dysfunction_any_crude <- simulateResiduals(fittedModel = m_shunt_dysfunction_any, plot = F)
check_model(m_shunt_dysfunction_any)
 t_shunt_dysfunction_any_crude <- table_regression(m_shunt_dysfunction_any, 
                                                  include_intercept = FALSE,
                                                  coef_names = c("Ultrasound" = "Ultrasound (vs Stereotactic navigation) - Any time" )
                                                  )


## Data for Forestplot
 t_forest_shunt_dysfunction <- rbind(c("Shunt dysfunction", rep("", 3)),
                                t_shunt_dysfunction_any_crude)
 t_forest_shunt_dysfunction$Coefficients[2] <- c("Any time")



# Dysfunction post-op
 t_shunt_dyfunction_postop <- summarize_categorical(
    df = dta,
    x = pop_48_72h_sd_yn_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Shunt dysfunction - 48-120h post operation",
    test_type = "none"
)

# Dysfunction discharge
 t_shunt_dyfunction_discharge <- summarize_categorical(
    df = dta,
    x = discharge_shunt_dysfun_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Shunt dysfunction - Discharge",
    test_type = "none"
)

# Dysfunction 1st Follow-up
 t_shunt_dyfunction_fu1 <- summarize_categorical(
    df = dta,
    x = fu1_shunt_dysfun_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Shunt dysfunction - 1st Follow-up",
    test_type = "none"
)

# Dysfunction 2nd Follow-up
 t_shunt_dyfunction_fu2 <- summarize_categorical(
    df = dta,
    x = fu2_shunt_dysfun_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    include_zero_categories = TRUE,
    summary_title = "Shunt dysfunction - 2nd Follow-up",
    test_type = "none"
)



## Combined Table
 t_shunt_dyfunction <- rbind(
    t_shunt_dyfunction_any,
    t_shunt_dyfunction_postop,
    t_shunt_dyfunction_discharge,
    t_shunt_dyfunction_fu1,
    t_shunt_dyfunction_fu2
    
)
