# -----------------------------------------------------------------------------
# Script Name: 07_NaVPS_analysis_times.R
# Purpose: Main analysis 
# Author: Florian Halbeisen
# Date Created: 2025-01-22
# -----------------------------------------------------------------------------
# Description:
# 1, Creating table for complication rate 
# 2. Creating table for PROMS
# -----------------------------------------------------------------------------

create_model <- function(y, adjustment = c("crude", "adjusted")){
    adjustment_type <- match.arg(adjustment)
    if(adjustment_type == "crude"){
        formula <- as.formula(paste(y, "~ randomisation_group_stereo_ref"))
    } else {
        formula <-  as.formula(paste(y, "~ randomisation_group_stereo_ref +  bmi + op0_previous_burr_hole + opd0_surgeon_in_years + disease_hydrocephalus"))
    }
    formula
}

create_linear_reg <- function(y, adjustment = c("crude", "adjusted")){
    formula <- create_model(y, adjustment)
    lm(formula, dta)
}

create_logistic_reg <- function(y, adjustment = c("crude", "adjusted")){
    formula <- create_model(y, adjustment)
    glm(formula, family="binomial", data=dta)
}

create_regression <- function(y, model = c("linear", "logistic"), adjustment = c("crude", "adjusted") ){
    model_type <- match.arg(model)
    if(model_type == "linear"){
        model <- create_linear_reg(y, adjustment)
    } 
    if(model_type == "logistic"){
        model <- create_logistic_reg(y, adjustment)
    }
    return(model)
}



coef_names_overall <- c(
    "randomisation_group_stereo_refUltrasound" = "Ultrasound (vs Stereotactic navigation)",
    "Ultrasound" = "Ultrasound (vs Stereotactic navigation)",
    "bmi" = "BMI",
    "op0_previous_burr_holeYes" = "Previous burr hole",
    "opd0_surgeon_in_years" = "Experience surgeon (years)",
    "disease_hydrocephalus" = "Underlying disease causing hydrocephalus",
    "disease_hydrocephalusSubarachnoid hemorrhage" = "Subarachnoid hemorrhage",
    "disease_hydrocephalusOther bleeding" = "Other bleeding",
    "disease_hydrocephalusTumor" = "Tumor",
    "disease_hydrocephalusTrauma" = "Trauma",
    "disease_hydrocephalusOther" = "Other"
    
)


# -----------------------------------------------------------------------------
# Primary outcome - Surgical time 
# -----------------------------------------------------------------------------
## Overview
p_surgerytime_crude <- box_wilcox(
    dta,
    x = op0_sit_correct_value_num,
    group = randomisation_group_fct,
    include_points = TRUE,
    include_pvalue = FALSE,
    y_lab = "Corrected surgical intervention time (min)"
)

ggsave(paste0(path_study,"/TableFigure/boxplot_sugerytime.svg"),
       p_surgerytime_crude)


t_surgerytime_median <- summarize_continuous(
    df = dta,
    x = op0_sit_correct_value_num,
    group = randomisation_group_fct ,
    summary_title = "Surgical time in min (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


t_surgerytime_mean <- summarize_continuous(
    df = dta,
    x = op0_sit_correct_value_num,
    group = randomisation_group_fct ,
    summary_title = "Surgical time in min (Mean & SD)",
    summary_type = "mean_sd",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)

colnames(t_surgerytime_median) <- colnames(t_surgerytime_mean)
t_surgerytime <- rbind(t_surgerytime_mean, t_surgerytime_median)



## Crude model
# Linear regression
summary(m_surgerytime_crude <- lm(op0_sit_correct_value_num ~ randomisation_group_stereo_ref, dta))
t_surgerytime_crude <- table_regression(
    m_surgerytime_crude,
    coef_names = coef_names_overall,
    include_intercept = FALSE
)

# Assumption checks
assumptions_surgerytime_crude <- simulateResiduals(fittedModel = m_surgerytime_crude, plot = T)
check_model(m_surgerytime_crude)


## Adjusted model
summary(
    m_surgerytime_adjusted <- create_regression(
        "op0_sit_correct_value_num",
        model = "linear",
        adjustment = "adjusted"
    )
)

# Table 
t_surgerytime_adjusted <- table_regression(
    m_surgerytime_adjusted,
    coef_names = 
        c(coef_names_overall, 
          "randomisation_group_fctStereotactic navigation" = "Surgical intervention time - adjusted"),
    lrt_p = TRUE,
    title_category = "multi_category",
    include_intercept = FALSE
)

# Assumption checks
assumptions_surgerytime_adjusted <- simulateResiduals(fittedModel = m_surgerytime_adjusted, plot = T)
check_model(m_surgerytime_adjusted)



## Combined Regression Analysis
t_surgerytime_regression <- rbind(
    c("Crude Model", "", "", ""),
    t_surgerytime_crude,
    c("Adjusted Model", "", "", ""),
    t_surgerytime_adjusted
)


## Data for Forestplot
t_forest_surgerytime <- rbind(c("Surgical intervention time", rep("", 3)),
                              t_surgerytime_crude,
                              t_surgerytime_adjusted[1, ])

t_forest_surgerytime$Coefficients[2:3] <- c("Surgical intervention time - Crude", "Surgical intervention time - Adjusted")



# -----------------------------------------------------------------------------
# Sekundäre Outcome:
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Operation time in minutes (time in minutes from “Schnitt” to “Naht”)
# -----------------------------------------------------------------------------

## Overview
t_optime_median <- summarize_continuous(
    df = dta,
    x = opd0_op_time_in_min,
    group = randomisation_group_fct ,
    summary_title = "Operation time in min (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)

p_optime_crude <- box_wilcox(
    dta,
    x = opd0_op_time_in_min,
    group = randomisation_group_fct,
    include_points = TRUE,
    include_pvalue = FALSE,
    y_lab = "Operation time (min)"
)



## Cured model
summary(m_optime_crude <- lm(opd0_op_time_in_min ~ randomisation_group_stereo_ref, dta))
assumptions_optime_crude <- simulateResiduals(fittedModel = m_optime_crude, plot = T) # Has outliers --> robust regression

t_optime_crude <- table_regression(
    m_optime_crude,
    coef_names = coef_names_overall,
    include_intercept = FALSE
)

## Adjusted model
summary(
    m_optime_adjusted <- create_regression(
        "opd0_op_time_in_min",
        model = "linear",
        adjustment = "adjusted"
    )
)

t_optime_adjusted <- table_regression(
    m_optime_adjusted,
    coef_names =   c(coef_names_overall, 
                     "randomisation_group_fctStereotactic navigation" = "Operation time - adjusted"),
    lrt_p = TRUE,
    title_category = "multi_category",
    include_intercept = FALSE
)
# Assumption checks
assumptions_optime_adjusted <- simulateResiduals(fittedModel = m_optime_adjusted, plot = T)
check_model(m_optime_adjusted)

## Combined Regression Analysis
t_optime_regression <- rbind(
    c("Crude Model", "", "", ""),
    t_optime_crude,
    c("Adjusted Model", "", "", ""),
    t_optime_adjusted
)



## Data for Forestplot
t_forest_optime <- rbind(c("Operation time", rep("", 3)),
                         t_optime_crude,
                         t_optime_adjusted[1, ])


t_forest_optime$Coefficients[2:3] <- c("Operation time - Crude", "Operation time - Adjusted")


# -----------------------------------------------------------------------------
# Anaesthesia time in minutes (time in minutes from “Beginn Anästhesie” to “Ende Anästhesie”)
# -----------------------------------------------------------------------------
t_anesthesiatime_median <- summarize_continuous(
    df = dta,
    x = opd_0_anesthesia_timemin,
    group = randomisation_group_fct ,
    summary_title = "Anesthesia time in min (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none",
    include_total = TRUE,
    include_N = FALSE
)


p_anesthesiatime_crude <- box_wilcox(
    dta,
    x = opd_0_anesthesia_timemin,
    group = randomisation_group_fct,
    include_points = TRUE,
    include_pvalue = FALSE,
    y_lab = "Anesthesia time (min)"
    # breaks = seq(25,175,25)
)


## Cured model
summary(
    m_anesthesiatime_crude <- lm(opd_0_anesthesia_timemin ~ randomisation_group_stereo_ref, dta)
)
assumptions_anesthesiatime_crude <- simulateResiduals(fittedModel = m_anesthesiatime_crude, plot = T)

t_anesthesiatime_crude <- table_regression(
    m_anesthesiatime_crude,
    # coef_names = c("randomisation_group_fctStereotactic navigation" = "Anesthesia time"),
    coef_names = coef_names_overall,
    include_intercept = FALSE
)



## Adjusted model
summary(
    m_anesthesiatime_adjusted <- create_regression(
        "opd_0_anesthesia_timemin",
        model = "linear",
        adjustment = "adjusted"
    )
)

t_anesthesiatime_adjusted <- table_regression(
    m_anesthesiatime_adjusted,
    coef_names = c(coef_names_overall, 
                   "randomisation_group_fctStereotactic navigation" = "Anesthesia time - adjusted"),
    lrt_p = TRUE,
    title_category = "multi_category",
    include_intercept = FALSE
)
# Assumption checks
assumptions_anesthesiatime_adjusted <- simulateResiduals(fittedModel = m_anesthesiatime_adjusted, plot = T)


## Combined Regression Analysis
t_anesthesiatime_regression <- rbind(
    c("Crude Model", "", "", ""),
    t_anesthesiatime_crude,
    c("Adjusted Model", "", "", ""),
    t_anesthesiatime_adjusted
)



## Data for Forestplot
t_forest_anesthesiatime <- rbind(c("Anesthesia time", rep("", 3)),
                                 t_anesthesiatime_crude,
                                 t_anesthesiatime_adjusted[1, ])

 t_forest_anesthesiatime$Coefficients[2:3] <- c("Anesthesia time - Crude", "Anesthesia time - Adjusted")




# -----------------------------------------------------------------------------
# Combined Operation times
t_times_median <- rbind(t_optime_median, t_anesthesiatime_median)


t_time_regression <- rbind(
    c("Operation time", "", "", ""),
    t_optime_regression,
    c(" ", "", "", ""),
    c("Anesthesia time", "", "", ""),
    t_anesthesiatime_regression
    
)

 t_forest_time <- rbind(t_forest_surgerytime,
                       t_forest_optime,
                       t_forest_anesthesiatime)



# ------------------------------------------------------------------------------
# Forestplots
# -----------------------------------------------------------------------------
svg(paste0(path_study,"/TableFigure/forestplot_time.svg"), 
    width = 15, 
    height = 3.5)

create_ggforestplot(
    t_forest_time,
    title_estimate = "Estimate",
    size_study_label = 2,
    x_axis_title = paste0("Favours Ultrasound", strrep(" ", 105), "Favours Stereotactic")
)
# Close the device to save the file
dev.off()


# Open a PNG device
png(paste0(path_study,"/TableFigure/forestplot_sugerytime.svg"), 
    width = 6000, 
    height = 500, 
    res = 300)
# Render the forest plot and tables
create_ggforestplot(
    t_forest_surgerytime,
    title_estimate = "Estimate",
    vertical_line = 0,
    x_axis_title = paste0("Favours Ultrasound", strrep(" ", 175), "Favours Stereotactic")
)
# Close the device to save the file
dev.off()
