# -----------------------------------------------------------------------------
# Script Name: 04_NaVPS_characteristics.R
# Purpose: Main analysis 
# Author: Florian Halbeisen
# Date Created: 2025-01-23
# -----------------------------------------------------------------------------
# Description:
# 1, Creating table for study characteristics
# 
#
# Input dataframe: dta
# Output dataframe: t_char
# Dependencies R: dyplr, tidyr
# -----------------------------------------------------------------------------



t_char_age <- summarize_continuous(
    df = dta,
    x = age_in_years,
    group = randomisation_group_fct ,
    summary_title = "Age, years (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)

t_char_height <- summarize_continuous(
    df = dta,
    x = height,
    group = randomisation_group_fct ,
    summary_title = "Height, cm (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)


t_char_sex <- summarize_categorical(
    df = dta,
    x = gender_fct,
    group = randomisation_group_fct,
    include_N_total = "all",
    summary_title = "Sex",
    test_type = "none"
)

t_char_bmi <- summarize_continuous(
    df = dta,
    x = bmi,
    group = randomisation_group_fct ,
    summary_title = "BMI (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)


t_char_evans_index <- summarize_continuous(
    df = dta,
    x = evans_index,
    group = randomisation_group_fct ,
    summary_title = "Admission Evans Index (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)



t_char_volumetry_ventricles_cm3 <- summarize_continuous(
    df = dta,
    x = volumetry_ventricles_cm3,
    group = randomisation_group_fct ,
    summary_title = "Admission Volumetry of side ventricles (in cm3) (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)


t_char_admission_gcs <- summarize_continuous(
    df = dta,
    x = admission_gcs,
    group = randomisation_group_fct ,
    summary_title = "Admission GSC (Median & IQR)",
    summary_type = "median_iqr",
    test_type = "none", 
    include_total = TRUE, 
    include_N = FALSE
)

t_char_admission_mrs <- summarize_categorical(
    df = dta,
    x = admission_mrs,
    group = randomisation_group_fct,
    include_N_total = "all",
    summary_title = "Admission Modified Ranking Scale",
    test_type = "none"
)


t_char_admission_gos <- summarize_categorical(
    df = dta,
    x = admission_gos_fct,
    group = randomisation_group_fct,
    include_N_total = "all", include_zero_categories = T,
    summary_title = "Admission Glasgow Outcome scale",
    test_type = "none"
)

t_char_prior_vp_shunt <- summarize_categorical(
    df = dta,
    x = prior_vp_shunt_op_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Prior VP shunt OP",
    test_type = "none"
)


t_char_prior_evd <- summarize_categorical(
    df = dta,
    x = prior_evd_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Prior EVD OP",
    test_type = "none"
)

t_char_prior_head_op <- summarize_categorical(
    df = dta,
    x = prior_head_op_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Prior head operation",
    test_type = "none"
)

t_char_disease_hydrocephalus <- summarize_categorical(
    df = dta,
    x = disease_hydrocephalus,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Underlying disease causing hydrocephalus",
    test_type = "none"
)


## Neurological symptoms 
# Headaches
t_char_admission_headache <- summarize_categorical(
    df = dta,
    x = admission_headache_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Headache",
    test_type = "none"
)

# Vomitus
t_char_admission_vomitus <- summarize_categorical(
    df = dta,
    x = admission_vomitus_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Vomitus",
    test_type = "none"
)

# Coma
t_char_admission_coma <- summarize_categorical(
    df = dta,
    x = admission_coma_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Coma",
    test_type = "none"
)

# Gait disturbances
t_char_admission_gait_disturbance <- summarize_categorical(
    df = dta,
    x = admission_gait_disturbance_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Gait disturbance",
    test_type = "none"
)
# Dementia
t_char_admission_dementia <- summarize_categorical(
    df = dta,
    x = admission_dementia_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Dementia",
    test_type = "none"
)

# Urinary incontinence 
t_char_urunary_incontinence <- summarize_categorical(
    df = dta,
    x = urunary_incontinence_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Urinary incontinence",
    test_type = "none"
)

# Motor deficit
t_char_admission_motor_deficit <- summarize_categorical(
    df = dta,
    x = admission_motor_deficit_binary,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Motor deficit",
    test_type = "none"
)

# Sensory deficit
t_char_admission_sensory_deficit <- summarize_categorical(
    df = dta,
    x = admission_sensory_deficit_binary,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Sensory deficit",
    test_type = "none"
)

# Aphasia
t_char_admission_aphasia <- summarize_categorical(
    df = dta,
    x = admission_aphasia_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Aphasia",
    test_type = "none"
)

# Delir
t_char_admission_delir <- summarize_categorical(
    df = dta,
    x = admission_delir_fct,
    group = randomisation_group_fct,
    include_N_total = "all", 
    summary_title = "Admission Delir",
    test_type = "none"
)


# Incldue only Yes for each symptom into final table
# Replace Yes with Symptom name
t_char_admission_headache[2,1] <- t_char_admission_headache[1,1]
t_char_admission_vomitus[2,1]  <- t_char_admission_vomitus[1,1]
t_char_admission_coma[2,1]  <- t_char_admission_coma[1,1]
t_char_admission_gait_disturbance[2,1]  <- t_char_admission_gait_disturbance[1,1]
t_char_admission_dementia[2,1]  <- t_char_admission_dementia[1,1]
t_char_urunary_incontinence[2,1]  <- t_char_urunary_incontinence[1,1]
t_char_admission_motor_deficit[2,1]  <- t_char_admission_motor_deficit[1,1]
t_char_admission_sensory_deficit[2,1]  <- t_char_admission_sensory_deficit[1,1]
t_char_admission_aphasia[2,1]  <- t_char_admission_aphasia[1,1]
t_char_admission_delir[2,1]  <- t_char_admission_delir[1,1]

# Combine yes of each symptom 
t_char_admission_symptoms <- rbind(
    t_char_admission_headache[2,],
    t_char_admission_vomitus[2,],
    t_char_admission_coma[2,],
    t_char_admission_gait_disturbance[2,],
    t_char_admission_dementia[2,],
    t_char_urunary_incontinence[2,],
    t_char_admission_motor_deficit[2,],
    t_char_admission_sensory_deficit[2,],
    t_char_admission_aphasia[2,],
    t_char_admission_delir[2,]
)


# -----------------------------------------------------------------------------
# Combining tables into charactersitics table 

# Rename colnames so we can use rbind to combine them
colnames(t_char_age) <- colnames(t_char_sex)
colnames(t_char_height) <- colnames(t_char_sex)
colnames(t_char_bmi) <- colnames(t_char_sex)
colnames(t_char_evans_index) <- colnames(t_char_sex)
colnames(t_char_volumetry_ventricles_cm3) <- colnames(t_char_sex)
colnames(t_char_admission_gcs) <- colnames(t_char_sex)



t_char <- rbind(
    t_char_age,
    t_char_sex,
    t_char_height,
    t_char_bmi,
    t_char_evans_index,
    t_char_volumetry_ventricles_cm3,
    
    t_char_admission_gcs,
    t_char_admission_mrs,
    t_char_admission_gos,
    
    t_char_prior_vp_shunt,
    t_char_prior_evd,
    t_char_prior_head_op,
    t_char_disease_hydrocephalus,
    t_char_admission_symptoms
)
