# ================================================================
# Data preparation & variable engineering for shunt study dataset
# ================================================================

# ------------------------------------------------
# Basic row filtering
# - Keep rows with non-empty admission CT scan dates
# ------------------------------------------------
dta <- dta[dta$admission_ct_scan_date != "", ]


# ------------------------------------------------
# Numeric extraction from character field `op0_sit_correct_value`
# - Extract leading integer and coerce to numeric
# ------------------------------------------------
dta$op0_sit_correct_value_num <- gsub("^(\\d+).*", "\\1", dta$op0_sit_correct_value)
dta$op0_sit_correct_value_num <- as.numeric(dta$op0_sit_correct_value_num)

# ------------------------------------------------
# Randomisation group factors (two reference codings)
# ------------------------------------------------
dta$randomisation_group_fct <- factor(
  dta$randomisation_group,
  levels = c("Ultrasound", "Stereotactic navigation")
)

dta$randomisation_group_stereo_ref <- factor(
  dta$randomisation_group,
  levels = c("Stereotactic navigation", "Ultrasound")
)


# ------------------------------------------------
# Demographics & admission status factors
# ------------------------------------------------
dta$gender_fct <- factor(
  dta$gender,
  levels = c("female", "male"),
  labels = c("Female", "Male")
)

dta$admission_gos_fct <- factor(
  dta$admission_gos,
  levels = c(
    "5 Geringe Behinderung.", "4 Mässige Behinderung.", "3 Schwere Behinderung.",
    "2 Persistierender vegetativer Zustand.", "1 Tod."
  )
)

dta$prior_vp_shunt_op_fct <- factor(dta$prior_vp_shunt_op, levels = c("Yes", "No"))
dta$prior_evd_fct         <- factor(dta$prior_evd,         levels = c("Yes", "No"))
dta$prior_head_op_fct     <- factor(dta$prior_head_op,     levels = c("Yes", "No"))

# Note: source variable uses "no" (lowercase) in one level
dta$admission_headache_fct <- factor(
  dta$admission_headache,
  levels = c("Yes", "no"),
  labels = c("Yes", "No")
)

dta$admission_vomitus_fct          <- factor(dta$admission_vomitus,          levels = c("Yes", "No"))
dta$admission_coma_fct             <- factor(dta$admission_coma,             levels = c("Yes", "No"))
dta$admission_gait_disturbance_fct <- factor(dta$admission_gait_disturbance, levels = c("Yes", "No"))
dta$admission_dementia_fct         <- factor(dta$admission_dementia,         levels = c("Yes", "No"))

# Note: original column is spelled "urunary_incontinence"
dta$urunary_incontinence_fct <- factor(dta$urunary_incontinence, levels = c("Yes", "No"))

# Collapse lateralized deficits into binary Yes/No
dta$admission_motor_deficit_binary <- factor(
  dta$admission_motor_deficit,
  levels = c("right", "left", "both sides", "no"),
  labels = c("Yes", "Yes", "Yes", "No")
)
dta$admission_sensory_deficit_binary <- factor(
  dta$admission_sensory_deficit,
  levels = c("right", "left", "both sides", "no"),
  labels = c("Yes", "Yes", "Yes", "No")
)

dta$admission_aphasia_fct <- factor(dta$admission_aphasia, levels = c("Yes", "No"))
dta$admission_delir_fct   <- factor(dta$admission_delir,   levels = c("Yes", "No"))


# ------------------------------------------------
# Etiology of hydrocephalus (single-label derived from checkbox-like fields)
# - Priority follows the assignment order below
# ------------------------------------------------
dta$disease_hydrocephalus <- NA
# Normal pressure hydrocephalus
dta$disease_hydrocephalus[dta$disease_hydrocephalus_1 == "Checked"] <- "Normal pressure hydrocephalus"
# Subarachnoid hemorrhage
dta$disease_hydrocephalus[dta$disease_hydrocephalus_2 == "Checked"] <- "Subarachnoid hemorrhage"
# Other bleeding (ICB, intraventricular, cerebellar)
dta$disease_hydrocephalus[dta$disease_hydrocephalus_3 == "Checked"] <- "Other bleeding"
dta$disease_hydrocephalus[dta$disease_hydrocephalus_4 == "Checked"] <- "Other bleeding"
# Tumor
dta$disease_hydrocephalus[dta$disease_hydrocephalus_6 == "Checked"] <- "Tumor"
# Trauma
dta$disease_hydrocephalus[dta$disease_hydrocephalus_5 == "Checked"] <- "Trauma"
# Other (infection, post Chiari decompression)
dta$disease_hydrocephalus[dta$disease_hydrocephalus_7 == "Checked"] <- "Other"
dta$disease_hydrocephalus[dta$disease_hydrocephalus_8 == "Checked"] <- "Other"

dta$disease_hydrocephalus <- factor(
  dta$disease_hydrocephalus,
  levels = c(
    "Normal pressure hydrocephalus", "Subarachnoid hemorrhage",
    "Other bleeding", "Tumor", "Trauma", "Other"
  )
)


# ================================================================
# Shunt dysfunction composites & factor recodings
# ================================================================
# Any shunt dysfunction across timepoints; Missing if any non-empty non-Yes/No
dta$shunt_dysfunction_any <- apply(
  dta[, c("pop_48_72h_sd_yn", "discharge_shunt_dysfun", "fu1_shunt_dysfun", "fu2_shunt_dysfun")],
  1,
  function(row) {
    if ("Yes" %in% row) {
      "Yes"
    } else if (all(row %in% c("No", ""))) {
      "No"
    } else {
      "Missing"
    }
  }
)

dta$shunt_dysfunction_any_fct <- factor(dta$shunt_dysfunction_any, levels = c("Yes", "No"))
dta$shunt_dysfunction_any_ny  <- factor(dta$shunt_dysfunction_any, levels = c("No", "Yes"))

# Timepoint-specific factors with explicit NA handling
dta <- dta %>%
  dplyr::mutate(
    pop_48_72h_sd_yn_fct = dplyr::na_if(pop_48_72h_sd_yn, ""),
    pop_48_72h_sd_yn_fct = factor(pop_48_72h_sd_yn_fct, levels = c("Yes", "No"))
  ) %>%
  dplyr::mutate(
    discharge_shunt_dysfun_fct = dplyr::na_if(discharge_shunt_dysfun, ""),
    discharge_shunt_dysfun_fct = factor(discharge_shunt_dysfun_fct, levels = c("Yes", "No"))
  ) %>%
  dplyr::mutate(
    fu1_shunt_dysfun_fct = dplyr::na_if(fu1_shunt_dysfun, ""),
    fu1_shunt_dysfun_fct = factor(fu1_shunt_dysfun_fct, levels = c("Yes", "No"))
  )

# Convert NA to explicit "Missing" level for follow-ups
dta$fu1_shunt_dysfun_fct <- forcats::fct_na_value_to_level(dta$fu1_shunt_dysfun_fct, level = "Missing")

dta <- dta %>%
  dplyr::mutate(
    fu2_shunt_dysfun_fct = dplyr::na_if(fu2_shunt_dysfun, ""),
    fu2_shunt_dysfun_fct = factor(fu2_shunt_dysfun_fct, levels = c("Yes", "No"))
  )
dta$fu2_shunt_dysfun_fct <- forcats::fct_na_value_to_level(dta$fu2_shunt_dysfun_fct, level = "Missing")


# ================================================================
# Catheter position & grading (post-op and FU2)
# ================================================================
dta <- dta %>%
  dplyr::mutate(
    pop_48_72h_c_position_fct    = dplyr::na_if(pop_48_72h_c_position, ""),
    pop_48_72h_c_position_fct    = factor(pop_48_72h_c_position_fct, levels = c("optimal", "non-optimal")),
    pop_48_72h_c_position_fct_rev = factor(pop_48_72h_c_position_fct, levels = c("non-optimal", "optimal"))
  ) %>%
  dplyr::mutate(
    fu2_cath_pos_fct    = dplyr::na_if(fu2_cath_pos, ""),
    fu2_cath_pos_fct    = factor(fu2_cath_pos_fct, levels = c("optimal", "non-optimal")),
    fu2_cath_pos_fct_rev = factor(fu2_cath_pos_fct, levels = c("non-optimal", "optimal"))
  ) %>%
  dplyr::mutate(
    pop_48_72h_cath_grading_fct = dplyr::na_if(pop_48_72h_cath_grading, ""),
    pop_48_72h_cath_grading_fct = factor(pop_48_72h_cath_grading_fct, levels = c("Grade I", "Grade II", "Grade III", "Grade IV"))
  ) %>%
  dplyr::mutate(
    fu2_cath_pos_grad_fct = dplyr::na_if(fu2_cath_pos_grad, ""),
    fu2_cath_pos_grad_fct = factor(fu2_cath_pos_grad_fct, levels = c("Grade I", "Grade II", "Grade III", "Grade IV"))
  )


# ================================================================
# Clinical outcome – Modified Rankin Scale (collapsed categories)
#   Favorable: mRS 0–2; Bad: mRS 3–5; Dead: 6
# ================================================================
dta$admission_mrs_short <- factor(
  dta$admission_mrs,
  levels = c(
    "0 No symptoms.",
    "1 No significant disability.",
    "2 Slight disability.",
    "3 Moderate disability.",
    "4 Moderately severe disability.",
    "5 Severe disability.",
    "6 Dead."
  ),
  labels = c(
    "Favorable outcome", "Favorable outcome", "Favorable outcome",
    "Bad outcome", "Bad outcome", "Bad outcome",
    "Dead"
  )
)

dta$pop_48_72h_mrs_short <- factor(
  dta$pop_48_72h_mrs,
  levels = c(
    "0 No symptoms.",
    "1 No significant disability.",
    "2 Slight disability.",
    "3 Moderate disability.",
    "4 Moderately severe disability.",
    "5 Severe disability.",
    "6 Dead."
  ),
  labels = c(
    "Favorable outcome", "Favorable outcome", "Favorable outcome",
    "Bad outcome", "Bad outcome", "Bad outcome",
    "Dead"
  )
)

dta$discharge_mrs_short <- factor(
  dta$discharge_mrs,
  levels = c(
    "0 No symptoms.",
    "1 No significant disability.",
    "2 Slight disability.",
    "3 Moderate disability.",
    "4 Moderately severe disability.",
    "5 Severe disability.",
    "6 Dead."
  ),
  labels = c(
    "Favorable outcome", "Favorable outcome", "Favorable outcome",
    "Bad outcome", "Bad outcome", "Bad outcome",
    "Dead"
  )
)

dta$fu1_mrs_short <- factor(
  dta$fu1_mrs,
  levels = c(
    "0 No symptoms.",
    "1 No significant disability.",
    "2 Slight disability.",
    "3 Moderate disability.",
    "4 Moderately severe disability.",
    "5 Severe disability.",
    "6 Dead."
  ),
  labels = c(
    "Favorable outcome", "Favorable outcome", "Favorable outcome",
    "Bad outcome", "Bad outcome", "Bad outcome",
    "Dead"
  )
)

dta$fu2_mrs_short <- factor(
  dta$fu2_mrs,
  levels = c(
    "0 No symptoms.",
    "1 No significant disability.",
    "2 Slight disability.",
    "3 Moderate disability.",
    "4 Moderately severe disability.",
    "5 Severe disability.",
    "6 Dead."
  ),
  labels = c(
    "Favorable outcome", "Favorable outcome", "Favorable outcome",
    "Bad outcome", "Bad outcome", "Bad outcome",
    "Dead"
  )
)

# Quick check (invisible)
dta[, c("discharge_mrs_short", "fu1_mrs_short", "fu2_mrs_short")]


# ================================================================
# Complications (any) across timepoints + NY orderings
# ================================================================
dta$opd0_opcomplications_fct    <- factor(dta$opd0_opcomplications,    levels = c("Yes", "No"))
dta$pop_48_72h_complications_fct <- factor(dta$pop_48_72h_complications, levels = c("Yes", "No"))
dta$discharge_compl_fct         <- factor(dta$discharge_compl,         levels = c("Yes", "No"))
dta$fu1_compl_fct               <- factor(dta$fu1_compl,               levels = c("Yes", "No"))
dta$fu2_compl_fct               <- factor(dta$fu2_compl,               levels = c("Yes", "No"))

# Reversed level order (No/Yes)
dta$opd0_opcomplications_ny     <- factor(dta$opd0_opcomplications,    levels = c("No", "Yes"))
dta$pop_48_72h_complications_ny <- factor(dta$pop_48_72h_complications, levels = c("No", "Yes"))
dta$discharge_compl_ny          <- factor(dta$discharge_compl,         levels = c("No", "Yes"))
dta$fu1_compl_ny                <- factor(dta$fu1_compl,               levels = c("No", "Yes"))
dta$fu2_compl_ny                <- factor(dta$fu2_compl,               levels = c("No", "Yes"))

# Any complication composite across timepoints (+ Missing if partially unknown)
dta$complications_any <- apply(
  dta[, c("opd0_opcomplications_fct", "pop_48_72h_complications_fct", "discharge_compl_fct", "fu1_compl_fct", "fu2_compl_fct")],
  1,
  function(row) {
    if ("Yes" %in% row) {
      "Yes"
    } else if (all(row %in% c("No", NA))) {
      "No"
    } else {
      "Missing"
    }
  }
)
dta$complications_any_fct <- factor(dta$complications_any, levels = c("Yes", "No"))
dta$complications_any_ny  <- factor(dta$complications_any, levels = c("No", "Yes"))


# ------------------------------------------------
# Type of complication (timepoint-specific)
# - Derived from checkbox-style indicators
# ------------------------------------------------
# Post-op 48–72h
dta$type_complication_pop_48_72h <- NA
dta$type_complication_pop_48_72h[dta$pop_48_72h_comp_yes_1 == "Checked"] <- "Infection"
dta$type_complication_pop_48_72h[dta$pop_48_72h_comp_yes_2 == "Checked"] <- "Bleeding"
dta$type_complication_pop_48_72h[dta$pop_48_72h_comp_yes_3 == "Checked"] <- "Seizure"
dta$type_complication_pop_48_72h[dta$pop_48_72h_comp_yes_4 == "Checked"] <- "Fracture"
dta$type_complication_pop_48_72h[dta$pop_48_72h_comp_yes_5 == "Checked"] <- "Other"

# Discharge
dta$type_complication_discharge <- NA
dta$type_complication_discharge[dta$discharge_comp_3 == "Checked"] <- "Infection"
dta$type_complication_discharge[dta$discharge_comp_4 == "Checked"] <- "Bleeding"
dta$type_complication_discharge[dta$discharge_comp_5 == "Checked"] <- "Seizure"
dta$type_complication_discharge[dta$discharge_comp_6 == "Checked"] <- "Fracture"
dta$type_complication_discharge[dta$discharge_comp_7 == "Checked"] <- "Other"

# Follow-up 1
dta$type_complication_fu1 <- NA
dta$type_complication_fu1[dta$fu1_comp_reason_3 == "Checked"] <- "Infection"
dta$type_complication_fu1[dta$fu1_comp_reason_4 == "Checked"] <- "Bleeding"
dta$type_complication_fu1[dta$fu1_comp_reason_5 == "Checked"] <- "Seizure"
dta$type_complication_fu1[dta$fu1_comp_reason_6 == "Checked"] <- "Fracture"
dta$type_complication_fu1[dta$fu1_comp_reason_7 == "Checked"] <- "Other"

# Follow-up 2
dta$type_complication_fu2 <- NA
dta$type_complication_fu2[dta$fu2_comp_3 == "Checked"] <- "Infection"
dta$type_complication_fu2[dta$fu2_comp_4 == "Checked"] <- "Bleeding"
dta$type_complication_fu2[dta$fu2_comp_5 == "Checked"] <- "Seizure"
dta$type_complication_fu2[dta$fu2_comp_6 == "Checked"] <- "Fracture"
dta$type_complication_fu2[dta$fu2_comp_7 == "Checked"] <- "Other"


# ================================================================
# Imaging-derived metrics: volumetry & Evans index (absolute/relative deltas)
# ================================================================
# Volumetry differences vs baseline
dta$volumetry_diff_postop <- dta$pop_48_72h_volumetry - dta$volumetry_ventricles_cm3
dta$volumetry_diff_fu2    <- dta$fu2_volumetry        - dta$volumetry_ventricles_cm3

dta$volumetry_diff_rel_postop <- (dta$pop_48_72h_volumetry - dta$volumetry_ventricles_cm3) / dta$volumetry_ventricles_cm3 * 100
dta$volumetry_diff_rel_fu2 <- (dta$fu2_volumetry - dta$volumetry_ventricles_cm3) / dta$volumetry_ventricles_cm3 * 100

# Ventricular size change (categorical impressions)
dta <- dta %>%
  dplyr::mutate(
    pop_48_72h_v_reduciton_fct = dplyr::na_if(pop_48_72h_v_reduciton, ""),
    pop_48_72h_v_reduciton_fct = factor(pop_48_72h_v_reduciton_fct, levels = c("yes", "same", "worse"))
  ) %>%
  dplyr::mutate(
    fu2_ventricle_red_fct = dplyr::na_if(fu2_ventricle_red, ""),
    fu2_ventricle_red_fct = factor(fu2_ventricle_red_fct, levels = c("yes", "same", "worse"))
  )

# Evans Index differences vs baseline
dta$evans_diff_postop <- dta$pop_48_72h_evans_index - dta$evans_index
dta$evans_diff_fu2    <- dta$fu2_evans                - dta$evans_index

dta$evans_diff_rel_postop <- (dta$pop_48_72h_evans_index - dta$evans_index) / dta$evans_index * 100
dta$evans_diff_rel_fu2    <- (dta$fu2_evans             - dta$evans_index) / dta$evans_index * 100

dta <- dta %>%
  dplyr::mutate(
    pop_48_72h_ei_improved_fct = dplyr::na_if(pop_48_72h_ei_improved, ""),
    pop_48_72h_ei_improved_fct = factor(pop_48_72h_ei_improved_fct, levels = c("improved", "same", "worse"))
  ) %>%
  dplyr::mutate(
    fu2_ei_improved_fct = dplyr::na_if(fu2_ei_improved, ""),
    fu2_ei_improved_fct = factor(fu2_ei_improved_fct, levels = c("improved", "same", "worse"))
  )


# ================================================================
# Procedure details
# ================================================================
# Number of puncture attempts (>1 => Yes)
dta$punctures_yn <- factor(ifelse(dta$opd0_amount > 1, "Yes", "No"), levels = c("Yes", "No"))
dta$punctures_ny <- factor(ifelse(dta$opd0_amount > 1, "Yes", "No"), levels = c("No", "Yes"))

# Revision surgery flags by timepoint
dta$opd0_revision_surgery_fct <- factor(dta$opd0_revision_surgery, levels = c("Yes", "No"))
dta$pop_48_72h_rs_fct         <- factor(dta$pop_48_72h_rs,         levels = c("Yes", "No"))
dta$discharge_rs_fct          <- factor(dta$discharge_rs,          levels = c("Yes", "No"))
dta$fu1_rs_fct                <- factor(dta$fu1_rs,                levels = c("Yes", "No"))
dta$fu2_rs_fct                <- factor(dta$fu2_rs,                levels = c("Yes", "No"))

# Any revision due to infection across timepoints (binary labeling)
dta$revision_any_infection <- ifelse(
  dta$pop_48_72h_rs_reason_2 == "Checked" |
    dta$discharge_reas_for_rev_2 == "Checked" |
    dta$fu1_reas_for_rev_2 == "Checked" |
    dta$fu2_reas_for_rev_2 == "Checked",
  "Infection",
  "No Infection"
)
