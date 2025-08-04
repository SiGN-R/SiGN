library(tidyverse)

# Load and merge data
#-------------------------------------------------------------------------------
fl <- c(
  "dev/Dunn_et_al_2024_data/raw_data/avian.csv",
  "dev/Dunn_et_al_2024_data/raw_data/rodent.csv",
  "dev/Dunn_et_al_2024_data/raw_data/primate.csv"
)

data_list <- lapply(fl, function(file) {
  read_csv(file,
    skip = 1,
    col_types = cols(
      `%RF1` = "c", `%Rf2` = "c",
      Exp = "c"
    )
  )
})

data <- bind_rows(data_list) |>
  select(6:27, 30:35, 40:49, 51:52, 54:55)

# Rename cols
#-------------------------------------------------------------------------------
data <- data |>
  rename(
    study = Study,
    year = Year,
    species = Species,
    exp = Exp,
    condition = Condition,
    forced_exposure = Forced_Exposure,
    cp = CP,
    il_dur_a = ILa_dur,
    il_dur_b = ILb_dur,
    tl_dur_a1 = TLa1_dur,
    tl_dur_a2 = TLa2_dur,
    tl_dur_b1 = TLb1_dur,
    tl_dur_b2 = TLb2_dur,
    tl_p_a1 = TLa1_p,
    tl_p_a2 = TLa2_p,
    tl_p_b1 = TLb1_p,
    tl_p_b2 = TLb2_p,
    tr_p_a1 = TLa1_rp,
    tr_p_a2 = TLa2_rp,
    tr_p_b1 = TLb1_rp,
    tr_p_b2 = TLb2_rp,
    il_sched_a = Sched_IL_a,
    il_sched_b = Sched_IL_b,
    tl_sched_a1 = Sched_TL_a1,
    tl_sched_a2 = Sched_TL_a2,
    tl_sched_b1 = Sched_TL_b1,
    tl_sched_b2 = Sched_TL_b2,
    ref = Reference,
    term_rein = Primary_Reinforcer,
    term_rein_cat = Primary_Rein_Cat,
    operant_response = Choice_Response,
    il_stim = IL_Stim,
    tl_s_splus_stim = S_plus_TL,
    tl_s_minus_stim = S_minus_TL,
    tl_unsig_stim = Unsig_TL,
    deprive_lvl = Deprivation,
    strain = Strain,
    sex = Sex,
    sex_MF_ratio = `Sex_M:F`,
    age = Age_Reported
  )

# Sort data
#-------------------------------------------------------------------------------
data <- data |>
  arrange(year, study, exp) |>
  mutate(row = row_number()) |>
  select(
    row, study, year,
    species, strain, sex, sex_MF_ratio, age,
    exp, condition,
    7:28,
    operant_response, forced_exposure, deprive_lvl,
    il_stim,
    tl_s_splus_stim, tl_s_minus_stim,
    tl_unsig_stim,
    term_rein, term_rein_cat,
    DOI, ref
  )

# Fix Sex
#-------------------------------------------------------------------------------
table(data$sex, useNA = "always")
data$sex <- ifelse(data$sex == "Male", "M", data$sex)

# Add versioning
#-------------------------------------------------------------------------------
data$data_version <- Sys.Date()

# Save data
#-------------------------------------------------------------------------------
write_csv(data, "dev/Dunn_et_al_2024_data/gen_data/Dunn_et_al_2024.csv")
