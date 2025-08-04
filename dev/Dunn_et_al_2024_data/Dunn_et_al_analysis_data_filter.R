# Load data
#-------------------------------------------------------------------------------
data <- read.csv("dev/Dunn_et_al_2024_data/gen_data/Dunn_et_al_2024.csv")

# Filter observations
#-------------------------------------------------------------------------------
# Species
data <- subset(data, species %in% c("Pigeon", "Starling"))

# Sample Size
data <- subset(data, n > 2)

# IL schedules
data <- subset(
  data,
  (il_sched_a == "FR" & il_sched_b == "FR" & il_dur_a == 1 & il_dur_b == 1) |
    (il_sched_a == "VI" & il_sched_b == "VI")
)

# TL Schedules
data <- subset(
  data,
  apply(data[c("tl_sched_a1", "tl_sched_a2", "tl_sched_b1", "tl_sched_b2")],
    MARGIN = 1,
    function(row) all(row %in% c("FI", "FT"))
  )
)

# Remove identical contingencies on alternatives
data <- with(data, {
  # IL Matches
  same_il <- (il_dur_a == il_dur_b) & (il_sched_a == il_sched_b)

  # TL matches a1 to b1
  same_tl_direct <-
    (tl_dur_a1 == tl_dur_b1) & (tl_dur_a2 == tl_dur_b2) &
      (tl_p_a1 == tl_p_b1) & (tl_p_a2 == tl_p_b2) &
      (tr_p_a1 == tr_p_b1) & (tr_p_a2 == tr_p_b2)

  # TL matches a1 to b2
  same_tl_reverse <-
    (tl_dur_a1 == tl_dur_b2) & (tl_dur_a2 == tl_dur_b1) &
      (tl_p_a1 == tl_p_b2) & (tl_p_a2 == tl_p_b1) &
      (tr_p_a1 == tr_p_b2) & (tr_p_a2 == tr_p_b1)

  data[!(same_il & (same_tl_direct | same_tl_reverse)), ]
})

# Remove empty columns
data <- data[, colSums(!is.na(data)) > 0]

# Re-number row names
row.names(data) <- 1:nrow(data)
nrow(data)

# Write data
write.csv(data, "dev/Dunn_et_al_2024_data/gen_data/Dunn_et_al_2024_modeled_subset.csv",
  row.names = FALSE
)
