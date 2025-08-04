#' SiGN Model Prediction
#'
#' Computes predicted choice proportions using the
#' SiGN (Signals for Good News) model, based on input parameters describing
#' reinforcement schedules, stimulus durations, and signalling conditions.
#'
#' @param params A named list of model parameters, typically created with
#' [choice_params()]. This list must include all required values such as
#' initial and terminal link durations, terminal link entry and reinforcement
#' probabilities, schedule types. See [choice_params()] for a complete
#' description of required elements.
#'
#' @returns
#' An object of class `"SiGN"`, which includes:
#' \itemize{
#'   \item \code{cp}: A numeric vector of predicted choice proportions.
#'   \item \code{details}: A data frame containing intermediate model components
#'         such as reinforcement rates, delay reductions, beta values, and
#'         signalling status.
#' }
#'
#' @details
#' Specifics on the calculation can be found in Dunn et al. (2024).
#'
#' @references
#' Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).
#' Suboptimal choice: A review and quantification of the signal for good news
#' (SiGN) model. *Psychological Review*. *131*(1), 58-78.
#' \url{https://doi.org/10.1037/rev0000416}
#'
#' @examples
#' # Standard FR 1 Initial Links
#' params <- choice_params(profile = "zentall")
#' result <- SiGN(params)
#' result$cp       # predicted choice proportion
#' result$details  # access intermediate terms
#'
#' # Long VI 30 Initial Links
#' params <- choice_params(profile = "zentall",
#'   il_dur_a = 30, il_dur_b = 30, il_sched_a = "VI", il_sched_b = "VI")
#' result <- SiGN(params)
#' result$cp
#' result$details
#' @export
#-------------------------------------------------------------------------------
SiGN <- function(params){

  if (!is.list(params)) {
    stop("Please provide a list of parameters created with `choice_params()`.")
  }

  # Store values to local environment
  list2env(params, envir = environment())

  # Overall terminal reinforcement (tr) probability (p)
  tr_p_a <- tl_p_a1 * tr_p_a1 + tl_p_a2 * tr_p_a2
  tr_p_b <- tl_p_b1 * tr_p_b1 + tl_p_b2 * tr_p_b2

  # Initial link info
  #-------------------------------------------------------------------------------
  # Average amount of times through initial link to primary reinforcement
  # il_dur_a / tr_p_a
  # il_dur_b / tr_p_b

  # Waiting for terminal reinforcement in initial link
  # il_dur_a * (1 / tr_p_a)
  # il_dur_b * (1 / tr_p_b)

  # Terminal link info
  #-------------------------------------------------------------------------------

  # Adjust duration of tl signals for extinction
  tl_dur_a1 <- s_delta(tl_dur_a1, tr_p_a1, s_delta)
  tl_dur_a2 <- s_delta(tl_dur_a2, tr_p_a2, s_delta)

  tl_dur_b1 <- s_delta(tl_dur_b1, tr_p_b1, s_delta)
  tl_dur_b2 <- s_delta(tl_dur_b2, tr_p_b2, s_delta)

  # Average time spent in terminal link phase
  tl_dur_a <- tl_p_a1 * tl_dur_a1 + tl_p_a2 * tl_dur_a2
  tl_dur_b <- tl_p_b1 * tl_dur_b1 + tl_p_b2 * tl_dur_b2

  # Average time to terminal reinforcement in terminal link phase
  # tl_dur_a * (il_dur_a / tr_p_a)
  # tl_dur_b * (il_dur_b / tr_p_b)

  # Primary Reinforcement Rate
  #-------------------------------------------------------------------------------
  # Individual primary rein. rates
  r_a <- r(il_dur_a, il_dur_b, tl_dur_a, tr_p_a, il_sched_a, il_sched_b)
  r_b <- r(il_dur_b, il_dur_a, tl_dur_b, tr_p_b, il_sched_a, il_sched_b)

  # Primary rein. rates with common IL
  # Note: Requires concurrent VIs to be selected
  r_a_com <- r(il_dur_a, il_dur_b, tl_dur_a, tr_p_a, il_sched_a, il_sched_b,
               common_il_dur = TRUE
  )
  r_b_com <- r(il_dur_b, il_dur_a, tl_dur_b, tr_p_b, il_sched_a, il_sched_b,
               common_il_dur = TRUE
  )

  # Big T
  # (Average time to terminal reinforcement from onset of initial links)
  #-------------------------------------------------------------------------------

  # Note: this is designed to account for unequal initial link durations
  # (See the OSF document for more details)

  f_a <- ((1 / il_dur_a) / ((1 / il_dur_a) + (1 / il_dur_b))) *
    (tr_p_a / (tr_p_a + tr_p_b))

  f_b <- (1 - (1 / il_dur_a) / ((1 / il_dur_a) + (1 / il_dur_b))) *
    (1 - tr_p_a / (tr_p_a + tr_p_b))

  P_a <- f_a / (f_a + f_b)
  P_b <- 1 - P_a

  Big_T <- ifelse(il_sched_a == "VI" & il_sched_b == "VI",
                  P_a * (1 / r_a_com) + P_b * (1 / r_b_com),
                  P_a * (1 / r_a) + P_b * (1 / r_b)
  )

  # Check if no terminal links present
  # Big_T <- ifelse(
  #   apply(cbind(tl_dur_a1, tl_dur_a2, tl_dur_b1, tl_dur_b2), MARGIN = 1, FUN = sum) == 0,
  #   NA, Big_T
  # )

  # Average/classic delay reduction
  #-------------------------------------------------------------------------------
  dr_avg_a <- Big_T * tr_p_a - tl_dur_a
  dr_avg_b <- Big_T * tr_p_b - tl_dur_b

  # Establish if an alternative is signalled
  #-------------------------------------------------------------------------------
  sig_a <- sig_check(tl_dur_a1, tl_dur_a2, tr_p_a1, tr_p_a2)
  sig_b <- sig_check(tl_dur_b1, tl_dur_b2, tr_p_b1, tr_p_b2)

  # S+ Information
  #-------------------------------------------------------------------------------
  # Store tl signal duration
  sig_dur_a <- s_plus_dur(sig_a, tl_dur_a1, tl_dur_a2, tr_p_a1, tr_p_a2)
  sig_dur_b <- s_plus_dur(sig_b, tl_dur_b1, tl_dur_b2, tr_p_b1, tr_p_b2)

  # Store tl signal reinforcement probability
  sig_tr_p_a <- s_plus_tr_p(sig_a, tl_dur_a1, tl_dur_a2, tr_p_a1, tr_p_a2)
  sig_tr_p_b <- s_plus_tr_p(sig_b, tl_dur_b1, tl_dur_b2, tr_p_b1, tr_p_b2)

  # Bonus Delay Reduction
  #-------------------------------------------------------------------------------
  dr_bonus_a <- dr_bonus(sig_a, Big_T, sig_tr_p_a, sig_dur_a, tr_p_a, tl_dur_a)
  dr_bonus_b <- dr_bonus(sig_b, Big_T, sig_tr_p_b, sig_dur_b, tr_p_b, tl_dur_b)

  # Beta
  #-------------------------------------------------------------------------------
  beta_a <- beta_sig(
    beta_toggle, sig_a, il_sched_a, il_sched_b,
    il_dur_a, il_dur_b, sig_dur_a, beta_log
  )
  beta_b <- beta_sig(
    beta_toggle, sig_b, il_sched_b, il_sched_a,
    il_dur_b, il_dur_a, sig_dur_b, beta_log
  )

  # Total Conditional Reinforcement (i.e., total delay-reduction)
  #-------------------------------------------------------------------------------
  cr_a <- ifelse(!is.na(Big_T), dr_avg_a + dr_bonus_a * beta_a, NA)
  cr_b <- ifelse(!is.na(Big_T), dr_avg_b + dr_bonus_b * beta_b, NA)

  # SiGN Prediction
  #-------------------------------------------------------------------------------
  cp <- pred_SiGN(r_a, r_b, cr_a, cr_b)
  cp

  # Model Diagnostics
  diag <- data.frame(
    cp = cp,
    r_a = r_a,
    r_b = r_b,
    r_a_com,
    r_b_com,
    Big_T = Big_T,
    cr_a = cr_a,
    cr_b = cr_b,
    dr_avg_a = dr_avg_a,
    dr_avg_b = dr_avg_b,
    dr_bonus_a = dr_bonus_a,
    dr_bonus_b = dr_bonus_b,
    beta_a = beta_a,
    beta_b = beta_b,
    sig_a = sig_a,
    sig_b = sig_b,
    tr_p_a = tr_p_a,
    tr_p_b = tr_p_b,
    s_delta = s_delta,
    beta_log = beta_log
  )

  # Return structured output
  output <- list(
    cp = cp,
    details = diag
  )

  class(output) <- "SiGN"
  return(output)
}

#' @export
print.SiGN <- function(x, ...) {
  cat("Predicted Choice Proportion:\n")
  print(x$cp)

  cat("\nUse `$details` for additional model terms.\n")
  invisible(x)
}
