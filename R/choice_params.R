#' Generate SiGN Model Parameter List
#'
#' A helper function that constructs a named list of parameters for behavioural
#' choice predictions for the function [SiGN()]. Users can either select a
#' predefined profile or specify custom values. The function includes built-in
#' validation and recycling of arguments when lengths differ.
#'
#' @usage
#' choice_params(
#'  profile = c("zentall", "kendall", "fantino"),
#'  il_dur_a = NULL, il_dur_b = NULL,
#'  tl_dur_a1 = NULL, tl_dur_a2 = NULL,
#'  tl_dur_b1 = NULL, tl_dur_b2 = NULL,
#'  tl_p_a1 = NULL, tl_p_a2 = NULL,
#'  tl_p_b1 = NULL, tl_p_b2 = NULL,
#'  tr_p_a1 = NULL, tr_p_a2 = NULL,
#'  tr_p_b1 = NULL, tr_p_b2 = NULL,
#'  il_sched_a = NULL, il_sched_b = NULL,
#'  s_delta = 1,
#'  beta_log = 10,
#'  beta_toggle = TRUE,
#'  display_params = FALSE
#'  )
#'
#' @param profile A character string specifying a default parameter profile. Must be one of
#'   `"zentall"`, `"kendall"`, or `"fantino"`. See details.
#' @param il_dur_a,il_dur_b Numeric vector of initial link durations for
#' alternatives A and B respectively.
#' @param tl_dur_a1,tl_dur_a2 Numeric vector of durations for two
#' terminal links on alternative A.
#' @param tl_dur_b1,tl_dur_b2 Numeric vector of durations for two
#' terminal links on alternative B.
#' @param tl_p_a1,tl_p_a2 Numeric vector of entry probabilities for two
#' terminal links on alternative A.
#' @param tl_p_b1,tl_p_b2 Numeric vector of entry probabilities for two
#' terminal links on alternative B.
#' @param tr_p_a1,tr_p_a2 Numeric vector of terminal reinforcement probabilities for two
#' terminal links on alternative A.
#' @param tr_p_b1,tr_p_b2 Numeric vector of terminal reinforcement probabilities for two
#' terminal links on alternative B.
#' @param il_sched_a,il_sched_b  Schedule type for each initial link. Must be
#' either `"VI"` (variable-interval) or `"FR"` (fixed-ratio). Case-insensitive.
#' @param s_delta Time required to perceive a stimulus that signals the
#' absence of terminal reinforcement. Default is 1. See also [s_delta()].
#' @param beta_log A positive real number specifying the base of
#' the logarithm used for β. Defaults to 10.
#' @param beta_toggle Logical defaulting to `TRUE`. Permits the model to adjust
#' the balance of conditional and primary reinforcement for signalled alternatives according to Equation 6 of Dunn et al. (2024). See also [beta_sig()].
#' @param display_params Logical. If `TRUE`, prints the parameter list as a
#' data frame.
#'
#' @details
#' *Default Profiles:*
#'
#' Three default profiles are available to streamline argument selection:
#' `"zentall"` (based on Stagner & Zentall, 2010), `"kendall"` (based on
#' Kendall, 1985), and `"fantino"` (based on Fantino, 1969). Notably, the "
#' fantino" profile reflects a single terminal link procedure, consistent with
#' the design used in Fantino’s original study.
#'
#' Default profile values can be overridden by specifying the corresponding
#' arguments directly.
#'
#' If argument lengths differ, values are recycled to the maximum length,
#' with a message.
#'
#' *Temporal Units:*
#'
#' The SiGN model is temporally relative, meaning it allows initial and
#' terminal link durations to be treated as any unit of time (e.g., seconds,
#' minutes, hours, etc.). However, units need to be
#' consistent across all link durations.
#'
#' *s_delta*
#'
#' Conventionally, concurrent-chains paradigms have measured time in seconds
#' with very easily discriminable stimuli. The default value of `s_delta`
#' reflects this convention. However, if shorter or longer temporal units are
#' used, or the stimuli takes considerable time to be discriminated, it may be
#' prudent to adjust `s_delta` accordingly.
#'
#' *Signal Discrimination:*
#'
#'  A choice alternative is treated by the [SiGN()] function as signalled if
#'  both terminal link durations are non-zero and either the durations or
#'  the reinforcement probabilities differ between the terminal links.
#'
#'  This definition assumes that differences in duration or reinforcement
#'  probability imply discriminability between the stimuli associated with
#'  each terminal link. For example, if one terminal link lasts 10 seconds
#'  and the other 20 seconds, and both have a reinforcement probability of 1,
#'  it is assumed that the organism can fully discriminate between them—e.g.,
#'  the terminal links may be visually distinct (such as different colours)
#'  and thus are recognisable from the moment of onset.
#'
#'  The SiGN model assumes that the (behavioural) function of terminal link
#'  stimuli remains fixed across the entire duration of the terminal link—that
#'  is, the probability of terminal reinforcement does not change
#'  partway through the terminal link. Procedures that depart from this
#'  assumption may not be appropriately handled by the model.
#'
#' *Schedule Selection:*
#'
#' Setting the initial link schedules as `"FR"` is most suitable for cases
#' involving an FR 1 schedule or when a single timer is employed in the
#' initial links of long VI schedules. This is because the `"FR"` setting does
#' not take into account the switching behaviour present with concurrent
#' schedules using independent timers. For example, if a single timer is used
#' for two concurrent VI 30 schedules, setting the Initial Link Schedule as
#' an FR is preferable because the time spent in the initial links is
#' controlled by one timer, not two independent timers operating concurrently.
#' However, if independent timers are used for each initial link, the model
#' requires this setting to be VI .
#'
#' Note that the SiGN model does not directly compute predictions for ratio
#' schedules. Instead, it represents the ratio as a duration with an individual
#' reinforcement rate, rather than a common/shared one.
#'
#' @returns A named list of validated and possibly recycled parameters suitable
#' for input into the `SiGN()` function.
#'
#' @references
#' Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).
#' Suboptimal choice: A review and quantification of the signal for good news
#' (SiGN) model. *Psychological Review*. *131*(1), 58-78.
#' \url{https://doi.org/10.1037/rev0000416}
#'
#' Fantino, E. (1969). Choice and rate of reinforcement. *Journal of the
#' Experimental Analysis of Behavior*, *12*(5), 723–730.
#' \url{https://doi.org/10.1901/jeab.1969.12-723}
#'
#' Kendall, S. B. (1985). A further study of choice and percentage
#' reinforcement. *Behavioural Processes*, *10*(4), 399–413.
#' \url{https://doi.org/10.1016/0376-6357(85)90040-3}
#'
#' Stagner, J. P., & Zentall, T. R. (2010). Suboptimal choice behavior by
#' pigeons. *Psychonomic Bulletin & Review*, *17*(3), 412–416.
#' \url{https://doi.org/10.3758/PBR.17.3.412}
#'
#' @seealso [dur_entry_conflict()], [entry_p_sums()], [valid_sched_input()], [recycle_list()], [eq_arg_n()]]
#'
#' @examples
#' # Use default Zentall profile
#' z <- choice_params("zentall", display_params = TRUE)
#'
#' # Custom specification with minimal values
#' k <- choice_params(
#'   profile = "kendall",
#'   tr_p_b1 = c(1, 0.5), tr_p_b2 = c(1, 0.5),
#'   display_params = TRUE
#' )
#' @export
#-------------------------------------------------------------------------------
choice_params <- function(
    profile = c("zentall", "kendall", "fantino"),
    # Initial link (il) duration (dur)
    il_dur_a = NULL, il_dur_b = NULL,
    # Terminal link (tl) duration (dur)
    tl_dur_a1 = NULL, tl_dur_a2 = NULL,
    tl_dur_b1 = NULL, tl_dur_b2 = NULL,
    # Terminal link (tl) entry probability (p)
    tl_p_a1 = NULL, tl_p_a2 = NULL,
    tl_p_b1 = NULL, tl_p_b2 = NULL,
    # Terminal reinforcement (tr) probability (p)
    tr_p_a1 = NULL, tr_p_a2 = NULL,
    tr_p_b1 = NULL, tr_p_b2 = NULL,
    # Schedules
    il_sched_a = NULL,
    il_sched_b = NULL,
    # S delta (pure tl signal for no reinforcement) duration
    s_delta = 1,
    # Base of beta's logarithm
    beta_log = 10,
    # Option to turn of beta
    beta_toggle = TRUE,
    # Print data frame of parameters
    display_params = FALSE
    ) {
  profile <- tolower(profile)
  profile <- match.arg(profile)

  defaults <- switch(profile,
    zentall = list(
      il_dur_a = 1, il_dur_b = 1,
      tl_dur_a1 = 10, tl_dur_a2 = 10,
      tl_dur_b1 = 10, tl_dur_b2 = 10,
      tl_p_a1 = 0.2, tl_p_a2 = 0.8,
      tl_p_b1 = 0.2, tl_p_b2 = 0.8,
      tr_p_a1 = 1, tr_p_a2 = 0,
      tr_p_b1 = 0.5, tr_p_b2 = 0.5,
      il_sched_a = "FR",
      il_sched_b = "FR",
      s_delta = 1,
      beta_log = 10,
      beta_toggle = TRUE#,
      #display_params = FALSE
    ),
    kendall = list(
      il_dur_a = 1, il_dur_b = 1,
      tl_dur_a1 = 15, tl_dur_a2 = 15,
      tl_dur_b1 = 15, tl_dur_b2 = 15,
      tl_p_a1 = 0.5, tl_p_a2 = 0.5,
      tl_p_b1 = 0.5, tl_p_b2 = 0.5,
      tr_p_a1 = 1, tr_p_a2 = 0,
      tr_p_b1 = 1, tr_p_b2 = 1,
      il_sched_a = "FR",
      il_sched_b = "FR",
      s_delta = 1,
      beta_log = 10,
      beta_toggle = TRUE#,
      #display_params = FALSE
    ),
    fantino = list(
      il_dur_a = 90, il_dur_b = 30,
      tl_dur_a1 = 30, tl_dur_a2 = 0,
      tl_dur_b1 = 90, tl_dur_b2 = 0,
      tl_p_a1 = 1, tl_p_a2 = 0,
      tl_p_b1 = 1, tl_p_b2 = 0,
      tr_p_a1 = 1, tr_p_a2 = 0,
      tr_p_b1 = 1, tr_p_b2 = 0,
      il_sched_a = "VI",
      il_sched_b = "VI",
      s_delta = 1,
      beta_log = 10,
      beta_toggle = TRUE#,
      #display_params = FALSE
    )
  )

  # Argument names
  arg_names <- names(defaults)

  # Currently supplied values
  supplied <- mget(arg_names, inherits = FALSE)

  # Apply defaults where needed
  arg_list <- utils::modifyList(defaults, Filter(Negate(is.null), supplied))

  # Misc.
  arg_list$il_sched_a <- toupper(arg_list$il_sched_a)
  arg_list$il_sched_b <- toupper(arg_list$il_sched_b)
  arg_list$s_delta <- s_delta
  arg_list$beta_log <- beta_log
  arg_list$beta_toggle <- beta_toggle

  # Validation
  #--------------------------------------------------------------------------
  errors <- character(0)

  # Equal number of arguments
  eq_arg <- eq_arg_n(arg_list[1:16])

  if (eq_arg == FALSE) {

    # Add warning if 3 or more lengths are different
    if (length(unique(lengths(arg_list[1:16]))) > 2) {
      message("\u2139 Arguments differ in length; values will be recycled. Check that this is what you intended.")
    }

    arg_list <- recycle_list(arg_list)
  }

  # Numeric and non-negatives
  numeric_args <- names(arg_list[c(1:14, 17:18)])

  for (arg in numeric_args) {
    value <- arg_list[[arg]]

    if (!is.numeric(value)) {
      errors <- c(errors, sprintf("Parameter '%s' must be numeric.", arg))
      next
    }

    if (any(value < 0, na.rm = TRUE)) {
      errors <- c(
        errors, sprintf("Parameter '%s' contains negative values.", arg)
      )
      next
    }
  }

  # Greater than 0
  above_0_args <- names(arg_list[c(1:2, 18)])

  for (arg in above_0_args) {
    value <- arg_list[[arg]]

    if (any(value <= 0, na.rm = TRUE)) {
      errors <- c(errors, sprintf("Parameter '%s' must contain values greater than 0.", arg))
      next
    }
  }

  # Check entry probability sums (adjust for difference)
  if (any(entry_p_sums(arg_list[7:10]) != 2)) {
    errors <- c(errors, "Terminal link entry probabilites should sum to 1 for each choice alternative.\nE.g., `tl_p_a1 + tl_p_a2 == 1`")
  }

  # Entry probability conflict with duration
  if (dur_entry_conflict(arg_list[3:6], arg_list[7:10])) {
    errors <- c(errors, "A terminal link duration has been set with an entry probability of 0.\nPlease provide a probability > 0 or set the duration to 0.")
  }

  # Terminal Reinforcement Probabilities
  tr_p_args <- names(arg_list[11:14])

  for (arg in tr_p_args) {
    value <- arg_list[[arg]]

    if (any(value > 1, na.rm = TRUE)) {
      errors <- c(errors, sprintf("Parameter '%s' contains probabilities greater than 1.", arg))
    }
  }

  # Schedule Values
  if (valid_sched_input(arg_list[15:16]) == FALSE) {
    errors <- c(errors, "Only 'FR' 'FR' or 'VI' 'VI' schedules are accepted.")
  }

  # No NA or Null
  all_args <- names(arg_list)

  for (arg in all_args) {
    value <- arg_list[[arg]]

    if (is.null(value)) {
      errors <- c(errors, sprintf("Parameter '%s' is missing.", arg))
      next
    }

    if (any(is.na(value))) {
      errors <- c(errors, sprintf("Parameter '%s' contains NA values.", arg))
    }
  }

  # Output errors
  if (length(errors) > 0) {
    stop(
      paste(
        "Parameter validation failed:\n\n",
        paste(errors, collapse = "\n\n")
      ),
      call. = FALSE
    )
  }

  # Display values
  if (display_params == TRUE) {
    print(as.data.frame(arg_list))
  }

  return(arg_list)
}
