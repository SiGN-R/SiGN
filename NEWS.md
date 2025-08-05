# SiGN (development version)

## 2025-08-05

* Added additional warning inside `choice_mod_eval()` for overfitting.

## 2025-08-02

* Two new vignettes (i.e., "articles") have been added to illustrate model evaluation with the package:
  * [Model Evaluation: Descriptive Statistics](../articles/eval_descriptive.html)
  * [Model Evaluation: Information-Theoretic Statistics](../articles/eval_info-theoretic.html)

## 2025-08-01

* Added the more general `choice_mod_eval()` to replace `SiGN_eval()`, allowing other pre-fit models to be compared directly against the SiGN model.

## 2025-07-31

* Added `SiGN_gen_eval()`, a function for applying free parameters to the SiGN model and returning model evaluation metrics.

## 2025-07-28

* Added function to compute (negative) log-Likelihood for a Beta error model
* Finished `SiGN_eval()` function to evaluate model fit with descriptive and likelihood-based metrics.

## 2025-07-26

* Added function for Lin's Concordance Correlation Coefficient.
* Added function SiGN eval (incomplete).

## 2025-07-22

* Updated `choice_params()` to only display recycling info message if three or more arguments differ in length.

## 2025-07-20

* Updated TL entry probabilities and choice proportions in `subopt_full` for Cunningham & Shahan 2019 (newly digitized values).
* Adding versioning to `subopt_full` and `subopt_avian` data sets.
* Improved "Signal Discrimination" notes in `choice_params()` documentation.
