#' Summary Stats
#'
#' @param obj object returned by apply_model
#'
#' @returns list
#' @export
#'
#' @examples
#' summary_stats(obj3)
summary_stats <- function(obj){

  measure <- value <- n_indeps <- model <- auc <- time <- NULL

  #bootstrap longer
  obj$bootstrap_data_longer <- obj$bootstrap_data %>%
    tidyr::pivot_longer(cols = c(dplyr::starts_with(obj$perf_measure),
                                 dplyr::starts_with("time_")),
                        names_to = c("measure", "model"),
                        names_sep = "_",
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = measure,
                       values_from = value)

  #summary statistics
  obj$summary_stats <- obj$bootstrap_data_longer %>%
      dplyr::group_by(n_indeps, model) %>%
      dplyr::summarise(perf_m = mean(auc),
                perf_q025 = stats::quantile(auc, 0.025),
                perf_q975 = stats::quantile(auc, 0.975),
                time_m = mean(time),
                time_q025 = stats::quantile(time, 0.025),
                time_q975 = stats::quantile(time, 0.975)) %>%
      dplyr::ungroup()

  obj$bootstrap_data_longer <- NULL

  return(obj)

}
