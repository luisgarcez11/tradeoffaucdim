#' Summary Stats
#'
#' @param obj object returned by apply_model
#'
#' @returns list
#' @export
#'
#' @examples
#' summary_stats(obj)
summary_stats <- function(obj){

  #bootstrap longer
  obj$bootstrap_data_longer <- obj$bootstrap_data %>%
    tidyr::pivot_longer(cols = c(starts_with(obj$perf_measure),
                                 starts_with("time_")),
                        names_to = c("measure", "model"),
                        names_sep = "_",
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = measure,
                       values_from = value)

  #summary statistics
  obj$summary_stats <- obj$bootstrap_data_longer %>%
      group_by(n_indeps, model) %>%
      summarise(perf_m = mean(auc),
                perf_q025 = quantile(auc, 0.025),
                perf_q975 = quantile(auc, 0.975),
                time_m = mean(time),
                time_q025 = quantile(time, 0.025),
                time_q975 = quantile(time, 0.975)) %>%
      ungroup()

  return(obj)

}
