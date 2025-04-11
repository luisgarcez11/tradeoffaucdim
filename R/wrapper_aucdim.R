

#' Wrap all pipeline
#'
#' @param data a dataframe to be analysed
#' @param outcome a string representing the outcome variable
#' @param indep_vars a vector of strings to be considered
#' @param ... arguments used across pipeline
#'
#' @returns tibble
#' @export
wrapper_aucdim <- function(data, outcome, indep_vars,
                            ...
                           ) {

  p_in <- p_out <- models <- test_partition_prop <- perf_measure <-
    n_maximum_dim <- n_samples <- NULL

  obj <- bootstrap_data(data, outcome, indep_vars,
                        n_samples, n_maximum_dim) %>%
    define_indepvars(p_in = p_in,
                   p_out = p_out) %>%
    apply_model(models = models,
                test_partition_prop = test_partition_prop,
                perf_measure = perf_measure) %>%
    summary_stats() %>%
    plot_curve() %>%
    compare_test()

  return(obj)



}
