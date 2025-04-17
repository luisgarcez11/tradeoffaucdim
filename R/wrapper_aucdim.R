

#' Wrap all pipeline
#'
#' @param data a dataframe to be analyzed
#' @param outcome a string representing the outcome variable
#' @param indep_vars a vector of strings to be considered
#' @param ... arguments used across pipeline
#'
#' @returns a list with the final object
#' @export
wrapper_aucdim <- function(data, outcome, indep_vars,
                           n_samples = 100, n_maximum_dim = 5,
                           p_in = 0.50, p_out = 0.60,
                           models = c("SL.glm"),
                           test_partition_prop = 0.2,
                           perf_measure = "auc",
                           x_label_offset = 1, y_label_offset = 10

                           ) {

  obj <- bootstrap_data(data, outcome, indep_vars,
                        n_samples, n_maximum_dim) %>%
    define_indepvars(p_in = p_in,
                   p_out = p_out) %>%
    apply_model(models = models,
                test_partition_prop = test_partition_prop,
                perf_measure = perf_measure) %>%
    summary_stats() %>%
    plot_curve() %>%
    compare_test(x_label_offset , y_label_offset)

  return(obj)



}
