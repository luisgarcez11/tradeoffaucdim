#' Comapare test
#'
#' @param obj object returned by plot_curve
#'
#' @returns list
#' @export
#'
#' @examples
#' compare_test(obj5)
compare_test <- function(obj){

  x <- y <- p_value <- auc <- id <- model <- time <-
    n_indeps <- t_test <- diff_auc <- NULL

  #test results
  if(length(obj$models)== 2){
    obj$test <- list()

    #compute p_values
    obj$test$perf_ttest <- tibble::tibble(
      n_indeps = 1:obj$n_maximum_dim) %>%
      dplyr::mutate(t_test = purrr::map(.x = n_indeps,
                                 .f = function(.x){
                                   stats::t.test(x = obj$bootstrap_data$diff_auc[
                                   obj$bootstrap_data$n_indeps == .x
                                   ],
                                   mu = 0
                                 )}
                                 )) %>%
      dplyr::mutate(p_value = purrr::map_dbl(.x = t_test,
                                       .f = function(.x) {
                                         .x$p.value})) %>%
      dplyr::mutate(y = 10) %>%
      dplyr::mutate(weight = 1)

    #add global p_values
    # Convert p-values to Z-scores using inverse normal CDF
    z_scores <- stats::qnorm(1 - obj$test$perf_ttest$p_value)
    weights <- obj$test$perf_ttest$weight

    # Compute weighted Z-score
    weighted_z <- sum(weights * z_scores) / sqrt(sum(weights^2))

    # Compute combined p-value
    obj$test$perf_ttest <- obj$test$perf_ttest %>%
      dplyr::mutate(global_p_value = 1 - stats::pnorm(weighted_z))

    #add plot with differences
    obj$test$plot_auc_diff <- ggplot2::ggplot(
      data = obj$bootstrap_data[,c("diff_auc", "n_indeps")] ,
      mapping = ggplot2::aes(x = diff_auc)
    )+
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(~n_indeps, scales = "free") +
      ggplot2::theme_bw()+
      ggplot2::geom_label(data = obj$test$perf_ttest %>%
                   dplyr::mutate(x = 0),
                ggplot2::aes(x, y,
                    label = round(p_value, 5)), size = 3,
                position = "identity") +
      ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = 0), colour = "red",
                 linetype = 2) +
      ggplot2::xlab("AUC Differences") +
      ggplot2::ylab("Count")


    #compute p_values for time differences
    obj$test$time_ttest <- tibble::tibble(
      n_indeps = 1:obj$n_maximum_dim) %>%
      dplyr::mutate(t_test = purrr::map(.x = n_indeps,
                                 .f = function(.x){
                                   stats::t.test(obj$bootstrap_data$diff_time[
                                     obj$bootstrap_data$n_indeps == .x
                                   ], mu = 0)}
      )) %>%
      dplyr::mutate(p_value = purrr::map_dbl(.x = t_test,
                                      .f = function(.x) {
                                        .x$p.value})) %>%
      dplyr::mutate(x = max(obj$bootstrap_data$diff_time),
             y = 10) %>%
      dplyr::mutate(weight = 1)

    #add global p_values
    # Convert p-values to Z-scores using inverse normal CDF
    z_scores <- stats::qnorm(1 - obj$test$time_ttest$p_value)
    weights <- obj$test$time_ttest$weight

    # Compute weighted Z-score
    weighted_z <- sum(weights * z_scores) / sqrt(sum(weights^2))

    # Compute combined p-value
    obj$test$time_ttest <- obj$test$time_ttest %>%
      dplyr::mutate(global_p_value = 1 - stats::pnorm(weighted_z))


  return(obj)}

  if(length(obj$models)> 2){

    #prepare data for ANOVA
    data = obj$bootstrap_data %>%
      tidyr::pivot_longer(cols = c(dplyr::starts_with(obj$perf_measure)),
                          names_to = c("measure", "model"),
                          names_sep = "_",
                          values_to = obj$perf_measure)

    #repeated measures ANOVA
    obj$test$perf_anova <- ez::ezANOVA(data =  data,
                dv = auc,
                wid = id,
                within = model
                )

    #prepare data for ANOVA
    data = obj$bootstrap_data %>%
      tidyr::pivot_longer(cols = c(dplyr::starts_with("time")),
                          names_to = c("measure", "model"),
                          names_sep = "_",
                          values_to = "time")

    #repeated measures ANOVA for time
    obj$test$time_anova <- ez::ezANOVA(data =  data,
                                       dv = time,
                                       wid = id,
                                       within = model
    )

  }

  if(length(obj$models) == 1){
    return(obj)
  }



}
