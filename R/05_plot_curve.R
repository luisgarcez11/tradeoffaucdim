#' Plot curve
#'
#' @param obj object returned by 04_summary_statistics
#'
#' @returns list
#' @export
#'
#' @examples
#' plot_curve(obj)
plot_curve <- function(obj){

    #plot performance
    obj$plot_performance <- ggplot2::ggplot(data = obj$summary_stats )+
      geom_line(aes( x = n_indeps, y = perf_m, colour = model ))+
      geom_errorbar(aes( x = n_indeps, y = perf_m,
                         ymin = perf_q025, ymax= perf_q975,
                        colour = model),
                    width=.2, linetype = 1) +
      xlab("Dimensionality") +
      ylab(toupper(obj$perf_measure[1])) +
      labs(caption = paste0("Variable order: ",paste0(obj$ordered_indep_vars,
                                           collapse = ", ")))+
      theme_bw()

    #add plot histogram
    obj$plot_performance_hist <- ggplot2::ggplot(
      data = obj$bootstrap_data %>%
        dplyr::select(starts_with(obj$perf_measure), "n_indeps") %>%
        tidyr::pivot_longer(cols = starts_with(obj$perf_measure[1]),
                            names_sep = "_",
                            names_to = c("measure", "model"),
                            values_to = obj$perf_measure[1]) ,
      mapping = aes(x = auc, fill = model)
    )+
      geom_histogram() +
      facet_wrap(~n_indeps, scales = "free") +
      theme_bw()+
      xlab(toupper(obj$perf_measure[1])) +
      ylab("Count")

    #plot time
    obj$plot_time <- ggplot2::ggplot(data = obj$summary_stats )+
      geom_line(aes( x = n_indeps, y = time_m, colour = model ))+
      geom_errorbar(aes( x = n_indeps, y = time_m,
                         ymin = time_q025, ymax= time_q975,
                         colour = model),
                    width=.2, linetype = 1) +
      xlab("Dimensionality") +
      ylab("Time (in seconds)") +
      labs(caption = paste0("Variable order: ",
                            paste0(obj$ordered_indep_vars, collapse = ", ")))+
      theme_bw()

    #plot time histogram
    obj$plot_time_hist <- ggplot2::ggplot(
      data = obj$bootstrap_data %>%
        dplyr::select(starts_with("time"), "n_indeps") %>%
        tidyr::pivot_longer(cols = starts_with("time"),
                            names_to = c("measure", "model"),
                            names_sep = "_",
                            values_to = "value") ,
      mapping = aes(x = value, fill = model)
    )+
      geom_histogram() +
      facet_wrap(~n_indeps, scales = "free") +
      theme_bw()+
      xlab("Time elapsed (in seconds)") +
      ylab("Count")




  return(obj)


}
