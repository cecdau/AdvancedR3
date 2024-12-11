
#' Funcion for summary statistics
#'
#' @param data
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(dplyr::across(
            value,
            base::list(
                average = mean,
                sd = sd
            )
        )) %>%
        dplyr::mutate(dplyr::across(
            dplyr::where(is.numeric),
            ~ base::round(.x, digits = 1)
        ))
}


#' A plot of data
#'
#' @param data
#'
#' @return A plot object

plot_distributions <- function(data) {
    ggplot2::ggplot(data, aes(x = value)) +
        ggplot2::geom_histogram() +
        ggplot2::facet_wrap(vars(metabolite), scales = "free")
}

