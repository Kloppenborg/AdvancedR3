#' Generate descriptive statistics for table 1
#'
#' @param data The lipidomics dataset
#'
#' @return A dataframe/table
#'
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}

#' Plot for basic distribution of metabolites
#'
#' @param data The lipidomics dataset
#'
#' @return A Histogram with the GGplot2 package
#'
plot_distributions <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}


#' Convert a column´s charater values to snakecase format
#'
#' @param data The lipidomics dataset
#' @param columns The columns you want to convert into snakecase format
#'
#' @return a tibble/Data Frame
column_values_to_snake_case <-
  function(data, columns) {
    data %>%
      dplyr::mutate(
        dplyr::across({{ columns }}, snakecase::to_snake_case)
      )
  }


#' Function to pivot lipidomics data from long into wide, using snakecase formated column names
#'
#' @param data Lipidomics
#'
#' @return A wide data frame with 1 obs for each participants

metabolites_to_wider <- function(data) {
  data %>%
    tidyr::pivot_wider(
      names_from = metabolite,
      values_from = value,
      values_fn = mean,
      names_prefix = "metabolite_"
    )
}
