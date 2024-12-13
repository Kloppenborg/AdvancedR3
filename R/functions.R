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

#' A transformtion recipe to preprocess the data
#'
#' @param data the lipidomics_wide
#' @param metabolite_variable  The column of metabolite variable
#'
#' @return A recipe object

create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }},
      age,
      gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class,
      new_role = "outcome"
    ) %>%
    recipes::step_normalize(
      tidyselect::starts_with("metabolite_")
    )
}

#' Create a workflow object ofthe model and transformation
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object

create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}


#' Ceate a tidy output of the model results
#'
#' @param workflow_fitted_model The model workflow object that has been fitted
#'
#' @return A data frame

tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}

#' Convert long format df into af list of wid form df
#'
#' @param data The lipidomics dataset
#'
#' @return A list of data frames
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}

#' Generate the results of a model
#'
#' @param data The lipidomics
#'
#' @return A data frame
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() %>%
      parsnip::set_engine("glm"),
    data %>%
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Calculate theestimates(coeficient)for the model for each metabolite
#'
#' @param data The lipidomics dataset
#'
#' @return A data frame
calculate_estimates <- function(data) {
  model_estimates <- data %>%
    split_by_metabolite() %>%
    purrr::map(generate_model_results) %>%
    purrr::list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "metabolite_"))
  data %>%
    dplyr::select(metabolite) %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snake_case(term) %>%
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
    dplyr::distinct(metabolite, term) %>%
    dplyr::right_join(model_estimates, by = "term") %>%
    dplyr::select(-term)
}


#' Plot a pointrange plot with results
#'
#' @param results model_estimates
#'
#' @return A ggplot object
plot_estimates <- function(results) {
  results %>%
  ggplot2::ggplot(ggplot2::aes(
    x = estimate,
    y = metabolite,
    xmin = estimate - std.error,
    xmax = estimate + std.error
  )) +
  ggplot2::geom_pointrange() +
  ggplot2::coord_fixed(xlim = c(0, 5))
}
