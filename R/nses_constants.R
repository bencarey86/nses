#' NSES concepts to filter Census variables by when importing data
#'
#' This is a list of regular expressions that can be used to filter Census variables to obtain
#' data required to comput the NSES index.
#'
#' @name nses_concepts
#' @export
nses_concepts <- c(
    "Educational attainment" = "^Educational Attainment For The Population 25 Years And Over$",
    "Unemployment" = "^Employment Status For The Population 16 Years And Over$",
    "Median income" = "^Median Household Income In The Past 12 Months \\(In 20[0-9][0-9] Inflation-Adjusted Dollars\\)$",
    "Poverty" = "^Poverty Status Of Individuals In The Past 12 Months By Living Arrangement$",
    "Median housing value" = "^Median Value \\(Dollars\\)$",
    "Occupant density" = "^Tenure By Occupants Per Room$"
)

#' NSES states
#'
#' This is a data frame containing information about the all states in the United States
#' and Washington, D.C. It is used to filter out territories and other non-state entities.
#'
#' @name nses_states
#' @export
nses_states <- read.delim("https://www2.census.gov/geo/docs/reference/state.txt", sep = "|") |>
    janitor::clean_names() |>
    dplyr::select(
        "code" = "state",
        "abbreviation" = "stusab",
        "name" = "state_name"
    ) |>
    dplyr::mutate(
        "code" = stringr::str_pad(code, 2, pad = "0")
    ) |>
    dplyr::filter(!code %in% c("60", "66", "69", "72", "74", "78"))
