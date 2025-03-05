#' NSES concepts to filter Census variables by when importing data
#'
#' This is a list of regular expressions that can be used to filter Census variables to obtain
#' data required to comput the NSES index.
#'
#' @name nses_concepts
#' @docType variable
#' @usage nses_concepts
#' @export
nses_concepts <- c(
    "Educational attainment" = "^Educational Attainment For The Population 25 Years And Over$",
    "Unemployment" = "^Employment Status For The Population 16 Years And Over$",
    "Median income" = "^Median Household Income In The Past 12 Months \\(In 20[0-9][0-9] Inflation-Adjusted Dollars\\)$",
    "Poverty" = "^Poverty Status Of Individuals In The Past 12 Months By Living Arrangement$",
    "Median housing value" = "^Median Value \\(Dollars\\)$",
    "Occupant density" = "^Tenure By Occupants Per Room$"
)
