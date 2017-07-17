###################################################################################################
# checks.R
#
# Oisin Fitzgerald
# initialised: 01/06/17
#
# Contains:
###################################################################################################


#' check_type
#'
#' @inheritParams classify
#' @param type_to_check the data type to check (numeric, character, date)
#' @param dictionary A data frame used by the function classify.
#' Each row of the data frame dictionary specifies a data type for a variable
#' found in data and rules. There are two columns in dictionary, variable and type.
#' The column names in rules are matched in dictionary and type is one of numeric,
#' character, or date for each variable.
#'
check_type <- function(type_to_check, data, dictionary) {
  if (any(dictionary$type == type_to_check)) {
    vars <- dictionary$variable[dictionary$type == type_to_check]
    type_check <- sapply(data[vars],
      function(x) do.call(paste0("is.", type_to_check), args =  list(x)))
    wrong_types <- names(data[vars][!type_check])
    if (!all(type_check)) {
      stop(paste0("data not of type ", type_to_check, ": ",
        paste0(wrong_types, collapse = ", ")))
    }
  }
}


#' check_dictionary
#'
#' @inheritParams classify
#' @param dictionary A data frame used by the function classify.
#' Each row of the data frame dictionary specifies a data type for a variable
#' found in data and rules. There are two columns in dictionary, variable and type.
#' The column names in rules are matched in dictionary and type is one of numeric,
#' character, or date for each variable.
check_dictionary <- function(dictionary, data) {

  ## check correct column names supplied
  if (all(!names(dictionary) %in% c("variable", "type"))) {
    stop("data dictionary should contain columns variable and type")
  }

  ## check variable and type are character vectors
  stopifnot(is.character(dictionary$variable))
  stopifnot(is.character(dictionary$type))

  ## check data types are in valid set
  valid_types <- c("numeric", "character", "date")
  # logical vector indicating position of incorrect types
  type_check <- dictionary$type %in% valid_types
  wrong_types <- dictionary$type[!type_check]
  #
  if (!identical(wrong_types, character(0))) {
    stop(paste0("incorrect data types: ", wrong_types))
  }

  ## check character variables
  check_type("character", data, dictionary)

  ## check numeric
  check_type("numeric", data, dictionary)

  ## check dates?

}

#' check_rules
#'
#' @inheritParams classify
#' @param rules rules #!.
#'
check_rules <- function(data, rules) {

  ## check correct column names supplied
  names_check <- names(rules)[-1] %in% names(data)
  wrong_names <- names(rules)[-1]

  if (all(!names(rules)[-1] %in% names(data))) {
    stop(paste0("variables mentioned in rules should correspond to the data - ", wrong_names))
  }
}

## End --------------------------------------------------------------------------------------------
