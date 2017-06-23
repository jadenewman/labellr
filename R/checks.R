###################################################################################################
# checks.R
#
# Oisin Fitzgerald
# initialised: 01/06/17
#
#
# Contains:
###################################################################################################

#' check_dictionary
#'
#' @param dictionary a data.frame with columns variable and type
#' @return nada
#' @examples
check_dictionary <- function(dictionary) {

  ## check correct column names supplied
  if (all(!names(dictionary) %in% c("variable", "type"))) {
    stop("data dictionary should contain columns variable and type")
  }

  ## check variable and type are character vectors
  stopifnot(is.character(dictionary$variable))
  stopifnot(is.character(dictionary$type))

  ## check data types are in valid set
  valid_types <- c("num", "char", "date")
  # logical vector indicating position of incorrect types
  type_check <- dictionary$type %in% valid_types
  wrong_types <- dictionary$type[!type_check]
  #
  if (!identical(wrong_types, character(0))) {
    stop(paste0("incorrect data types: ", wrong_types))
  }
}

#' check_definitions
#'
#' @param definitions a data.frame
#' @return nada
#' @examples
check_definitions <- function(data, rules) {

  ## check correct column names supplied
  names_check <- names(rules)[-1] %in% names(data)
  wrong_names <- names(rules)[-1]

  if (all(!names(rules)[-1] %in% names(data))) {
    stop(paste0("variables mentioned in rules should correspond to the data - ", wrong_names))
  }
}

## End --------------------------------------------------------------------------------------------
