###################################################################################################
# classify.R
#
# Oisin Fitzgerald
# initialised: 01/06/17
#
#
# Contains: num_classify, char_classify, date_classify, classify
###################################################################################################

# num_classify ------------------------------------------------------------------------------------

#' num_classify
#'
#' @param data a numeric vector of data to be tested against a rule.
#' @param rule a character vector of length one that specifies the rule to test. The rule can either
#' indicate a value must be present ("1") or absent ("0") or test the entered value using one of the
#' set of comparison functions ("==", ">=", "<=", ">", "<"). If a comparison function is used any NA
#' value in the data is assumed to have failed the test. If an empty string is entered for the rule,
#' any value is assumed correct.
#' @return logical vector indicating which elements of the data match the rule.
num_classify <- function(data, rule) {

  ## test for valid input
  stopifnot(is.vector(data))
  stopifnot(is.numeric(data))
  # make a character and remove whitespace
  rule <- as.character(rule)
  # account for empty character
  if (identical(rule, character(0)) | is.na(rule)) {
    rule <- ""
  }

  ## check for present/absent rule or functional rule
  if (rule %in% c("1", "0", "")) {
    ## present/absent rule checks
    if (rule == "") {
      tmp_res <- rep(TRUE, length(data))
    } else if (rule == "0") {
        tmp_res <- is.na(data)
      } else if (rule == "1") {
          tmp_res <- !is.na(data)
      }
  } else {
    ## functional rule
    ## allowable functions
    allowable_funcs <- c("==", ">=", "<=", ">", "<")

    ## extracting arguments for test
    val <-  unlist(strsplit(c(rule), split = "[>=]"))
    # remove empty strings
    val <- val[nchar(val) > 0]
    val <- as.numeric(val)
    vals <- rep(val, length(data))

    ## extract function for test
    func <- unlist(strsplit(c(rule), split = "[^>=]"))
    # remove empty strings
    func <- func[nchar(func) > 0]
    func <- func[!is.na(func)]

    # test for valid function
    stopifnot(func %in% allowable_funcs)

    # results of the comparisons/tests
    tmp_res <- do.call(func, args = list(c(data), c(vals)))
  }
  ## make NA -> FALSE
  tmp_res <- as.logical(tmp_res)
  tmp_res[is.na(tmp_res)] <- FALSE
  ## return
  tmp_res
}

# char_classify ------------------------------------------------------------------------------------

#' char_classify
#'
#' @param data a character vector of data to be tested against a rule.
#' @param rule a character vector of length one that specifies the rule to test.
#' @return logical vector indicating which elements of the data match the rule.
char_classify <- function(data, rule) {
  ## test for valid input
  stopifnot(is.vector(data))
  if (!is.character(data) & !all(is.na(data))) {
    stop("character indicated in data dictionary") #!
  }

  ## test if non-present rule
  if(grepl("!", rule)) {
    ## extract values string should not equal
    neg_vals <- rule[grep("!", rule)]
    neg_vals <- unlist(strsplit(neg_vals, "!"))
    neg_vals <- neg_vals[neg_vals != ""]
    pos_vals <- rule[-grep("!", rule)]
      ## if pos_vals empty let it equal empty string
    if (identical(pos_vals, character(0))) {
      pos_vals <- ""
    }
    ## test for non-presence of values
    neg_res <- !(data %in% neg_vals)
  } else {
    ## no negative results => all true
    neg_res <- rep(TRUE, length(data))
    pos_vals <- rule
  }

  ## don't test if any value allowed
  if(pos_vals == "" | is.na(rule)) {   #!
    # any value allow => all TRUE
    pos_res <- rep(TRUE, length(data))
  } else {
    ## test against data
    pos_res <- data %in% pos_vals
  }
  ## return logical
  as.logical(pos_res * neg_res)
}

# date_classify -----------------------------------------------------------------------------------

#' date_classify
#'
#' @param data a date vector of data to be tested against a rule.
#' @param rule a character vector of length one that specifies the rule to test. For date data only
#' present (1) or absent (0) rules are testable. If an empty string is entered for the rule, any
#' value is assumed correct.
#' @return logical vector indicating which elements of the data match the rule.
date_classify <- function(data, rule) {
  ## test for valid input
  stopifnot(is.vector(data))
  #stopifnot(is.character(rule))

  ## make present/absent (1/0) -> numeric
  rule <- as.numeric(rule)

  if(is.na(rule[1])) {
    # any value allow => all TRUE
    tmp_res <- rep(TRUE, length(data))
  } else {
    ## test for presence/absence of date
    if(rule[1] == 1) {
      tmp_res <- !is.na(data)
    } else {
      if (rule[1] == 0) {
        tmp_res <- is.na(data)
      }
    }
  }
  ## return logical
  as.logical(tmp_res)
}


# classify ----------------------------------------------------------------------------------------

#' @title Categorise your data
#'
#' @description classify performs categorises rows of a data frame according to
#' a set of rules. These rules specify allowable values for each particular category level.
#'
#' @param data data.frame whose rows are to be categorised according to a provided set of rules.
#' @param dictionary Each row of the data frame dictionary specifies a data type for a variable
#' found in data and rules. There should be two columns in dictionary, variable and type.
#' The column names in rules should be exactly matched in dictionary and type should be one of num,
#' char, or date for each variable.
#' @param rules The first column of this data frame should be the name of the (to be created)
#' classification variable and subsequent columns should be the names of the variables which
#' are to be tested against a rule.
#' Each row of the rules data.frame should contain a classification level in the first column and
#' from there the rules to test each column against.
#' @param default_def the default value to be given to all unclassified rows.
#' @details
#' Rulesets for variable type \strong{num}:
#' \itemize{
#'    \item present/absent: blah blah
#'    \item comparison to a constant: blah blah
#' }
#' Rulesets for variable type \strong{char}:
#' \itemize{
#'    \item present/absent: blah blah
#'    \item present in set: blah blah
#' }
#' Rulesets for variable type \strong{date}:
#' \itemize{
#'    \item present/absent: blah blah
#' }
#' @return data.frame with added column classifying the data according to a set of rules.
#' @export
classify <- function(data, dictionary, rules, default_def = "unknown") {
  ## check input
  check_dictionary(dictionary)
  check_rules(data, rules)

  ## initialise columns to complete
  # add new definitions to the first column
  new_var_name <- names(rules)[1]
  # add in default labels
  new_var <- rep(default_def, nrow(data))
  data <- cbind(new_var, data, stringsAsFactors = FALSE)
  names(data)[1] <- new_var_name

  ## rule results matrix: nrows = num rows of data; ncols = number of definitions
  # logical matrix that will have TRUE where (i,j) fits a rule
  rule_res <- matrix(nrow = nrow(data), ncol = nrow(rules))

  ## outer loop along rows of label rules
  for (j in seq_len(nrow(rules))) {
    ## assume all rows meet current (jth) rule at first
    rule_res[ ,j] <- rep(TRUE, nrow(data))
    ## sequence loses first entry as this is the labels
    col_seq <- seq_along(rules)[-1]

    ## inner loop along to test (ith) element of rule on
    ## relevant column of dataset
    for (i in col_seq) {
      ## name, data column and rule to test:
      # name of variable on which to test definitions:
      var_name <- names(rules)[i]
      # extract column of interest in dataset
      col_data <- data[ ,names(data) == var_name , drop = TRUE]
      # rule to test
      rule <- rules[j, i]

      ## call to classify function depending on variable type:
      # variable type
      var_type <- dictionary$type[var_name == dictionary$variable]
      # function name (in package)
      f <- paste(var_type, "classify", sep = "_")
      # call function
      tmp_res <- do.call(f, args = list(data = col_data, rule = rule))

      ## logical results of rule testing
      # uses boolean algebra on element of vectors
      rule_res[ ,j] <- as.logical(rule_res[ ,j]*tmp_res)
    }
    ## Not desirable that a previously defined row is redefined:
    ## replace_index function will paste multiple definitions together
    # previously undefined rows
    data_default <- data[[1]] == default_def
    replace_index <- rule_res[ ,j] & (data[[1]] == default_def)
    # replace unknown with definition
    data[replace_index, new_var_name] <- rules[j, 1]
    # previously defined rows
    paste_index <- rule_res[ ,j] & !data_default
    current_def <- data[paste_index, new_var_name]
    # paste definitions together
    data[paste_index, new_var_name] <- paste(current_def, rules[j, 1], sep = "_")
  }
  ## return dataset with added definitions
  data
}

