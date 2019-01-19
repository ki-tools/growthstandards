#' Get subject-level or time-varying variables and rows of longitudinal data
#'
#' @param dat data frame with longitudinal data
#' @rdname subjecttime
#' @export
get_subject_data <- function(dat) {
  if (!has_data_attributes(dat))
    dat <- get_data_attributes(dat)
  
  var_summ <- attr(dat, "hbgd")$var_summ
  subj_vars <- c("subject-level", "subject id")
  ind <- which(var_summ$type %in% subj_vars)
  if (length(ind) == 0)
    return(NULL)
  
  dat <- dat[!duplicated(dat$subjid), var_summ$variable[ind]]
  
  var_summ <- subset(var_summ, variable %in% names(dat))
  attr(dat, "hbgd")$var_summ <- var_summ
  dat
}

#' @rdname subjecttime
#' @export
get_time_data <- function(dat) {
  if (!has_data_attributes(dat))
    dat <- get_data_attributes(dat)
  
  var_summ <- attr(dat, "hbgd")$var_summ
  subj_vars <- c("subject-level", "subject id")
  ind <- which(!var_summ$type %in% subj_vars)
  if (length(ind) == 0)
    return(NULL)
  dat <- dat[, var_summ$variable[ind]]
  
  var_summ <- subset(var_summ, variable %in% names(dat))
  attr(dat, "hbgd")$var_summ <- var_summ
  dat
}


#' Infer and attach attributes to a longitudinal growth study dataset
#'
#' Infer attributes such as variable types of a longitudinal growth study and add as attributes to the dataset.
#'
#' @param dat a longitudinal growth study dataset
#' @param meta a data frame of meta data about the variables (a row for each variable)
#' @param study_meta a single-row data frame or named list of meta data about the study (such as study description, etc.)
#' @details
#' attributes added:
#' - \code{subjectlevel_vars}: vector of names of subject-level variables
#' - \code{longi_vars}: vector of names of time-varying variables
#' - \code{time_vars}: vector of names of measures of age
#' - \code{var_summ}: data frame containing variable summaries with columns \code{variable}, \code{label}, \code{type}[subject id, time indicator, time-varying, constant], \code{n_unique}
#' - \code{subj_count}: data frame of counts of records for each subject with columns \code{subjid}, \code{n}
#' - \code{n_subj}: scalar containing total number of subjects
#' - \code{labels} named list of variable labels - either populated by matching names with a pre-set list of labels (see \code{\link{hbgd_labels}}) or from a list provided from the \code{meta} argument
#' - \code{study_meta}: data frame of meta data (if provided from the \code{study_meta} argument)
#' - \code{short_id}: scalar containing the short unique identifier for the study (if \code{study_meta} is provided)
#' @seealso \code{\link{get_data_attributes}}
#' @examples
#' cpp <- get_data_attributes(cpp)
#' str(attributes(cpp))
#' @export
get_data_attributes <- function(dat, meta = NULL, study_meta = NULL) {
  
  agevars <- "agedays"
  
  hbgd_attrs <- list()
  
  ## add labels
  meta_lab <- NULL
  if (!is.null(meta) && is.data.frame(meta)) {
    hbgd_attrs$meta <- meta
    if (all(c("label", "name") %in% names(meta))) {
      meta_lab <- as.list(meta$label)
      names(meta_lab) <- meta$name
    } else {
      message("variables 'label' and 'name' are missing in the provided 'meta' data frame so labels were not taken from this - will use default labels") # nolint
    }
  }
  # fill in labels from default or use variable name as label when no label
  lab <- lapply(names(dat), function(nm) {
    res <- meta_lab[[nm]]
    if (is.null(res))
      res <- hbgd::hbgd_labels[[nm]]
    if (is.null(res))
      res <- nm
    res
  })
  names(lab) <- names(dat)
  hbgd_attrs$labels <- lab
  
  ## extract which variables are constants within each subject
  ## and which vary within each subject - store as attributes
  subjid_n <- dat %>%
    dplyr::group_by_(~subjid) %>%
    dplyr::summarise_each(dplyr::funs(n_distinct))
  same_ind <- sapply(subjid_n, function(x)
    all(x == 1))
  subjectlevel_vars <- names(which(same_ind))
  longi_vars <- setdiff(names(dat), c("subjid", agevars, subjectlevel_vars))
  time_vars <- intersect(agevars, names(dat))
  
  hbgd_attrs$subjectlevel_vars <- subjectlevel_vars
  hbgd_attrs$longi_vars <- longi_vars
  hbgd_attrs$time_vars <- time_vars
  
  lab <- hbgd_attrs$labels[names(dat)]
  
  ## create a summary of the variables
  var_summ <- data.frame(
    variable = names(dat),
    label = unlist(lab),
    type = NA,
    vtype = "cat",
    stringsAsFactors = FALSE
  )
  var_summ$type[var_summ$variable %in% subjectlevel_vars] <- "subject-level"
  var_summ$type[var_summ$variable %in% longi_vars] <- "time-varying"
  var_summ$type[var_summ$variable == "subjid"] <- "subject id"
  if (length(time_vars) > 0)
    var_summ$type[var_summ$variable %in% time_vars] <- "time indicator"
  var_summ$n_unique <- sapply(dat, function(a) length(unique(a)))
  var_summ$vtype[sapply(dat, is.numeric) & var_summ$n_unique > 10] <- "num"
  class(var_summ) <- c(class(var_summ), "var_summ")
  hbgd_attrs$var_summ <- var_summ
  
  ## get number of records for each subject and total number of subjects
  n_subj <- dat %>%
    group_by_(~subjid) %>%
    summarise(n = n())
  hbgd_attrs$subj_count <- data.frame(n_subj)
  hbgd_attrs$n_subj <- nrow(n_subj)
  
  if (nrow(n_subj) == nrow(dat))
    message("  - this data has as many rows as unique subjects.")
  
  if (!is.null(study_meta)) {
    hbgd_attrs$study_meta <- study_meta
    if (!is.null(study_meta$short_id))
      hbgd_attrs$short_id <- study_meta$short_id
  }
  
  if (any("agedays" %in% agevars)) {
    ## compute frequency of records by agedays
    ad_tab <- data.frame(dat %>%
                           dplyr::group_by(agedays) %>%
                           dplyr::summarise(n = n()))
    class(ad_tab) <- c(class(ad_tab), "ad_tab")
    hbgd_attrs$ad_tab <- ad_tab
  }
  
  attr(dat, "hbgd") <- hbgd_attrs
  
  dat
}

has_data_attributes <- function(dat)
  "hbgd" %in% names(attributes(dat))