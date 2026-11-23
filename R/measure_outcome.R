# library(devtools)
# library(roxygen2)

# library(purrr)
#
# library(ConditionCodes)
#
# condition_list  <- test
#
# data <- ukb_synthetic
#
# coding_systems = "all"
#
# patient_ID <- "eid"
#
# condition_threshold = 3
#
# mm_outcome_name = "cardiometabolic"
# by_phenotype = FALSE
#
# test2 <- measure_outcome(
#   data = ukb_synthetic,
#   condition_list = test,
#   mm_outcome_name = "cardiometabolic",
#   condition_threshold = 3,
#   coding_systems = "all",
#   patient_ID = "eid",
#   by_phenotype = FALSE,
#   contributing_conditions = TRUE, condition_diagnoses = FALSE, condition_count = FALSE)

#' A function to calculate multimorbidity or single condition outcomes
#'
#' This function allows you to measure a health outcome from a table of diagnostic codes
#' @importFrom stats na.omit setNames
#' @importFrom dplyr mutate filter select left_join group_by arrange slice
#'   summarise across any_of contains starts_with c_across everything if_else
#'   relocate all_of
#' @importFrom stringr str_replace str_trim str_detect str_split
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map reduce discard
#' @importFrom rlang :=
#' @param data Health data in long format consisting of
#' @param condition_list Codelist dataset consisting of phenotype names, as written in the HDRUK Library, and the URL for each phenotype.Condition categories can consist of multiple phenotypes, but each phenotype must be on a seperate row.
#' @param condition_threshold Threshold of conditions at which multimorbidity is defined
#' @param coding_systems Which coding systems to select to measure multimorbidity. Deafault is all. If selecting multiple use c("three_char_IC10", "Read covdes v2" ) format.
#' @param patient_ID Name of unique study pariticpant identified. Default is "eid".
#' @param mm_outcome_name Variable name for multimorbidity outcome. E.g. "CMS", "cardiometabolic".
#' @param by_phenotype If set to TRUE, calculates the first occurence of each phenotype rather diagnosis of any phenotype belonging to that condition category.
#' @param contributing_conditions IF set to TRUE (default), see the ordering of the contributing conditions used to define multimorbidity. May want to set to false if looking at multiple multimrobidity outcomes.
#' @param condition_diagnoses If set to TRUE (default), see all the conditions diagnosed in an individual. May want to set to false if examining multiple multimorbidity outcomes.
#' @param condition_count If set to TRUE (default), show a variable of the total number of conditions diagnosed. May want to set to false if examining multiple multimorbidity diagnoses.
#' @export

measure_outcome <- function(data, condition_list, mm_outcome_name, condition_threshold, coding_systems, patient_ID, by_phenotype = FALSE, contributing_conditions = TRUE, condition_diagnoses = TRUE, condition_count = TRUE) {


#collapse to one row per condition category

if(by_phenotype == FALSE) {

temp <- condition_list |>
    dplyr::group_by(Condition_category, Dx) |>
    dplyr::summarise(
      across(
        phenotype_name | Code_list_source | contains("codes") | contains("three_char_ICD10"),
        ~ paste0(.x, collapse = ", ")
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      across(
        contains("codes") | contains("three_char_ICD10"),
        ~ stringr::str_split(.x, ", ")
      )
    )  |>
    dplyr::mutate(
      across(
        contains("codes") | contains("three_char_ICD10"),
        ~ purrr::map(.x, ~ unique(trimws(.x[.x != "" & !is.na(.x)]))) #remove white space
      )
    )|>
    dplyr::mutate(
      across(
        contains("codes") | contains("three_char_ICD10"),
        ~ sapply(.x, unique)
      ))

   ## unlist codes

temp <- temp |>
          dplyr::mutate(across(contains("codes") | contains("three_char_ICD10"),
                        ~ sapply(.x, paste, collapse = ", ")))

}


#replace comma with | for regex compatbility


  if(by_phenotype == TRUE) {

    temp <- condition_list

  }

 temp <- temp |>
  dplyr::mutate(across(c(contains("codes"), any_of("three_char_ICD10")), ~ stringr::str_replace_all(.x, ", ", "|")))


### make output file one per patient

 out <- data |>
   dplyr::select(all_of(patient_ID)) |>
   dplyr::distinct()



for (i in 1:nrow(temp)) {

dx_name <- temp$Dx[i]

#select the coding system columns that will be used

  if (coding_systems == "all") {
    cols <- temp[i, ] |>
      dplyr::select(contains("codes"), any_of("three_char_ICD10"))
  } else {
    cols <- temp[i, ] |>
      dplyr::select(any_of(coding_systems))
  }

#get regex list of condition codes across all selected condition coding systems

pattern <- cols |>
  unlist() |>
  na.omit() |>
  stringr::str_split(",\\s*") |>
  unlist() |>
  stringr::str_trim() |>
  purrr::discard(~ .x == "") |>  # remove empty strings
  paste(collapse = "|")

# Skip if no codes found for this condition
if (pattern == "") {
  warning(paste("No codes found for condition:", dx_name))
  next
}

temp2 <- data |>
    dplyr::filter(!is.na(first.dx.date)) |> #don't run for ICD 10 codes people haven't been diagnosed with
    dplyr::filter(stringr::str_detect(diagnostic_code, pattern)) |> #take each row from the conditions list, and go through each code, and if it matches with the row in the data of health conditions, filter on that
    dplyr::slice_min(order_by = first.dx.date, by = all_of(patient_ID), with_ties = FALSE) |> #dplyr::select the earliest admission date per person per condition
  dplyr::select(all_of(patient_ID), inci=first.dx.date, first.dx.source)   #change prefix of admission date column to 'inci'

if(by_phenotype == FALSE) {
names(temp2)[2] <- paste0('inci.', temp$Dx[i])  #names based on condition groupings
names(temp2)[3] <- paste0('dx.source.', temp$Dx[i])
}

if(by_phenotype == TRUE) {
  names(temp2)[2] <- paste0('inci.', temp$phenotype_name[i])  #names based on individual phenotype names
  names(temp2)[3] <- paste0('dx.source.', temp$phenotype_name[i])
}


out <- out |> dplyr::left_join(temp2, by = patient_ID)

}

#make multimorbidity outcome variable

out <- out |>
  dplyr::rowwise() |>
  dplyr::mutate(across(starts_with("inci."), ~ as.Date(.x))) |>
  dplyr::mutate(
    condition_count = sum(!is.na(dplyr::c_across(starts_with("inci.")))),
    "mm.{mm_outcome_name}" := dplyr::if_else(condition_count >= condition_threshold, 1, 0) #set to 1 if greather than threshold
  )


#get diagnosis dates


# Only proceed if at least one condition was matched
if (!any(startsWith(names(out), "inci."))) {
  warning("No conditions were matched in the data. Returning empty result.")
  return(out)
}

#make long format of data plus diagnosis source

temp_out <- out |>
  dplyr::rowwise() |>
  dplyr::mutate(across(starts_with("inci."), ~ as.Date(.x))) |>
  dplyr::mutate(
    condition_count = sum(!is.na(dplyr::c_across(starts_with("inci.")))),
    "mm_status.{mm_outcome_name}" := dplyr::if_else(condition_count >= condition_threshold, 1, 0)
  ) |>
  tidyr::pivot_longer(
    cols = c(starts_with("inci."), starts_with("dx.source.")),
    names_to = c(".value", "dx"),
    names_pattern = "^(inci|dx\\.source)\\.(.*)"
  ) |>
  dplyr::select(all_of(patient_ID), condition_count, any_of(paste0("mm_status.", mm_outcome_name)), dx, inci, dx.source) |>
  dplyr::rename(date = inci)



# Create ranked condition variables up to the threshold



condition_ranks <- purrr::map(1:condition_threshold, function(rank) {

  rank_label <- c("first", "second", "third", "fourth", "fifth",
                  "sixth", "seventh", "eighth", "ninth", "tenth")[rank]

  temp_out |>
    dplyr::group_by(.data[[patient_ID]]) |>
    dplyr::arrange(date) |>
    dplyr::slice(rank) |>
    dplyr::mutate(
      "inci.{rank_label}_con" := date,
      "{rank_label}_con.dx" := ifelse(!is.na(date), stringr::str_replace(dx, "inci\\.", ""), NA),
      "{rank_label}_con.source" := dx.source
    ) |>
    dplyr::select(all_of(patient_ID),
                  contains(paste0(rank_label, "_con")),
                  starts_with("inci."))


})

# Join all ranked condition data frames to out
out <- purrr::reduce(condition_ranks, ~ dplyr::left_join(.x, .y, by = patient_ID), .init = out)

# make data for multimorbidity diagnosis

rank_label <- c("first", "second", "third", "fourth", "fifth",
                "sixth", "seventh", "eighth", "ninth", "tenth")[condition_threshold]

out <- out |>
  dplyr::mutate(
    !!paste0("inci.mm.", mm_outcome_name) := .data[[paste0("inci.", rank_label, "_con")]]
  ) |>
  dplyr::relocate(!!paste0("inci.mm.", mm_outcome_name), .after = !!paste0("mm.", mm_outcome_name))

# Drop rank columns if show_ranks = FALSE
if (contributing_conditions == FALSE) {

  rank_labels <- c("first", "second", "third", "fourth", "fifth",
                   "sixth", "seventh", "eighth", "ninth", "tenth")[1:condition_threshold]

  out <- out |>
    dplyr::select(-any_of(c(
      paste0("inci.", rank_labels, "_con"),
      paste0(rank_labels, "_con.dx"),
      paste0(rank_labels, "_con.source")
    )))

}

#drop columns showing diagnosis of individual condition

if (condition_diagnoses == FALSE) {

    cols_to_drop <- names(out)[2:(which(names(out) == "condition_count") - 1)] #drop all condition diagnosis from second column to just before condition count

    out <- out |>
      dplyr::select(-any_of(cols_to_drop))

}


if (condition_count == FALSE) {


  out <- out |>
    dplyr::select(-condition_count)

}

return(out)

}

