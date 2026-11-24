
# library(devtools)
# library(roxygen2)
# library(purrr)

#' A function to fetch codelists from the HDRUK Library
#'
#' This function allows you to create a table of diagnostic codes associated with condition phenotypes created in the HDRUK Library
#' @param x Dataset consisting of phenotype names, as written in the HDRUK Library, and the URL for each phenotype.Condition categories can consist of multiple phenotypes, but each phenotype must be on a seperate row.
#' @param wide_format If set to TRUE, Wide format output table created with codes of the same coding system included as a list object. If FALSE, a long-form list is created which may be useful for data dictionaries.Default is TRUE.
#' @param three_char_ICD10 If set to TRUE, a three character ICD10 variable is created. Useful for compatability with Biobank diagnostic codes. Default is FALSE.
#' @param collapse_phenotypes If set to TRUE, collapses all condition phenotypes that contribute to a condition category into one row. Default is FALSE.
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @export

fetch_codes <- function(x, wide_format = TRUE, collapse_phenotypes = FALSE, three_char_ICD10 = FALSE) { #long form codes

options(timeout = 600) #needs longer before timeout otherwise can't download csv files in time, extend to 10 mins

#clean import link suffix
  x <- x |>
    dplyr::mutate(
      Code_list_source = dplyr::if_else(
        !is.na(Code_list_source),
        Code_list_source |>
          stringr::str_replace("detail/", "") |>
          stringr::str_replace("#home", "") |>
          stringr::str_replace_all("\\s", "") |>  # remove any whitespace
          paste0("export/codes/"),
        NA_character_  # leave NA rows as NA
      )
    )

#make empty data frame for output
  phenotype_code_out <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("code", "description", "coding_system", "phenotype_name"))


#don't look for phenotypes with no import link
  codelist_out <- x |>
    dplyr::filter(!is.na(Code_list_source))  #stops working if comes across no URL
#for every phenotype, download the csv file containing the codelist

  temp.lists <- purrr::map(codelist_out$Code_list_source, function(url) {
    readr::read_csv(url) |>
      dplyr::select(code, description, coding_system, phenotype_name) |>
      dplyr::mutate(across(everything(), as.character))  # convert all cols to character, some are numeric won't be combined

  })

#bind codelists into single dataframe
 phenotype_code_out <- dplyr::bind_rows(temp.lists)


if(wide_format == TRUE) {
 #make output in wide format
  phenotype_code_out <- phenotype_code_out |>
      tidyr::pivot_wider(id_cols = phenotype_name,
                  names_from = coding_system,
                  values_from = code,
                  values_fn = list)
}

  if(wide_format == FALSE) {

      phenotype_code_out <- phenotype_code_out |>
        dplyr::relocate(coding_system)
  }


  if(wide_format == TRUE & three_char_ICD10 == TRUE) {

    #new variable first three characters of ICD10 code
    #for compatability with UK Biobank
    phenotype_code_out <- phenotype_code_out |>
    dplyr::mutate(three_char_ICD10 = lapply(`ICD10 codes`, substr, 1, 3))

    #dplyr::select only unique three character values
    phenotype_code_out <- phenotype_code_out |>
    dplyr::mutate(three_char_ICD10 = sapply(three_char_ICD10, unique)) #unique values

    phenotype_code_out <- phenotype_code_out |>
      dplyr::relocate(three_char_ICD10, .after = `ICD10 codes`)
  }

  if(wide_format == FALSE & three_char_ICD10 == TRUE) {

    #new variable first three characters of ICD10 code
    #for compatability with UK Biobank
    phenotype_code_out <- phenotype_code_out |>
      dplyr::mutate(three_char_ICD10 = dplyr::if_else(coding_system == "ICD10 codes",
             lapply(code, substr, 1, 3), NA))

    #dplyr::select only unique three character values
    phenotype_code_out <- phenotype_code_out |>
      dplyr::mutate(three_char_ICD10 = sapply(three_char_ICD10, unique)) #unique values

    phenotype_code_out <- phenotype_code_out |>
      dplyr::relocate(three_char_ICD10, .after = code)

  }

#join condition lists to original data

x <- dplyr::left_join(x, phenotype_code_out, by = "phenotype_name")

#unlist codes for neater formatting

x <- x |>
  dplyr::mutate(across(contains("codes") | contains("three_char_ICD10"),
                       ~ sapply(.x, paste, collapse = ", ")))



### if collapse by condition categories = true

if(collapse_phenotypes == TRUE & wide_format == TRUE) {

x <- x |>
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
      )
    )

x <- x |>
    dplyr::mutate(across(contains("codes") | contains("three_char_ICD10"),
                  ~ sapply(.x, paste, collapse = ", ")))


}
#if not collapse phenotypes, return condition list as is
return(x)

}




