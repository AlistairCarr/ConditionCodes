
# library(devtools)
# library(roxygen2)
# library(tidyverse)
# library(purrr)

#' A function to fetch codelists from the HDRUK Library
#'
#' This function allows you to create a table of diagnostic codes associated with condition phenotypes created in the HDRUK Library
#' @param x Dataset consisting of phenotype names, as written in the HDRUK Library, and the URL for each phenotype.Condition categories can consist of multiple phenotypes, but each phenotype must be on a seperate row.
#' @param wide_format If set to TRUE, Wide format output table created with codes of the same coding system included as a list object. If FALSE, a long-form list is created which may be useful for data dictionaries.Default is TRUE.
#' @param three_char_ICD10 If set to TRUE, a three character ICD10 variable is created. Useful for compatability with Biobank diagnostic codes. Default is FALSE.
#' @export

fetch_codes <- function(x, wide_format = TRUE, three_char_ICD10 = FALSE) { #long form codes

#clean import link suffix

  x <- x |>
    mutate(Code_list_source = str_replace(Code_list_source, "detail/", "")) %>%    #delete these suffixes
    mutate(Code_list_source = str_replace(Code_list_source, "#home", ""))  |>
    mutate(Code_list_source = paste0(Code_list_source, "export/codes/")) |>   #add suffix
    mutate(Code_list_source = if_else(is.na(Code_list_source), NA_character_, Code_list_source)) #leavel link as NA if no source link


#make empty data frame for output

  phenotype_code_out <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("code", "description", "coding_system", "phenotype_name"))


#don't look for phenotypes with no import link
  codelist_out <- x %>%
    filter(Code_list_source != "NAexport/codes/") #loop stops working if encounters an NA

#for every phenotype, download the csv file containing the codelist

  temp.lists <- purrr::map(codelist_out$Code_list_source, function(url) {
    read_csv(url) |>
      select(code, description, coding_system, phenotype_name) |>
      mutate(across(everything(), as.character))  # convert all cols to character, some are numeric won't be combined

  })

  phenotype_code_out <- bind_rows(temp.lists)


if(wide_format == TRUE) {
 #make output in wide format
  phenotype_code_out <- phenotype_code_out %>%
      pivot_wider(id_cols = phenotype_name,
                  names_from = coding_system,
                  values_from = code,
                  values_fn = list)
}

  if(wide_format == FALSE) {

      phenotype_code_out <- phenotype_code_out |>
        relocate(coding_system)
  }


  if(wide_format == TRUE & three_char_ICD10 == TRUE) {

    #new variable first three characters of ICD10 code
    #for compatability with UK Biobank
    phenotype_code_out <- phenotype_code_out |>
    mutate(three_char_ICD10 = lapply(`ICD10 codes`, substr, 1, 3))

    #select only unique three character values
    phenotype_code_out <- phenotype_code_out |>
    mutate(three_char_ICD10 = sapply(three_char_ICD10, unique)) #unique values

    phenotype_code_out <- phenotype_code_out |>
    relocate(three_char_ICD10, .after = `ICD10 codes`)
  }

  if(wide_format == FALSE & three_char_ICD10 == TRUE) {

    #new variable first three characters of ICD10 code
    #for compatability with UK Biobank
    phenotype_code_out <- phenotype_code_out |>
      mutate(three_char_ICD10 = if_else(coding_system == "ICD10 codes",
             lapply(code, substr, 1, 3), NA))

    #select only unique three character values
    phenotype_code_out <- phenotype_code_out |>
      mutate(three_char_ICD10 = sapply(three_char_ICD10, unique)) #unique values

    phenotype_code_out <- phenotype_code_out |>
      relocate(three_char_ICD10, .after = code)

  }

#join condition lists to original data

x <- left_join(x, phenotype_code_out, by = "phenotype_name")

  return(x)

}



