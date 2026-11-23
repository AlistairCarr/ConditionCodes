#demonstration code

#uninstall.package
remove.packages("ConditionCodes")


#install from gt hub

remotes::install_github("AlistairCarr/ConditionCodes", force = TRUE)

library(ConditionCodes)
##

###### Making code lists #######


### What's required is HDRUK Phenotype URL

# 1. Make a list of your phenotypes

mental_health_codelist <- ConditionCodes::mental_health

##SSRI phenotype based on prescriptions, rest based on CALIBER phenotypes

## multiple phenotypes per condition

cardimetabolic_codelist <- ConditionCodes::cardiometabolic

CMS_codelist <- ConditionCodes::CMS

# 2. Use fetch_codes()

mental_health_codelist_with_codes <- fetch_codes(mental_health_codelist)

#default is to present codes per phenotype

#can group by larger condition category if desider (dewfault = FALSE)


mental_health_codelist_with_codes2 <- fetch_codes(mental_health_codelist, collapse_phenotypes = TRUE)


#you may also wish in long-form to see the description of all individual codes

mental_health_codelist3 <- fetch_codes(mental_health_codelist,
                                                      collapse_phenotypes = FALSE,
                                                      wide_format = FALSE)


## if you're using Biobank data, also function to create a three character ICD10 code variable

mental_health_codelist_with_codes4 <- fetch_codes(mental_health_codelist,
                                                      collapse_phenotypes = FALSE,
                                                      wide_format = TRUE,
                                                  three_char_ICD10 = TRUE)

#but not relevent for here

#CMS list of conditions defined with ICD10 codes



CMS_codelist_with_codes <- fetch_codes(CMS_codelist, three_char_ICD10 = TRUE)

#realistically you'd only make the codelist with codes once then save that file


cardiometabolic_codelist_with_codes <- fetch_codes(cardiometabolic, three_char_ICD10 = TRUE)

#realistically you'd only make the codelist with codes once then save that file


rm(list = setdiff(ls(), c("CMS_codelist_with_codes", "cardiometabolic_codelist_with_codes")))

#could create altnerative codelists e.g. Barnett, Ho etc


### calculate outcome with codelist as a lookup file

data <- ConditionCodes::ukb_synthetic


mm_CMS <- measure_outcome(
  data = data,  #long form data frame with unique patient IDs, diagnostic codes per pt, date of diagnosis, and source of diagnosis
  condition_list = CMS_codelist_with_codes, #lookup file created with fetch_condition()
  mm_outcome_name = "CMS", #suffix for multimorbidity outcome var. Useful if you want to look at multiple way to diagnose mm.
  condition_threshold = 2, #threshold at which to measure multimorbidity,
  coding_systems = "all",  #could select only read codes, only ICD10 etc
  patient_ID = "eid", #unique study ID / patient identifier
)



?measure_outcome

