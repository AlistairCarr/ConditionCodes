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

mental_health_codelist_with_codes_long <- fetch_codes(mental_health_codelist,
                                                      collapse_phenotypes = FALSE,
                                                      wide_format = FALSE)


