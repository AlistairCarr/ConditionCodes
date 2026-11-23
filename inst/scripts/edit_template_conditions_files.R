
#fix errors in CMS file
#non caliber links

load("data/CMS.rda")

CMS <- CMS |> filter(CMS$phenotype_name != "Multimorbidity_connective tissue disorder")

CMS2 <- CMS |>
  filter(CMS$phenotype_name != "Multimorbidity_connective tissue disorder")



names(CMS)
class(CMS)
names(CMS)

head(CMS$phenotype_name)
