## make synthetic Biobank data for outcome ascertainment demonstration


library(tidyverse)
library(data.table)
library(xml2)
library(rvest)
library(readxl)
library(RSQLite)
library(haven)

#extend timeout options so can load datasets

options(timeout = 600)

#install synthetic data from UKB webiste

tbl1 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41257_HES_SimDates.tsv", nrows = 1000)

tbl2 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41260_HES_SimDates.tsv", nrows = 1000)

tbl3 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41262_HES_SimDates.tsv", nrows = 1000)

tbl4 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41263_HES_SimDates.tsv", nrows = 1000)

tbl5 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41280_HES_SimDates.tsv", nrows = 1000)

tbl6 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41281_HES_SimDates.tsv", nrows = 1000)

tbl7 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41282_HES_SimDates.tsv", nrows = 1000)

tbl8 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/41283_HES_SimDates.tsv", nrows = 1000)

tbl9 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/bulk_strings.tsv", nrows = 1000)

tbl10 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/dates_death.tsv", nrows = 1000)

tbl11 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/datetime_fields_2.tsv", nrows = 1000)

tbl12 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/datetime_fields.tsv", nrows = 1000)

tbl13 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/fo_fields_trimmed.tsv", nrows = 1000)

tbl14 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/integer_arrays_part1.tsv", nrows = 1000)

tbl15 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/integer_arrays_part2.tsv", nrows = 1000)

tbl16 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/integer_diet_quest_fields.tsv", nrows = 1000)

tbl17 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/integer_no_arrays.tsv", nrows = 1000)

tbl18 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/integer_other_quest_fields.tsv", nrows = 1000)

tbl19 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/oaa_fields.tsv", nrows = 1000)

tbl20 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/real_fields1.tsv", nrows = 1000)

tbl21 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/real_fields2.tsv", nrows = 1000)

tbl22 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/string_fields1.tsv", nrows = 1000)

tbl23 <- fread("https://biobank.ndph.ox.ac.uk/synthetic_dataset/tabular/string_fields2.tsv", nrows = 1000)


ukb_synthetic <- list(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6, tbl7, tbl8,
                      tbl9, tbl10, tbl11, tbl12, tbl13, tbl14, tbl15,
                      tbl16, tbl17, tbl18, tbl19, tbl20, tbl21, tbl22, tbl23) |>
  reduce(left_join, by = "eid")


rm(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6, tbl7, tbl8,
    tbl9, tbl10, tbl11, tbl12, tbl13, tbl14, tbl15,
    tbl16, tbl17, tbl18, tbl19, tbl20, tbl21, tbl22, tbl23)



data <- ukb_synthetic |>
  select(eid,
         # `21003-0.0`, #age
         # `31-0.0`,   #sex
         `130000-0.0`:`132605-0.0`)  #first occurences diagnosis dates


write_rds(ukb_synthetic, "data/total_ukb_synthetic.rds")



#remove suffix
data <- data %>%
           rename_with(~str_remove(., "-0.0"))



#make a datafield lookup file from Biobank datafields
#infectious disease

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2401"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- table[[1]]

#stack additional datafiels
#blood conditions

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2403"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

## endocrine

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2404"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#mental health

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2405"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#nervous system
url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2406"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#eye

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2407"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])


#ear
url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2408"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])


#circulatory

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2409"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#resp

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2410"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#digestive

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2411"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#skin
url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2412"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#msk

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2413"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#genitourinary

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2414"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

#pregnancy

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2415"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])


#perinatal

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2416"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])


#congenital

url <- "https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2417"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page) #Parses tables into data frames

datafields <- rbind(datafields, table[[1]])

write_rds(datafields, "data/ukb_datafields.rds")

#### relabel datafielsds #######

#use clever function to re-label field labels in main dataset


datafields <- datafields %>%
  mutate(FieldID = as.character(`Field ID`))

new_names_vec <- deframe(select(datafields, Description, FieldID))
new_names_vec

data <- data %>%
  rename(!!!new_names_vec[new_names_vec %in% names(data)])

#pivot longer
#separate into condition date then condition source, and merge together


temp.date <- data %>%
  select(-starts_with("Source")) %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "condition",
    values_to = "first.dx.date") %>%
  mutate(row_n = row_number()) #variable to merge on



temp.source <- data %>%
  select(-starts_with("Date")) %>%
  pivot_longer(
    cols = starts_with("Source"),
    names_to = "condition.source",
    values_to = "first.dx.source") %>%
  mutate(row_n = row_number()) #variable to merge on


data <-  left_join(temp.date, temp.source, by = c("row_n", "eid"))

## rename condition with 3 letter ICD 10 code


data <- data %>%
  mutate(condition = str_sub(condition, start = 6L, end = 8L))

data <- data %>%
  select(eid, condition, first.dx.date, first.dx.source)


##add cancer diagnoses

cancer_d <- ukb_synthetic |>
  select(eid, contains("40006"), #type of cancer
                                    contains ("40005")) %>%  #date of diagnosis

  ##make into same format as first occurences data

  #appears to be some historical instance of cancer diagnoses with no date but nothing since bl

  temp.cond <- cancer_d %>%
  select(eid, contains("40006")) %>%
  pivot_longer(
    cols = contains("40006"),
    names_to = "instance",
    values_to = "condition") %>%
  mutate(row_n = row_number())

temp.date <- cancer_d %>%
  select(eid, contains("40005")) %>%
  pivot_longer(
    cols = contains("40005"),
    names_to = "instance",
    values_to = "first.dx.date") %>%
  mutate(row_n = row_number()) #variable to merge on

cancer_d <- left_join(temp.cond, temp.date, by = c("row_n", "eid")) %>%
  select(eid, condition, first.dx.date)

cancer_d  <- cancer_d  %>%
  mutate(condition = if_else(condition == "", NA, condition)) #make condition NA

cancer_d  <- cancer_d  %>%
  mutate(first.dx.source = if_else(!is.na(condition), 99, NA)) #make condition NA

#99 = diagnosed by cancer registry data

##turn into three character ICD10 code

cancer_d  <- cancer_d  %>%
  mutate(condition = str_sub(condition, start = 1L, end = 3L)) #first 3 characters

data <- rbind(data, cancer_d )

data <- data %>%
  filter(!is.na(first.dx.date)) #get rid of blank condition rows to reduce file size

#rename condition column diagnostic_code

data <- data |>
  rename(diagnostic_code = condition) |>
  mutate(code_type = "three_char_ICD10") |>
  relocate(code_type, .after = diagnostic_code)


write_rds(data, "data/ukb_synthetic.rds")


