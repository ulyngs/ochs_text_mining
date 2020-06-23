## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)

# create the folders if they don't exist
dir.create("data_processed")
dir.create("data_raw")


## --------------------------------------------------------------------------------------------------------------------------------
mahabharata_raw <- read_lines("data_raw/MBH1-18U.HTM") %>% 
  as_tibble() %>% 
  rename(text = value)

mahabharata_raw
ramayana_raw <- read_lines("data_raw/sa_rAmAyaNa.txt") %>% 
  as_tibble() %>% 
  rename(text = value)

rgveda_raw <- read_lines("data_raw/sa_Rgveda-edAufrecht.txt") %>% 
  as_tibble() %>% 
  rename(text = value)


## --------------------------------------------------------------------------------------------------------------------------------
mahabharatha_text <- mahabharata_raw %>% 
  slice(82:( n() - 4 )) #keep only from rows 81 until 4 rows before the end

ramayana_text <- ramayana_raw %>% 
  slice(34:n())

rgveda_text <- rgveda_raw %>% 
  slice(34:n())


## --------------------------------------------------------------------------------------------------------------------------------
mahabharatha_cleaned <- mahabharatha_text %>% 
  mutate(verse_line = str_extract(text, "[^\t]+")) %>% 
  mutate(text_clean = str_replace(text, "[^\t]+", ""),
         text_clean = str_replace(text_clean, "(\t)", ""),
         text_clean = str_replace(text_clean, "<BR>", "")) %>% 
  filter(text_clean != "")



## --------------------------------------------------------------------------------------------------------------------------------
ramayana_cleaned <- ramayana_text %>%
  filter(text != "") %>%  #drop empty rows
  mutate(verse_line = str_extract(text, r"(R_\d,\d+\.\d+)")) %>% 
  mutate(verse_line = case_when(
    !is.na(verse_line) ~ verse_line,
    is.na(verse_line) & !is.na(lead(verse_line)) ~ lead(verse_line),
    is.na(verse_line) & is.na(lead(verse_line)) & !is.na(lead(verse_line, n = 2)) ~ lead(verse_line, n = 2),
    is.na(verse_line) & is.na(lead(verse_line)) & is.na(lead(verse_line, n = 2)) ~ lead(verse_line, n = 3)
  )) %>% 
  mutate(text_clean = str_replace(text, r"(R_\d,\d+\.\d+)", ""))


## --------------------------------------------------------------------------------------------------------------------------------
rgveda_cleaned <- rgveda_text %>% 
  filter(text != "") %>%   #drop empty rows
  #slice(1:10) %>% 
  mutate(verse_line = str_extract(text, r"(RV_\d+,\d+\.\d+)")) %>% 
  mutate(verse_line = case_when(
    !is.na(verse_line) ~ verse_line,
    is.na(verse_line) & !is.na(lead(verse_line)) ~ lead(verse_line),
    is.na(verse_line) & is.na(lead(verse_line)) & !is.na(lead(verse_line, n = 2)) ~ lead(verse_line, n = 2),
    is.na(verse_line) & is.na(lead(verse_line)) & is.na(lead(verse_line, n = 2)) ~ lead(verse_line, n = 3)
  )) %>% 
  mutate(text_clean = str_replace(text, r"(RV_\d+,\d+\.\d+)", ""),
         text_clean = str_replace_all(text_clean, r"(\|)", ""))


## --------------------------------------------------------------------------------------------------------------------------------
texts <- mahabharatha_cleaned %>% 
  mutate(document = "Mahabharatha") %>% 
  bind_rows(ramayana_cleaned %>% mutate(document = "Ramayana")) %>% 
  bind_rows(rgveda_cleaned %>% mutate(document = "Rgveda"))

texts %>% 
  write_csv("data_processed/texts_combined_line_by_line.csv")


