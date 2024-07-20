# load libraries -------------------------------------------------------------
library(readr)
library(kaggler)
library(tidyverse)
# get data from kaggle -------------------------------------------------------
kaggler::kgl_auth(creds_file = './credentials/kaggle.json')
response <- kaggler::kgl_datasets_download_all(owner_dataset = "datahackers/state-of-data-brazil-2023")
utils::download.file(response[["url"]], "data/state-of-data-brazil-2023.zip", mode="wb")
unzip_result <- utils::unzip("data/state-of-data-brazil-2023.zip",exdir = "data/", overwrite = TRUE)
data <- readr::read_csv(unzip_result)

# data handling --------------------------------------------------------------
data <- data %>%
  select("('P0 ', '" = `('P0', 'id')`,matches("\\('P[1-8]_[a-zA-Z|1] "))

columns <- colnames(data)

columns <- columns %>%
str_remove_all("\\('") %>%
str_remove_all("'\\)")

to_from_columns <- tibble::tibble('columns' = columns) %>%
    separate(columns,c("code","question"),sep = " ', '") %>%
    mutate(question = ifelse(code == "P0","Id",question)) %>%
    mutate_all(toupper) %>%
    mutate(question = str_replace_all(question," ","_")) %>%
    mutate(question = str_replace_all(question,"/","_")) %>%
    mutate(question = str_replace_all(question,"_"," "))
readr::write_csv(to_from_columns,"data/to_from_columns.csv")
colnames(data) <- to_from_columns$code
