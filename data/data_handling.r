# load libraries -------------------------------------------------------------
library(readr)
library(kaggler)
library(tidyverse)
library(highcharter)

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

view(to_from_columns)
colnames(data) <- to_from_columns$code

data <- data %>%
mutate(P1_I = str_replace(P1_I, ".*\\(([^)]+)\\)$", "\\1"))



dados_estados <- data %>%
    select("Estado" = P1_I) %>%
    mutate(P1_I = str_replace(Estado, ".*\\(([^)]+)\\)$", "\\1")) %>%
    group_by(Estado) %>%
    summarise(
        n = n()
    )

dados_genero <- data %>% select(P1_I,P1_B) %>%
    mutate(
        P1_B = case_when(
            P1_B %in% c("Outro","Prefiro não informar") ~ "Outro/Não informado",
            TRUE ~ P1_B
        )
    ) %>%
    rename(Estado = P1_I) %>%
    table() %>%
    as.data.frame() %>%
    pivot_wider(names_from = P1_B,values_from = Freq)

dados_etnia <- data %>% select(P1_I,P1_C) %>%
    rename(Estado = P1_I) %>%
    mutate(P1_C = case_when(
        P1_C %in% c("Outra","Prefiro não informar") ~ "Outra/Não informada",
        TRUE ~ P1_C
    )) %>%
    table() %>%
    as.data.frame() %>%
    pivot_wider(names_from = P1_C,values_from = Freq)

dados_mapa <- dados_estados %>%
    left_join(dados_genero,by = "Estado") %>%
    left_join(dados_etnia,by = "Estado") 


hcmap("countries/br/br-all", data = dados_mapa, value = "n",
      joinBy = c("hc-a2", "Estado"), name= "<b>State Of Data - Brasil 2023 </b>",
      dataLabels = list(enabled = TRUE),
      tooltip = list(pointFormat = paste0(
          "<style='display: block; font-size: 11px;'> <br> 
          <b>Genero</b> <br> 
           &deg; Masculino: {point.Masculino} <br>
           - Feminino: {point.Feminino} <br> 
           - Outro/Não informado: {point.Outro/Não informado} <br> 
           <b> Etnia</b> <br>
            - Branca: {point.Branca} <br> 
            - Preta: {point.Preta} <br> 
            - Parda: {point.Parda} <br> 
            - Amarela: {point.Amarela} <br> 
            - Indígena: {point.Indígena} <br> 
            - Outro/Não informado: {point.Outra/Não informada} <br> <br>
           <b>Total de Respostas</b> <br>
           {point.value} <br> 
           </style>"))
      ) %>%
  hc_title(text = "Brasil") %>%
  hc_colorAxis(min = 0, max = max(data_fake$n)) %>% 
  hc_legend(layout = "vertical", align = "right", valueDecimals = 2)

data_fake %>% view()
