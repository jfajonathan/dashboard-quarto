library(tidyverse)
library(highcharter)

data  <- readr::read_csv("data/final_data.csv")


# map plot ---------------------------------------------------------------------
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
           - Masculino: {point.Masculino} <br>
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
  hc_colorAxis(min = 0, max = max(dados_mapa$n)) %>% 
  hc_legend(layout = "vertical", align = "right", valueDecimals = 2)
