#Instalação de pacotes

install.packages("readxl")
install.packages("summarytools")
install.packages("GGally")
install.packages("janitor")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("shiny")

#Bibliotecas

library(readxl)
library(summarytools)
library(GGally)
library(dplyr)
library(tidyr)
library(janitor)
library(corrplot)
library(ggplot2)
library(lubridate)
library(patchwork)
library(scales)  
library(ggpubr)
library(tidyverse)
library(shiny)

#Bases de dados 
##Leitura da base de dados 

dados <- read_excel("dados.xlsx")
dados <- dados %>% clean_names()
colnames(dados)
dados_wide$grupo_de_mercadoria <- NULL

###arrumar a base 

dados_wide <- dados %>%
  pivot_wider(
    names_from = `mercadoria_sh4`,      
    values_from = mov_portuaria,         
    values_fill = 0                       
  )

###ano para variável numerica 

dados_wide <- dados_wide %>%
  mutate(ano = as.numeric(ano))

View(dados_wide)

dados <- dados %>% select(-mercadoria_sh4)

##Descrição da base com descr 
 

descr(dados_wide, style = "grid")
colSums(is.na(dados_wide))

##Matriz de espalhamento scatterplot
dados_numericos <- dados_wide %>% select(where(is.numeric))
ggpairs(dados_numericos)

##  matriz de correlação com corrplot
matriz_cor <- cor(dados_numericos, use = "complete.obs")
corrplot(matriz_cor, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

###Preparo da base para analises sobre a operação carne fraca em 2017

dados_wide <- dados_wide %>%
  mutate(
    ano_semestre = paste0(ano, "-S", semestre),
    periodo = ifelse(ano < 2017, "Pré-2017", "Pós-2017")
  )


str(dados_wide)

#Histogramas 

##Histograma soja 1202

###Testanndo o melhor jeito de fazer a análise 

sturges_binwidth <- function(x) {
  n <- length(x)
  bins <- ceiling(1 + log2(n))
  return((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bins)
}

fd_bw <- fd(dados_wide$`1201`)
sturges_bw <- sturges_binwidth(dados_wide$`1201`)

###Histogramas lado a lado

p1 <- ggplot(dados_wide, aes(x = `1201`)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = fd_bw, fill = "lightblue", color = "black") +
  geom_density(kernel = "epanechnikov", color = "blue") +
  facet_wrap(~periodo) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Freedman-Diaconis", x = "Movimentação de Soja (em milhões)", y = "Densidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(dados_wide, aes(x = `1201`)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = sturges_bw, fill = "lightcoral", color = "black") +
  geom_density(kernel = "epanechnikov", color = "red") +
  facet_wrap(~periodo) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Sturges", x = "Movimentação de Soja (em milhões)", y = "Densidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Mostrar os dois juntos
p1 + p2



### Calcular número de bins e binwidth com Sturges para soja (1201)
soja_vals <- dados_wide$`1201`[!is.na(dados_wide$`1201`)]
n_soja <- length(soja_vals)
k_soja <- ceiling(log2(n_soja) + 1)
binwidth_soja <- (max(soja_vals) - min(soja_vals)) / k_soja

###Histograma 

ggplot(dados_wide, aes(x = `1201`)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = binwidth_soja, fill = "skyblue", color = "black") +
  facet_wrap(~periodo) +
  labs(title = "Distribuição da Movimentação de Soja (SH4: 1201)",
       x = "Movimentação (toneladas ou outra unidade)",
       y = "Densidade") +
  theme_minimal()

#CARNES 
library(tidyverse)

###Função para calcular binwidth pela regra de Sturges
sturges_binwidth <- function(x) {
  n <- length(x)
  range_x <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  k <- ceiling(log2(n) + 1)
  binwidth <- range_x / k
  return(binwidth)
}

## 201 - Carne Bovina congelada 
carne_201 <- dados_wide %>%
  select(ano, semestre, periodo, `201`) %>%
  rename(movimentacao = `201`) %>%
  filter(!is.na(movimentacao))

ggplot(carne_201, aes(x = movimentacao)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = sturges_binwidth(carne_201$movimentacao),
                 fill = "firebrick", color = "white", alpha = 0.8) +
  facet_wrap(~periodo) +
  geom_density(color = "black") +
  labs(title = "Movimentação de Carne Bovina (201)",
       x = "Movimentação", y = "Densidade") +
  theme_minimal() +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## 202 - Carne bovina fresca
carne_202 <- dados_wide %>%
  select(ano, semestre, periodo, `202`) %>%
  rename(movimentacao = `202`) %>%
  filter(!is.na(movimentacao))

ggplot(carne_202, aes(x = movimentacao)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = sturges_binwidth(carne_202$movimentacao),
                 fill = "mediumvioletred", color = "white", alpha = 0.8) +
  facet_wrap(~periodo) +
  geom_density(color = "black") +
  labs(title = "Movimentação de Carne Suína (202)",
       x = "Movimentação", y = "Densidade") +
  theme_minimal()


## 203 - Carne suína
carne_203 <- dados_wide %>%
  select(ano, semestre, periodo, `203`) %>%
  rename(movimentacao = `203`) %>%
  filter(!is.na(movimentacao))

ggplot(carne_203, aes(x = movimentacao)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = sturges_binwidth(carne_203$movimentacao),
                 fill = "darkorange", color = "white", alpha = 0.8) +
  facet_wrap(~periodo) +
  geom_density(color = "black") +
  labs(title = "Movimentação de Miúdos / Outras Carnes (203)",
       x = "Movimentação", y = "Densidade") +
  theme_minimal()


#Utilizando QQ-plot para entender se as variáveis possuem distribuição próxima da distribuição normal 

variaveis <- c("1201", "201", "202", "203")

for (var in variaveis) {
  
  df_pre <- dados_wide %>%
    filter(periodo == "Pré-2017") %>%
    select(all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  df_pos <- dados_wide %>%
    filter(periodo == "Pós-2017") %>%
    select(all_of(var)) %>%
    filter(!is.na(.data[[var]]))
  
  plot_pre <- ggqqplot(df_pre[[var]],
                       title = paste("Q-Q Plot de", var, "- Pré-2017"),
                       xlab = "Quantis teóricos",
                       ylab = "Quantis amostrais")
  
  plot_pos <- ggqqplot(df_pos[[var]],
                       title = paste("Q-Q Plot de", var, "- Pós-2017"),
                       xlab = "Quantis teóricos",
                       ylab = "Quantis amostrais")
  
  print(ggarrange(plot_pre, plot_pos, ncol = 2))
  
  readline(prompt = "Pressione Enter para continuar para o próximo par de gráficos...")
}


# Teste de normalidade: Shapiro

### Criar data frame vazio pra guardar os resultados

resultados_shapiro <- data.frame(
  variavel = character(),
  periodo = character(),
  n = numeric(),
  W = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

### Loop pra preencher a tabela
for (var in variaveis) {
  for (p in unique(dados_wide$periodo)) {
    
    dados_filtrados <- subset(dados_wide, periodo == p)
    valores <- na.omit(dados_filtrados[[var]])
    
    if (length(valores) >= 3 & length(valores) <= 5000) {
      teste <- shapiro.test(valores)
      resultados_shapiro <- rbind(resultados_shapiro, data.frame(
        variavel = var,
        periodo = p,
        n = length(valores),
        W = round(teste$statistic, 5),
        p_value = round(teste$p.value, 5)
      ))
    } else {
      resultados_shapiro <- rbind(resultados_shapiro, data.frame(
        variavel = var,
        periodo = p,
        n = length(valores),
        W = NA,
        p_value = NA
      ))
    }
  }
}

### Visualizar a tabela final
print(resultados_shapiro)

#Completude dos dados 
colSums(is.na(dados_wide))

## Criando vazios para depois fazer o MICE 

dados_simulados <- dados_wide
set.seed(123)  
n <- nrow(dados_wide)

### Inserir 7% de NA em "1201"
idx_1201 <- sample(1:n, size = floor(n * 0.07))
dados_wide$`1201`[idx_1201] <- NA

### Inserir 3% de NA em "202"
idx_202 <- sample(1:n, size = floor(n * 0.03))
dados_wide$`202`[idx_202] <- NA

colSums(is.na(dados_simulados))

## Utilizando o MICE 

install.packages("mice")
library(mice)

imputacao <- mice(dados_simulados, m = 5, method = 'pmm', seed = 500)

### Visualizar resultado imputado
dados_imputados <- complete(imputacao)

colSums(is.na(dados_imputados))



#SHINY

install.packages("markdown")
install.packages(c("shiny", "ggplot2", "readr", "dplyr", "colourpicker"))
install.packages("shinydashboard")
install.packages("maps")
install.packages("htmlwidgets")
install.packages("shinyWidgets")


library(tidyverse)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(colourpicker)
library(rvest)
library(data.table)
library(knitr)

### Lê o CSV
dados <- read_csv("dados_wide.csv")

dados <- dados %>%
  arrange(ano, semestre) %>%
  mutate(ano_semestre = factor(ano_semestre, levels = unique(ano_semestre)))

variaveis_disponiveis <- c("1201", "201", "202", "203")
portos_disponiveis <- sort(unique(dados$complexo_portuario))
anos_disponiveis <- sort(unique(dados$ano))

## --- UI ---
ui <- fluidPage(
  titlePanel("📊 Dashboard Interativo - Movimentação Portuária"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput("variavel", "Escolha a variável:",
                  choices = variaveis_disponiveis, selected = "1201"),
      
      colourInput("cor_linha", "Escolha a cor da linha:", value = "#1F77B4"),
      
      sliderInput("ano_range", "Selecione o intervalo de anos:",
                  min = min(anos_disponiveis),
                  max = max(anos_disponiveis),
                  value = c(min(anos_disponiveis), max(anos_disponiveis)),
                  sep = ""),
      
      uiOutput("slider_y"),
      
      pickerInput("porto", "Filtrar por porto:",
                  choices = portos_disponiveis,
                  selected = portos_disponiveis,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE))
    ),
    
    mainPanel(
      plotOutput("grafico_linha"),
      textOutput("mensagem_vazia")
    )
  )
)

## --- SERVER ---
server <- function(input, output, session) {
  
 
  output$slider_y <- renderUI({
    dados_filtrados <- dados %>%
      filter(complexo_portuario %in% input$porto,
             ano >= input$ano_range[1],
             ano <= input$ano_range[2])
    
    valores <- dados_filtrados[[input$variavel]]
    max_val <- max(valores, na.rm = TRUE)
    
    sliderInput("y_limites", "Limites do eixo Y (toneladas):",
                min = 0,
                max = max_val,
                value = c(0, max_val),
                step = max_val / 20)
  })
  
  output$grafico_linha <- renderPlot({
    # Filtro por porto e ano
    dados_filtrados <- dados %>%
      filter(complexo_portuario %in% input$porto,
             ano >= input$ano_range[1],
             ano <= input$ano_range[2]) %>%
      select(ano_semestre, valor = !!sym(input$variavel)) %>%
      group_by(ano_semestre) %>%
      summarise(valor = sum(valor, na.rm = TRUE)) %>%
      filter(valor > 0)
    
    if (nrow(dados_filtrados) == 0) return(NULL)
    
    ggplot(dados_filtrados, aes(x = ano_semestre, y = valor, group = 1)) +
      geom_line(color = input$cor_linha, size = 1.3) +
      scale_y_continuous(
        limits = input$y_limites,
        labels = comma_format(big.mark = ".", decimal.mark = ",")
      ) +
      labs(
        title = paste("Movimentação nacional (toneladas) - Variável", input$variavel),
        x = "Ano e Semestre",
        y = "Movimentação (toneladas)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$mensagem_vazia <- renderText({
    dados_filtrados <- dados %>%
      filter(complexo_portuario %in% input$porto,
             ano >= input$ano_range[1],
             ano <= input$ano_range[2]) %>%
      select(ano_semestre, valor = !!sym(input$variavel)) %>%
      group_by(ano_semestre) %>%
      summarise(valor = sum(valor, na.rm = TRUE))
    
    if (nrow(dados_filtrados) == 0 || all(dados_filtrados$valor == 0)) {
      "⚠️ Nenhum dado disponível para os filtros selecionados."
    } else {
      ""
    }
  })
}

## --- Roda o app ---
shinyApp(ui = ui, server = server)


