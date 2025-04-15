#### Trabalho de Visualização de Dados

## Análise da empresa fornecedora de energia elétrica EDP-SP

# Importando pacotes

library(tidyverse)
library(caret)
library(epiDisplay)
library(lubridate)

# Importando base
setwd("C:/Users/Michel/Documents/Michel/ESTATISTICA/12periodo/Trabalho")

dados = read_csv2("C:/Users/Bia-P/Downloads/Grupo7_EDP-SP.csv")
dados = read_csv2("Grupo7_EDP-SP.csv")

## Análise exploratória dos dados

# Tamanho da base

dim(dados)

# Tipos de dados

str(dados)

## Verificando a variação das variáveis 

nearZeroVar(dados,saveMetrics = T)  # variáveis sem variação: DatGeracaoConjuntoDados
                                                            # NumCNPJAgenteDistribuidora   
#help("nearZeroVar")                                         # SigAgenteDistribuidora
                                                            # NomAgenteDistribuidora
#View(dados)                                                # DscClassificacao

table(dados$DscDetalheMercado)

#View(dados1 |> 
 #      group_by(DscDetalheMercado))

# Análise de variáveis com pouca variabilidade

unique(dados$DscDetalheConsumidor)
tab1(dados$DscDetalheConsumidor)

unique(dados$IdeAgenteAcessante)
tab1(dados$IdeAgenteAcessante)

unique(dados$NomAgenteAcessante)
table(dados$NomAgenteAcessante)

# Inicialmente só as variáveis sem variação nenhuma serão retiradas.
# As com pouca variação serão mantidas.

dados1 = dados |> 
  dplyr::select(-c(DatGeracaoConjuntoDados,NumCNPJAgenteDistribuidora,
            SigAgenteDistribuidora,NomAgenteDistribuidora,
            DscClassificacao))

dim(dados1)


dados2 = dados1 |> 
  dplyr::select(everything()) |> 
  group_by(DatCompetencia,DscSubClasseConsumidor,DscClasseConsumoMercado,DscSubGrupoTarifario,DscModalidadeTarifaria,NomTipoMercado) |> 
  summarise(n = n())

#View(dados2)


# Realizando a divisão entre os dados a partir
# das classes da variável DscDetalhaMercado

# Classes da variável
tab1(dados1$DscDetalheMercado)

# Classes que informam sobre:  - Consumo de Energia:  .Demanda Contratada kW 
                                                    # .Demanda Faturada (kW)
                                                    # .Energia compensada (kWh)
                                                    # .Energia Consumida (kWh)
                                                    # .Energia debitada do SCEE (kWh)           
                                                    # .Energia Injetada (kWh)                   
                                                    # .Energia TE (kWh)                        
                                                    # .Energia TUSD (kWh)
                                                    # .PROINFA (kWh)
                                                    # .Ultrapassagem Demanda (kW) 



                            #  - Consumidores:   .Número de consumidores 



                            # - Valor Monetário: .COFINS (R$)
                            #                    .ICMS (R$) 
                            #                    .PIS/COFINS (R$)                          
                            #                    .PIS/PASEP (R$)                         
                            #                    .PROINFA (kWh)                             
                            #                    .Receita Bandeiras (R$)                  
                            #                    .Receita Demanda (R$)                   
                            #                    .Receita Energia (R$)                    
                            #                    .Receita energia compensada (R$)          
                            #                    .Receita Ultrapassagem Demanda (R$)



                            # - Demais classes:  .Desconto de energia compensada %         
                            #                    .Desconto Demanda %                       
                            #                    .Desconto Energia TE %                    
                            #                    .Desconto Energia TUSD % 

                           


# Filtrando bases em relação aos tipos de dados em estudo

# Base para número de consumidores

# Existem duas variáveis indicando o número de consumidores, porém
# com nomes diferentes (Consumidores x consumidores)

# Alterando o nome:

dados1 = dados1 |> 
  mutate(DscDetalheMercado = if_else(dados1$DscDetalheMercado == "Número de consumidores",
                                     "Número de Consumidores",dados1$DscDetalheMercado)
  ) 

tab1(dados1$DscDetalheMercado)

# Base criada
base_consumidores = dados1 |> 
  filter(DscDetalheMercado == "Número de Consumidores")

#View(base_consumidores)
head(base_consumidores)



# Base valor monetário:
base_valor_monetario = dados1 |> 
                       filter(DscDetalheMercado %in% c("COFINS (R$)"
                                                      ,"ICMS (R$)" 
                                                      ,"PIS/COFINS (R$)"                          
                                                      ,"PIS/PASEP (R$)"                         
                                                      ,"PROINFA (kWh)"                             
                                                      ,"Receita Bandeiras (R$)"                  
                                                      ,"Receita Demanda (R$)"                   
                                                      ,"Receita Energia (R$)"                    
                                                      ,"Receita energia compensada (R$)"          
                                                      ,"Receita Ultrapassagem Demanda (R$)"))

#View(base_valor_monetario)




# Base consumo kw:
base_consumo = dados1 |> 
  filter(DscDetalheMercado %in% c("Demanda Contratada kW" 
                                  ,"Demanda Faturada (kW)"
                                  ,"Energia compensada (kWh)"
                                  ,"Energia Consumida (kWh)"
                                  ,"Energia debitada do SCEE (kWh)"           
                                  ,"Energia Injetada (kWh)"                   
                                  ,"Energia TE (kWh)"                        
                                  ,"Energia TUSD (kWh)"
                                  ,"PROINFA (kWh)"
                                  ,"Ultrapassagem Demanda (kW)" ))

#View(base_consumo)


#### Análise descritiva das variáveis das bases

## Base consumo

tab1(base_consumo$NomTipoMercado)
tab1(base_consumo$DscModalidadeTarifaria)
tab1(base_consumo$DscSubGrupoTarifario) #https://www.gov.br/aneel/pt-br/assuntos/tarifas/entenda-a-tarifa/modalidades-tarifarias
tab1(base_consumo$DscClasseConsumoMercado)  # DscGrupoTarifario e DscClasseConsumoMercado dizem quase a mesma coisa
                                            # mais aconselhado usar a DscClasseConsumoMercado por ser mais específica

tab1(base_consumo$DscSubClasseConsumidor) # subclasse
tab1(base_consumo$DscDetalheConsumidor)

tab1(base_consumo$IdeAgenteAcessante) # parece ser uma variavel ruim

tab1(base_consumo$NumCNPJAgenteAcessante) # tambem nao parece ser uma boa variavel

tab1(base_consumo$DscPostoTarifario)

tab1(base_consumo$DscOpcaoEnergia)  # cativo: consumidor obrigado a consumir de empresa de energia
                                    # livre: que tem liberdade pra escolher de quem comprar energia
                                   # https://www.portalsolar.com.br/qual-a-diferenca-entre-consumidor-cativo-e-consumidor-livre-de-energia

tab1(base_consumo$DscDetalheMercado)

## Base consumidor

# Visualizando gráfico dos consumidores ao longo do tempo

# Criando variáveis para receber ano e mês das datas,
# além de ordenar a base pela variável data

base_consumidores = base_consumidores |> 
  mutate(ano = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%Y"),
         mes = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%m")) |> 
  arrange(DatCompetencia)

#View(base_consumidores)

# Criando variável para receber a soma dos números de consumidores por mês

base_soma_consumidores = base_consumidores |> 
  dplyr::select(c(DatCompetencia,ano,mes,VlrMercado)) |> 
  group_by(ano,mes) |> 
  summarise(soma_consumidores = sum(VlrMercado))

head(base_soma_consumidores,20)

# Criando variável data novamente nessa nova base

base_soma_consumidores$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
str(base_soma_consumidores$Data)            # está no formato character
base_soma_consumidores$Data = as.Date(base_soma_consumidores$Data,     # passando para o formato date
                                      format = "%Y-%m-%d")


# Passando datas originais para a nova base

for(i in 1:nrow(base_consumidores)){
  for(j in 1:nrow(base_soma_consumidores)) {
      if(base_soma_consumidores$Data[j] == "1980-01-01"  && base_soma_consumidores$ano[j] == base_consumidores$ano[i] && base_soma_consumidores$mes[j] == base_consumidores$mes[i]){
          base_soma_consumidores$Data[j] = base_consumidores$DatCompetencia[i]
      }
  }
}

base_consumidores$DatCompetencia


head(base_soma_consumidores,20)


# Gráfico para verificar a variacao de consumidores ao longo do tempo

dev.off()
base_soma_consumidores |> 
  ggplot(mapping = aes(x = Data, y = soma_consumidores/1000000))+
  geom_line(color = "blue",
            linewidth = 1.3)+
  scale_x_date(date_labels = "%b-%y",
               date_breaks = "3 month") +
  labs(x = "Data",
       y = "Consumidores (em milhões)",
       title = "Número de Consumidores",
       subtitle = "ao longo de 2023 e 2024")+
  theme_bw() +
  theme(panel.border = element_blank())+            # tendência de crescimento dos consumidores
  # +geom_smooth(method='lm',se = FALSE,color = "red")
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


## Em relacao ao consumo
#View(base_valor_monetario)

unique(base_valor_monetario$DscDetalheMercado)

base_valor_monetario$DatCompetencia = as.Date(base_valor_monetario$DatCompetencia,     # passando para o formato date
                                      format = "%Y-%m-%d")






## Em relação à receita
#View(base_valor_monetario |> 
 #      filter(DscDetalheMercado == "Receita Energia (R$)")) 

# Criando variáveis para receber ano e mes das datas,
# além de ordenar a base pela variável data

base_valor_monetario = base_valor_monetario |> 
  mutate(ano = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%Y"),
         mes = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%m")) |> 
  arrange(DatCompetencia)

#View(base_consumidores)



# Criando variável para receber a soma dos números de consumidores por mês

base_soma_valores = base_valor_monetario |> 
  filter(DscDetalheMercado == "Receita Energia (R$)") |> 
  dplyr::select(c(DatCompetencia,ano,mes,VlrMercado)) |> 
  group_by(ano,mes) |> 
  summarise(soma_receita = sum(VlrMercado))

head(base_soma_consumidores,20)

# Criando variável data novamente nessa nova base

base_soma_valores$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
str(base_soma_valores$Data)            # está no formato character
base_soma_valores$Data = as.Date(base_soma_valores$Data,     # passando para o formato date
                                      format = "%Y-%m-%d")


# Passando datas originais para a nova base

for(i in 1:nrow(base_valor_monetario)){
  for(j in 1:nrow(base_soma_valores)) {
    if(base_soma_valores$Data[j] == "1980-01-01"  && base_soma_valores$ano[j] == base_valor_monetario$ano[i] && base_soma_valores$mes[j] == base_valor_monetario$mes[i]){
      base_soma_valores$Data[j] = base_valor_monetario$DatCompetencia[i]
    }
  }
}

base_valor_monetario$DatCompetencia


head(base_soma_valores,20)


# Gráfico para a receita Mensal


base_soma_valores |> 
  ggplot(mapping = aes(x = Data, y = soma_receita/1000000))+
  geom_line(color = "blue",
            linewidth = 1.3)+
  scale_x_date(date_labels = "%b-%y",
               date_breaks = "3 month") +
  labs(x = "Data",
       y = "Receita (em milhões de R$) ",
       title = "Receita Mensal",
       subtitle = "ao longo de 2023 e 2024")+
  theme_bw() +
  theme(panel.border = element_blank()) +           
 geom_smooth(method='lm',se = FALSE,color = "red",size = 1.3)+
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# Gráfico para a Receita Total
library(scales)
base_soma_valores |> 
  group_by(ano) |> 
  summarise(soma_anual = sum(soma_receita)) |> 
  ggplot(aes(x = ano, fill = as.factor(ano), y = soma_anual / 1000000000)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Ano",
    y = "Total (em bilhões)",
    title = "Receita Total",
    subtitle = "em 2023 e 2024"
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c("2023" = "#1f77b4", "2024" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = label_dollar()(round(soma_anual / 1000000000, 3))), 
            size = 4, 
            position = position_stack(vjust = 0.95))





# Criando variável para receber a soma dos números de consumidores por mês

base_soma_valores1 = base_valor_monetario |> 
  filter(DscDetalheMercado == "Receita Energia (R$)") |> 
  dplyr::select(c(DatCompetencia,ano,mes,VlrMercado,DscClasseConsumoMercado)) |> 
  group_by(ano,mes,DscClasseConsumoMercado) |> 
  summarise(soma_receita = sum(VlrMercado))

head(base_soma_valores1,20)

base_soma_valores2 = base_soma_valores1 |> 
  group_by(DscClasseConsumoMercado) |> 
  summarise(soma_receita1 = sum(soma_receita))

base_soma_valores2

# # Criando variável data novamente nessa nova base
# 
# base_soma_valores$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
# str(base_soma_valores$Data)            # está no formato character
# base_soma_valores$Data = as.Date(base_soma_valores$Data,     # passando para o formato date
#                                  format = "%Y-%m-%d")
# 
# 
# # Passando datas originais para a nova base
# 
# for(i in 1:nrow(base_valor_monetario)){
#   for(j in 1:nrow(base_soma_valores)) {
#     if(base_soma_valores$Data[j] == "1980-01-01"  && base_soma_valores$ano[j] == base_valor_monetario$ano[i] && base_soma_valores$mes[j] == base_valor_monetario$mes[i]){
#       base_soma_valores$Data[j] = base_valor_monetario$DatCompetencia[i]
#     }
#   }
# }
# 
# base_valor_monetario$DatCompetencia
# 
# 
# head(base_soma_valores,20)
# 
# 



# Gráfico para a Receita Total por classe


library(scales)
base_soma_valores2 |> 
   
  ggplot(aes(x = reorder(DscClasseConsumoMercado,-soma_receita1), y = soma_receita1 / 1000000000)) +
  geom_bar(stat = "identity",
           fill = "darkblue") +
  labs(
    x = "Classes de Consumo",
    y = "Receita (em bilhões)",
    title = "Receita Total",
    subtitle = "por classe de consumo em 2023 e 2024"
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c("2023" = "#1f77b4", "2024" = "#ff7f0e")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = label_dollar(prefix = "R$")(round(soma_receita1 / 1000000000, 3))), 
            size = 4, 
            #position = position_stack(vjust = 0.8),
            #position = position_dodge(.9),
            inherit.aes = TRUE,vjust = -1)+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.9,
                                   hjust = 1,
                                   size = 10))+
  theme(axis.title.x = element_blank())
  # theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
   #                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




  
unique(base_valor_monetario$DscDetalheMercado)

library(dplyr)

# Adicionando a receita proporcional de novembro e dezembro
base_soma_valores_ajustada <- base_soma_valores %>%
  group_by(ano) %>%
  mutate(
    meses_disponiveis = if_else(ano == 2024, n_distinct(mes), 12), # Conta os meses disponíveis
    receita_total_projetada = if_else(
      ano == 2024,
      sum(soma_receita) * (12 / meses_disponiveis), # Ajusta proporcionalmente para 12 meses
      sum(soma_receita) # Mantém o valor original para 2023
    )
  ) %>%
  ungroup()

# Resumindo os dados por ano
dados_anuais <- base_soma_valores_ajustada %>%
  group_by(ano) %>%
  summarise(soma_anual = unique(receita_total_projetada))

# Criando o gráfico ajustado
library(ggplot2)
library(scales)

dados_anuais %>%
  ggplot(aes(x = ano, fill = as.factor(ano), y = soma_anual / 1e9)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Ano",
    y = "Total (em bilhões)",
    title = "Receita Total",
    subtitle = " para 2023 e 2024"
  ) +
  scale_y_continuous(labels = label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c("2023" = "#1f77b4", "2024" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  geom_text(aes(label = label_dollar(prefix = "R$")(round(soma_anual / 1e9, 3))), 
            size = 4, 
            position = position_stack(vjust = 0.95))

#View(base_soma_valores)



####### obtendo valores para novembro e dezembro com aprendizado de máquinas
# 
# base_soma_valores
# 
# 
# 
# View(base_valor_monetario)
# 
# 
# 
# 
# a = "2023-01-01"
# a = lubridate::ymd(a)
# a + 365
# 
# 
# # retirando variáveis ruins para o modelo
# library(caret)
# library(randomForest)
# 
# 
# 
# 
# # verificando NAs
# sum(is.na(base_valor_monetario$VlrMercado))
# 
# library(naniar)
# naniar::gg_miss_var(base_valor_monetario)
# 
# # retirando variavel com muitos NAs
# base_valor_monetario111 = base_valor_monetario |> 
#   dplyr::select(-c(NumCNPJAgenteAcessante,NomAgenteAcessante,IdeAgenteAcessante))
# 
# 
# # Importancia com permutacao (mais confiÃ¡vel)
# rf <- randomForest(VlrMercado~.,data = base_valor_monetario111, importance=T,)
# #imp <- importance(rf, type=1, scale = F) # permutation importances
# 
# varImpPlot(rf,type = 1,scale = F)
# 
# 
# # verificando meses presentes na base
# unique(base_valor_monetario$DatCompetencia)
# 
# # criando base com valores a serem previstos para novembro e dezembro de 2024
# base_valor_monetario_previsao = base_valor_monetario |> 
#   mutate(DatCompetencia = as.character(DatCompetencia)) |> 
#   filter(DatCompetencia %in% c("2023-11-01","2023-12-01")) |> 
#   mutate(DatCompetencia = ymd(DatCompetencia))
#   
# 
# 
# # divisao da base em treino e teste
# dim(base_valor_monetario111)[1]*0.8
# 
# base_treino = base_valor_monetario111[1:11453,]
# base_teste = base_valor_monetario111[11454:14317,]
# 
# ### 
# 
# 
# # realizando o cálculo para cada classe
# 
# #[1] "COFINS (R$)"       "Receita Energia (R$)"        "Receita Demanda (R$)"              
# #[4] "PIS/PASEP (R$)"       "ICMS (R$)"               "Receita Ultrapassagem Demanda (R$)"
# #[7] "Receita Bandeiras (R$)"       "PIS/COFINS (R$)"            "PROINFA (kWh)"                     
# #[10] "Receita energia compensada (R$)" 
# 
# base_treino_cofins = base_treino |> 
#   filter(DscDetalheMercado == "COFINS (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_cofins = base_teste |> 
#   filter(DscDetalheMercado == "COFINS (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# View(base_treino_cofins)
# modelo_lm_cofins = caret::train(VlrMercado~.,
#                             #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                             #DscClasseConsumoMercado+
#                             #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                           data = base_treino_cofins,
#                           method = "lm",
#                           trControl = ctrl,
#                           #tuneLenght = 5
#                           
# )
# 
# modelo_lm_cofins
# 
# previsao_cofins <- predict(modelo_svm, base_teste)
# 
# previsao_cofins
# 
# 
# ###
# base_treino_receita_e = base_treino |> 
#   filter(DscDetalheMercado == "Receita Energia (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_receita_e = base_teste |> 
#   filter(DscDetalheMercado == "Receita Energia (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# View(base_treino_receita_e)
# modelo_lm_receita_e = caret::train(VlrMercado~.,
#                                 #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                 #DscClasseConsumoMercado+
#                                 #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                 data = base_treino_receita_e,
#                                 method = "lm",
#                                 trControl = ctrl,
#                                 #tuneLenght = 5
#                                 
# )
# 
# modelo_lm_receita_e
# 
# previsao_receita_e <- predict(modelo_svm, base_teste)
# 
# previsao_receita_e
# 
# 
# ####
# 
# #   "Receita Demanda (R$)"              
# #[4] "PIS/PASEP (R$)"       "ICMS (R$)"               "Receita Ultrapassagem Demanda (R$)"
# #[7] "Receita Bandeiras (R$)"       "PIS/COFINS (R$)"            "PROINFA (kWh)"                     
# #[10] "Receita energia compensada (R$)" 
# 
# 
# #"Receita Demanda (R$)"
# 
# 
# base_treino_receita_d = base_treino |> 
#   filter(DscDetalheMercado == "Receita Demanda (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_receita_d = base_teste |> 
#   filter(DscDetalheMercado == "Receita Demanda (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_receita_d)
# modelo_lm_receita_d = caret::train(VlrMercado~.,
#                                    #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                    #DscClasseConsumoMercado+
#                                    #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                    data = base_treino_receita_d,
#                                    method = "lm",
#                                    trControl = ctrl,
#                                    #tuneLenght = 5
#                                    
# )
# 
# modelo_lm_receita_d
# 
# previsao_receita_d <- predict(modelo_svm, base_teste)
# 
# previsao_receita_d
# 
# 
# 
# 
# 
# #"PIS/PASEP (R$)"
# 
# 
# 
# base_treino_pis = base_treino |> 
#   filter(DscDetalheMercado == "PIS/PASEP (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_pis = base_teste |> 
#   filter(DscDetalheMercado == "PIS/PASEP (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_pis)
# modelo_lm_pis = caret::train(VlrMercado~.,
#                                    #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                    #DscClasseConsumoMercado+
#                                    #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                    data = base_treino_pis,
#                                    method = "lm",
#                                    trControl = ctrl,
#                                    #tuneLenght = 5
#                                    
# )
# 
# modelo_lm_pis
# 
# previsao_pis <- predict(modelo_svm, base_teste)
# 
# previsao_pis
# 
# 
# #"ICMS (R$)" 
# 
# 
# 
# base_treino_icms = base_treino |> 
#   filter(DscDetalheMercado =="ICMS (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_icms = base_teste |> 
#   filter(DscDetalheMercado == "ICMS (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_icms)
# modelo_lm_icms = caret::train(VlrMercado~.,
#                              #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                              #DscClasseConsumoMercado+
#                              #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                              data = base_treino_icms,
#                              method = "lm",
#                              trControl = ctrl,
#                              #tuneLenght = 5
#                              
# )
# 
# modelo_lm_icms
# 
# previsao_icms <- predict(modelo_svm, base_teste)
# 
# previsao_icms
# 
# #             "Receita Ultrapassagem Demanda (R$)"
# #[7] "Receita Bandeiras (R$)"       "PIS/COFINS (R$)"            "PROINFA (kWh)"                     
# #[10] "Receita energia compensada (R$)" 
# 
# 
# #"Receita Ultrapassagem Demanda (R$)"
# base_treino_demanda_u = base_treino |> 
#   filter(DscDetalheMercado =="Receita Ultrapassagem Demanda (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_demanda_u = base_teste |> 
#   filter(DscDetalheMercado == "Receita Ultrapassagem Demanda (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado))#,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_demanda_u)
# modelo_lm_demanda_u = caret::train(VlrMercado~.,
#                               #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                               #DscClasseConsumoMercado+
#                               #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                               data = base_treino_demanda_u,
#                               method = "lm",
#                               trControl = ctrl,
#                               #tuneLenght = 5
#                               
# )
# 
# modelo_lm_demanda_u
# 
# previsao_demanda_u <- predict(modelo_svm, base_teste)
# 
# previsao_demanda_u
# 
# 
# 
# 
# #"Receita Bandeiras (R$)"
# 
# 
# 
# base_treino_receita_b = base_treino |> 
#   filter(DscDetalheMercado =="Receita Bandeiras (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_receita_b = base_teste |> 
#   filter(DscDetalheMercado == "Receita Bandeiras (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_receita_b)
# modelo_lm_receita_b = caret::train(VlrMercado~.,
#                                    #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                    #DscClasseConsumoMercado+
#                                    #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                    data = base_treino_receita_b,
#                                    method = "lm",
#                                    trControl = ctrl,
#                                    #tuneLenght = 5
#                                    
# )
# 
# modelo_lm_receita_b
# 
# previsao_receita_b <- predict(modelo_svm, base_teste)
# 
# previsao_receita_b
# 
# 
# 
# #   "PIS/COFINS (R$)"            "PROINFA (kWh)"                     
# #[10] "Receita energia compensada (R$)" 
# 
# 
# #"PIS/COFINS (R$)"
# base_treino_pis_cofins = base_treino |> 
#   filter(DscDetalheMercado =="PIS/COFINS (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_pis_cofins = base_teste |> 
#   filter(DscDetalheMercado == "PIS/COFINS (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_pis_cofins)
# modelo_lm_pis_cofins = caret::train(VlrMercado~.,
#                                    #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                    #DscClasseConsumoMercado+
#                                    #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                    data = base_treino_pis_cofins,
#                                    method = "lm",
#                                    trControl = ctrl,
#                                    #tuneLenght = 5
#                                    
# )
# 
# modelo_lm_pis_cofins
# 
# previsao_pis_cofins <- predict(modelo_svm, base_teste)
# 
# previsao_pis_cofins
# 
# 
# 
# #"Receita energia compensada (R$)"
# 
# 
# base_treino_receita_c = base_treino |> 
#   filter(DscDetalheMercado =="Receita energia compensada (R$)" ) |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# base_teste_receita_c = base_teste |> 
#   filter(DscDetalheMercado =="Receita energia compensada (R$)") |> 
#   dplyr::select(-c(DscDetalheMercado,DscOpcaoEnergia,DscPostoTarifario,DscDetalheConsumidor))
# 
# set.seed(1000)
# ctrl = trainControl(method = "repeatedcv",number = 10,
#                     repeats = 3)
# 
# #View(base_treino_receita_c)
# modelo_lm_receita_c = caret::train(VlrMercado~.,
#                                     #NomTipoMercado+mes+ano+DscSubClasseConsumidor+
#                                     #DscClasseConsumoMercado+
#                                     #DscSubGrupoTarifario+DscModalidadeTarifaria+ano,
#                                     data = base_treino_receita_c,
#                                     method = "lm",
#                                     trControl = ctrl,
#                                     #tuneLenght = 5
#                                     
# )
# 
# modelo_lm_receita_c
# 
# previsao_receita_c <- predict(modelo_svm, base_teste)
# 
# previsao_receita_c
# 
# 
# 
# 


















base_valor_monetario$DatCompetencia = as.Date(base_valor_monetario$DatCompetencia,     # passando para o formato date
                                              format = "%Y-%m-%d")






## Em relação à receita
#View(base_valor_monetario |> 
#      filter(DscDetalheMercado == "Receita Energia (R$)")) 

# Criando variáveis para receber ano e mes das datas,
# além de ordenar a base pela variável data

base_valor_monetario = base_valor_monetario |> 
  mutate(ano = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%Y"),
         mes = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%m")) |> 
  arrange(DatCompetencia)

#View(base_consumidores)



# Criando variável para receber a soma dos números de consumidores por mês

base_soma_valores = base_valor_monetario |> 
  filter(DscDetalheMercado == "Receita Energia (R$)") |> 
  dplyr::select(c(DatCompetencia,ano,mes,VlrMercado)) |> 
  group_by(ano,mes) |> 
  summarise(soma_receita = sum(VlrMercado))

head(base_soma_consumidores,20)

# Criando variável data novamente nessa nova base

base_soma_valores$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
str(base_soma_valores$Data)            # está no formato character
base_soma_valores$Data = as.Date(base_soma_valores$Data,     # passando para o formato date
                                 format = "%Y-%m-%d")


# Passando datas originais para a nova base

for(i in 1:nrow(base_valor_monetario)){
  for(j in 1:nrow(base_soma_valores)) {
    if(base_soma_valores$Data[j] == "1980-01-01"  && base_soma_valores$ano[j] == base_valor_monetario$ano[i] && base_soma_valores$mes[j] == base_valor_monetario$mes[i]){
      base_soma_valores$Data[j] = base_valor_monetario$DatCompetencia[i]
    }
  }
}

base_valor_monetario$DatCompetencia


head(base_soma_valores,20)










unique(base_valor_monetario$DscDetalheMercado)

#### Verificando subgrupos tarifários

table(base_valor_monetario$DscDetalheMercado)

base_valor_monetario10 =(base_valor_monetario |> 
        filter(DscDetalheMercado == "ICMS (R$)")) 

# base_valor_monetario10 =(base_valor_monetario |> 
#                            filter(DscDetalheMercado == "Receita Energia (R$)")) 

#View(base_valor_monetario10)

base_valor_monetario10

table(base_valor_monetario10$DscSubGrupoTarifario)

# A1 e A2 -  Alta tensao: industrias, hospitais, estabelecimentos comerciais de grande porte
# A3ae A4 -  Medio tensao:  industrias, hospitais, estabelecimentos comerciais de medio porte
# B - residenciais, pequenos comércios, escritórios, prédios de apartamentos, pequenas indústrias, entre outros

# criando nova variável com os níveis de tensão, além de nova base
base_valor_monetario10 = base_valor_monetario10 |> 
  mutate(Tensão = if_else(DscSubGrupoTarifario == "A1","Alta",
                          
                          if_else(DscSubGrupoTarifario == "A3a","Média",
                                  if_else(DscSubGrupoTarifario == "A2","Alta",
                                  if_else(DscSubGrupoTarifario == "A4","Média","Baixa")))))

base_valor_monetario_count = base_valor_monetario10 |> 
  dplyr::select(DscSubGrupoTarifario,Tensão) |> 
  group_by(DscSubGrupoTarifario,Tensão) |> 
  summarise(n = n())
base_valor_monetario_count


# tabela em relação à tensão
table(base_valor_monetario10$Tensão)

library("wesanderson")


# Gráfico para os grupos tarifários
base_valor_monetario_count |> 
  ggplot(aes(x = reorder(DscSubGrupoTarifario,-n),y = n,fill = Tensão))+
  geom_bar(stat = "identity")+
  labs(title = "Subgrupos Tarifários",
       y = "Frequência",
       x = "Subgrupos")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_brewer(palette="Set1")


# classe consumo mercado
base_valor_monetario_count2 = base_valor_monetario10 |> 
   
  group_by(DscClasseConsumoMercado) |> 
  summarise(n = n())
base_valor_monetario_count2

# Gráfico para as classes de mercado
base_valor_monetario_count2 |> 
  ggplot(aes(x = reorder(DscClasseConsumoMercado,-n),y = n,
             fill = DscClasseConsumoMercado))+
  geom_bar(stat = "identity",position="stack",
           color = "darkblue",fill = "darkblue")+
  labs(title = "Classes de Consumo do Mercado",
       y = "Frequência",
       x = "Classes")+
  # theme_bw() +
  # theme(panel.border = element_blank())+
  # theme(axis.text.x = element_text(angle = 45,
  #                                  vjust = 0.9,
  #                                  hjust = 1))+
  # theme(legend.position="none")+
  # theme(panel.border = element_blank(),
  #       plot.title = element_text(hjust = 0.5),
  #       plot.subtitle = element_text(hjust = 0.5))
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 0.9,
                                   hjust = 1,
                                   size = 10))+
  theme(axis.title.x = element_blank())
  


# criando variáveis para receber ano e mes das datas,
# além de ordenar a base pela variável data

base_consumo = base_consumo |> 
  mutate(ano = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%Y"),
         mes = format(as.Date(DatCompetencia,"%Y-%m-%d"),"%m")) |> 
  arrange(DatCompetencia)

#View(base_consumidores)

unique(base_consumo$DscDetalheMercado)

# criando variável para receber a soma dos números de consumidores por mes
base_soma_consumo = base_consumo |> 
  filter(DscDetalheMercado == "Energia Consumida (kWh)") |> 
  dplyr::select(c(DatCompetencia,ano,mes,VlrMercado)) |> 
  group_by(ano,mes) |> 
  summarise(media_consumo_energia_kwh = mean(VlrMercado))

head(base_soma_consumo,20)

# Criando variável data novamente nessa nova base

base_soma_consumo$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
str(base_soma_consumo$Data)            # está no formato character
base_soma_consumo$Data = as.Date(base_soma_consumo$Data,     # passando para o formato date
                                 format = "%Y-%m-%d")


# Passando datas originais para a nova base

for(i in 1:nrow(base_consumo)){
  for(j in 1:nrow(base_soma_consumo)) {
    if(base_soma_consumo$Data[j] == "1980-01-01"  && base_soma_consumo$ano[j] == base_consumo$ano[i] && base_soma_consumo$mes[j] == base_consumo$mes[i]){
      base_soma_consumo$Data[j] = base_consumo$DatCompetencia[i]
    }
  }
}

base_consumo$DatCompetencia


head(base_soma_consumo,20)


# Gráfico para a média da energia consumida
base_soma_consumo |> 
  ggplot(mapping = aes(x = Data, y = media_consumo_energia_kwh))+
  geom_line(color = "blue",
            linewidth = 1.5)+
  scale_x_date(date_labels = "%b-%y",
               date_breaks = "3 month") +
  labs(x = "Data",
       y = "Energia Consumida (kwh)",
       title = "Energia Média Consumida",
       subtitle = "ano longo de 2023 e 2024")+
  theme_bw() +
  theme(panel.border = element_blank()) +
  geom_smooth(method='lm',se = FALSE,color = "red",size = 2)+
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  

####### mapa com a área de atuacao em SP

library(sf)
municipios = c( "Aparecida",
                "Biritiba Mirim",
                "Caçapava",
                "Cachoeira Paulista",
                "Canas",
                "Caraguatatuba",
                "Cruzeiro",
                "Ferraz de Vasconcelos",
                "Guararema",
                "Guaratinguetá",
                "Guarulhos",
                "Itaquaquecetuba",
                "Jacareí",
                "Jambeiro",
                "Lorena",
                "Mogi das Cruzes",
                "Monteiro Lobato",
                "Pindamonhangaba",
                "Poá",
               "Potim",
                "Roseira",
                "Salesópolis",
                "Santa Branca",
                "São José dos Campos",
                "São Sebastião",
                "Suzano",
                "Taubaté",
                "Tremembé")

# obtendo o shapefile do estado de SP com os municipios
shape_sp = read_sf("C:/Users/Bia-P/Downloads/SP_Municipios_2023.shp") 
shape_sp = read_sf("SP_Municipios_2023.shp")

# gráfico de sp

ggplot(shape_sp) +
  geom_sf(fill = "white")

shape_sp$NM_MUN

# criando variável para 
# identificar cidades onde a EDP atua
# shape_sp1 = shape_sp |> 
#   mutate(area_edp = if_else(NM_MUN %in% municipios, "Sim","Não"))

shape_sp1 = shape_sp |> 
  mutate(area_edp = if_else(NM_MUN %in% municipios, "Sim",
                            if_else(NM_MUN == "São Paulo","Capital","Não")))

table(shape_sp1$area_edp)

# identificando capital são paulo


# Gráfico

shape_sp1 |> 
  ggplot(aes(fill = area_edp)) +
  geom_sf()+
  theme_light()

shape_sp1 = shape_sp1 |> 
  dplyr::select(-CD_MUN)


library(tmap)
tmap_mode("view")
tm_shape(shp = shape_sp1) + 
  tm_fill(col = "area_edp",
          title = "Área de Atuação da EDP",
          palette = c('red','lightyellow','blue'))+
  tm_borders(lwd = 0.75,
             lty = "solid")



### análise das residências em relação aos clientes de baixa renda

#View(base_consumidores)

table(base_consumidores$DscSubClasseConsumidor)


# Filtrando base de consumidores por clientes de baixa renda

base_consumidores_b_r = base_consumidores |> 
  filter(DscSubClasseConsumidor %in% c("Residencial baixa renda \u0096 faixa 01",
                                       "Residencial baixa renda \u0096 faixa 02",
                                       "Residencial baixa renda \u0096 faixa 03",
                                       "Residencial baixa renda \u0096 faixa 04"))

#View(base_consumidores_b_r)

# Alterando as classes da variável DscSubClasseConsumidor para 
# indicar apenas as classes a que se referem, já que a base 
# trata apenas de conjuntos residenciais

base_consumidores_b_r = base_consumidores_b_r |> 
  mutate(DscSubClasseConsumidor = if_else(DscSubClasseConsumidor == "Residencial baixa renda \u0096 faixa 01","Faixa 1",
                                          if_else(DscSubClasseConsumidor == "Residencial baixa renda \u0096 faixa 02","Faixa 2",
                                                  if_else(DscSubClasseConsumidor == "Residencial baixa renda \u0096 faixa 03","Faixa 3","Faixa 4"))))
  

# criando nova base agora com a soma dos clientes em cada faixa
base_consumidores_b_r2 = base_consumidores_b_r |> 
  dplyr::select(DscSubClasseConsumidor,VlrMercado,ano,mes,DatCompetencia) |> 
  group_by(DscSubClasseConsumidor,ano,mes) |> 
  summarise(soma_faixa = sum(VlrMercado))


#  criando variável data novamente nessa nova base

base_consumidores_b_r2$Data = "1980-01-01"  # atribuindo uma data qualquer a variavel
str(base_consumidores_b_r2$Data)            # está no formato character
base_consumidores_b_r2$Data = as.Date(base_consumidores_b_r2$Data,     # passando para o formato date
                                      format = "%Y-%m-%d")


# passando datas originais para a nova base

for(i in 1:nrow(base_consumidores)){
  for(j in 1:nrow(base_consumidores_b_r2)) {
    if(base_consumidores_b_r2$Data[j] == "1980-01-01"  && base_consumidores_b_r2$ano[j] == base_consumidores$ano[i] && base_consumidores_b_r2$mes[j] == base_consumidores$mes[i]){
      base_consumidores_b_r2$Data[j] = base_consumidores$DatCompetencia[i]
    }
  }
}

base_consumidores$DatCompetencia


head(base_consumidores_b_r2,20)


# Alterando nome da variável das faixas 
base_consumidores_b_r2 = base_consumidores_b_r2 |> 
  rename("Clientes_de_Baixa_Renda" = "DscSubClasseConsumidor")

base_consumidores_b_r2 = base_consumidores_b_r2 |> 
  rename("Baixa Renda" = "Clientes_de_Baixa_Renda")


# Gráfico para verificar a variacao de consumidores ao longo do tempo


base_consumidores_b_r2 |> 
  ggplot(mapping = aes(x = Data, y = soma_faixa,
                       group = `Baixa Renda`,
                       colour = `Baixa Renda`))+
  geom_line(linewidth = 1.5)+
  scale_x_date(date_labels = "%b-%y",
               date_breaks = "3 month") +
  labs(x = "Data",
       y = "Número de Clientes",
       title = "Número de Clientes de Baixa Renda",
       subtitle = "ao longo de 2023 e 2024")+
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.title.x = element_blank())

# Gráfico de sazonalidade
base_sazonalidade <- base_consumo %>%
  filter(DscDetalheMercado == "Energia Consumida (kWh)") %>%
  group_by(ano, mes) %>%
  summarise(consumo_total = sum(VlrMercado)) %>%
  mutate(mes = factor(mes, levels = c("01", "02", "03", "04", "05", "06",
                                      "07", "08", "09", "10", "11", "12"), 
                      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                 "Jul", "Ago", "Set", "Out", "Nov", "Dez")))


### Dashboard

# Pacotes necessários
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(sf)
library(tmap)
library(dplyr)

# Interface do usuário
ui <- dashboardPage(
  dashboardHeader(title = "Painel Interativo EDP SP", titleWidth = 300),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Sobre a empresa", tabName = "sobre_a_empresa", icon = icon("info-circle")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Subgrupos Tarifários", tabName = "subgrupos", icon = icon("layer-group")),
      menuItem("Classes de Consumo", tabName = "classes_consumo", icon = icon("chart-pie")),
      menuItem("Sazonalidade", tabName = "sazonalidade", icon = icon("calendar")),
      menuItem("Receita", tabName = "receita", icon = icon("chart-bar")),
      menuItem("Consumo", tabName = "consumo", icon = icon("bolt")),
      menuItem("Clientes", tabName = "clientes", icon = icon("users")),
      menuItem("Clientes de Baixa Renda", tabName = "baixa_renda", icon = icon("hand-holding-usd")),
      menuItem("Conclusão", tabName = "conclusao", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #003366;
          color: #FFFFFF;
        }
        .skin-blue .main-header .navbar {
          background-color: #003366;
        }
        .skin-blue .main-sidebar {
          background-color: #002244;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #00509E;
          color: #FFFFFF;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #FFFFFF;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #00509E;
        }
        .box {
          border-top: 3px solid #00509E;
        }
      "))
    ),
    tabItems(
      # Sobre a empresa
      tabItem(tabName = "sobre_a_empresa",
              fluidRow(
                box(title = "Informações Gerais", status = "info", solidHeader = TRUE, width = 12,
                    HTML("<ul>
                <li>EDP: empresa global.</li>
                <li>Experiência no ramo.</li>
                <li>Serviço completo.</li>
                <li>Foco em energia verde.</li>
                <li>Referência em inovação e sustentabilidade.</li>
                <li>Projeções de investimentos futuros.</li>
             </ul>")
                )
              ),
              fluidRow(
                tags$div(
                  style = "text-align: center; margin-top: 20px;",
                  tags$img(
                    src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d2/EDP_2022.svg/1920px-EDP_2022.svg.png", 
                    alt = "Logo da EDP", 
                    style = "max-width: 600px; height: auto;"
                  )
                )
              )
      ),
      # Receita
      tabItem(tabName = "receita",
              fluidRow(
                box(title = "Receita Mensal", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_receita_mensal")),
                box(title = "Insights da Receita Mensal (Por Ano)", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_receita_mensal"))
              ),
              fluidRow(
                box(title = "Receita Total Anual", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_receita_anual")),
                box(title = "Projeção da Receita Total", status = "warning", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_receita_projecao"))
              ),
              fluidRow(
                box(title = "Comparação da Receita Total e Projeção", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_receita_comparacao"))
              )
      ),
      # Consumo
      tabItem(tabName = "consumo",
              fluidRow(
                box(title = "Consumo de Energia Mensal", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_consumo"))
              ),
              fluidRow(
                box(title = "Insights do Consumo", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_consumo_mensal"))
              )
      ),
      # Clientes
      tabItem(tabName = "clientes",
              fluidRow(
                box(title = "Número de Clientes Mensal", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_clientes"))
              ),
              fluidRow(
                box(title = "Insights dos Clientes", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_numero_clientes"))
              )
      ),
      # Mapa
      tabItem(tabName = "mapa",
              fluidRow(
                box(title = "Área de Atuação da Concessionária", status = "primary", solidHeader = TRUE, width = 12, 
                    tmapOutput("mapa_edp", height = 500))
              ),
              fluidRow(
                box(title = "Insights da Região de Atuação", status = "info", solidHeader = TRUE, width = 12,
                    HTML("<ul>
                <li>Proximidade com a capital São Paulo, considerada cidade global.</li>
                <li>Polo industrial e comercial referência na região metropolitana de São Paulo.</li>
                <li>83 subestações atendendo as 28 cidades.</li>
             </ul>")
                )
              )
      ),
      # Clientes de Baixa Renda
      tabItem(tabName = "baixa_renda",
              fluidRow(
                box(title = "Evolução por Faixa de Renda", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_baixa_renda"))
              ),
              fluidRow(
                box(title = "Insights sobre Clientes de Baixa Renda", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_baixa_renda"))
              )
      ),
      # Subgrupos Tarifários
      tabItem(tabName = "subgrupos",
              fluidRow(
                box(title = "Distribuição dos Subgrupos Tarifários", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_subgrupos_tarifarios"))
              ),
              fluidRow(
                box(title = "Insights dos Subgrupos Tarifários", status = "info", solidHeader = TRUE, width = 12,
                    HTML("<ul>
                <li>Destaque principal para a quantidade de indústrias e comércios de médio e pequeno portes.</li>
                <li>Importância e responsabilidade da EDP no fornecimento de energia em âmbito regional e nacional.</li>
                <li>Impacto da empresa como atrativo para novas empresas se instalarem na região.</li>
             </ul>")
                )
              )
      ),
      # Classes de Consumo
      tabItem(tabName = "classes_consumo",
              fluidRow(
                box(title = "Distribuição das Classes de Consumo", status = "primary", solidHeader = TRUE, width = 12, 
                    plotlyOutput("grafico_classes_consumo"))
              ),
              fluidRow(
                box(title = "Insights das Classes de Consumo", status = "info", solidHeader = TRUE, width = 12, 
                    htmlOutput("texto_classes_consumo"))
              )
      ),
      # Sazonalidade
      tabItem(tabName = "sazonalidade",
              fluidRow(
                box(title = "Análise de Sazonalidade do Consumo", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotlyOutput("grafico_sazonalidade", height = 500)
                )
              ),
              fluidRow(
                box(title = "Insights da Sazonalidade", status = "info", 
                    solidHeader = TRUE, width = 12,
                    htmlOutput("texto_sazonalidade")
                )
              )
      ),
      # Conclusão
      tabItem(tabName = "conclusao",
              fluidRow(
                box(title = "Conclusões e Recomendações", status = "primary", solidHeader = TRUE, width = 12,
                    htmlOutput("texto_conclusoes"))
              ),
              fluidRow(
                box(title = "Recomendações para o Acionista", status = "info", solidHeader = TRUE, width = 12,
                    htmlOutput("texto_recomendacoes"))
              )
      )
    )
  )
)


# Servidor
server <- function(input, output) {
  # KPIs
  output$receita_total <- renderValueBox({
    total_receita <- sum(base_soma_valores$soma_receita) / 1e9  # Receita total em bilhões
    valueBox(
      paste0("R$ ", round(total_receita, 2), " Bi"),
      "Receita Total (2024 Projeta)",
      icon = icon("dollar-sign"), color = "green"
    )
  })
  
  output$energia_consumida <- renderValueBox({
    media_consumo <- mean(base_soma_consumo$media_consumo_energia_kwh) / 1e6  # Consumo médio em GWh
    valueBox(
      paste0(round(media_consumo, 2), " GWh"),
      "Consumo Médio Mensal",
      icon = icon("bolt"), color = "yellow"
    )
  })
  
  output$num_clientes <- renderValueBox({
    total_clientes <- (base_soma_consumidores$soma_consumidores)[nrow(base_soma_consumidores)] / 1e6  # Total de clientes em milhões
    valueBox(
      paste0(round(total_clientes, 2), "M"),
      "Clientes Ativos (2024)",
      icon = icon("users"), color = "blue"
    )
  })
  
  # Gráficos de Receita
  output$grafico_receita_anual <- renderPlotly({
    dados_anuais <- base_soma_valores |> 
      group_by(ano) |> 
      summarise(receita = sum(soma_receita) / 1e9)  # Receita em bilhões
    
    p <- ggplot(dados_anuais, aes(x = ano, y = receita, fill = as.factor(ano))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0("R$ ", round(receita, 2), "B")),
                size = 4, position = position_stack(vjust = 0.95), color = "black") +  
      labs(x = "Ano", y = "Receita (em bilhões)", title = "Receita Total Anual") +
      scale_y_continuous(labels = label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("2023" = "#C8A2C8", "2024" = "purple")) +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
      )
    
    ggplotly(p)
  })
  
  
  output$grafico_receita_mensal <- renderPlotly({
    p <- ggplot(base_soma_valores, aes(x = Data, y = soma_receita / 1e6)) +
      geom_line(color = "magenta", size = 1.2) +
      labs(x = "Mês", y = "Receita (em milhões)", title = "Receita Mensal") +
      theme_light()+
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(p)
  })
  
  output$texto_receita_mensal <- renderUI({
    # Mês com maior receita para 2023
    mes_maior_receita_2023 <- base_soma_valores %>%
      filter(ano == "2023") %>%
      arrange(desc(soma_receita)) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Mês com menor receita para 2023
    mes_menor_receita_2023 <- base_soma_valores %>%
      filter(ano == "2023") %>%
      arrange(soma_receita) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Mês com maior receita para 2024
    mes_maior_receita_2024 <- base_soma_valores %>%
      filter(ano == "2024") %>%
      arrange(desc(soma_receita)) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Mês com menor receita para 2024
    mes_menor_receita_2024 <- base_soma_valores %>%
      filter(ano == "2024") %>%
      arrange(soma_receita) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Receita média mensal por ano
    receita_media_2023 <- base_soma_valores %>%
      filter(ano == "2023") %>%
      summarise(media = mean(soma_receita)) %>%
      pull(media)
    
    receita_media_2024 <- base_soma_valores %>%
      filter(ano == "2024") %>%
      summarise(media = mean(soma_receita)) %>%
      pull(media)
    
    # Comparação das médias
    variacao_receita_media <- ((receita_media_2024 - receita_media_2023) / receita_media_2023) * 100
    
    HTML(paste0(
      "<h4>Insights da Receita Mensal (Por Ano):</h4>",
      "<ul>",
      "<li><strong>2023:</strong>",
      "<ul>",
      "<li>Mês com maior receita: ", mes_maior_receita_2023, ".</li>",
      "<li>Mês com menor receita: ", mes_menor_receita_2023, ".</li>",
      "<li>Receita média mensal: R$ ", format(round(receita_media_2023, 2), big.mark = ".", decimal.mark = ","), ".</li>",
      "</ul></li>",
      "<li><strong>2024:</strong>",
      "<ul>",
      "<li>Mês com maior receita: ", mes_maior_receita_2024, ".</li>",
      "<li>Mês com menor receita: ", mes_menor_receita_2024, ".</li>",
      "<li>Receita média mensal: R$ ", format(round(receita_media_2024, 2), big.mark = ".", decimal.mark = ","), ".</li>",
      "</ul></li>",
      "<li><strong>Comparação:</strong>",
      " A receita média mensal em 2024 teve uma variação de ", 
      round(variacao_receita_media, 2), "% em relação a 2023.</li>",
      "</ul>"
    ))
  })
  
  # Gráfico de Projeção da Receita
  output$grafico_receita_projecao <- renderPlotly({
    p <- dados_anuais %>%
      ggplot(aes(x = ano, fill = as.factor(ano), y = soma_anual / 1e9)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0("R$ ", round(soma_anual / 1e9, 2), "B")), 
                size = 4, position = position_stack(vjust = 0.95), color = "black") +  
      labs(
        x = "Ano",
        y = "Total (em bilhões)",
        title = "Projeção da Receita Total",
        subtitle = "Projeção ajustada para 2024"
      ) +
      scale_y_continuous(labels = label_dollar(prefix = "R$", big.mark = ".", decimal.mark = ",")) +
      scale_fill_manual(values = c("2023" = "pink", "2024" = "hotpink")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    ggplotly(p)
  })
  
  
  output$texto_receita_comparacao <- renderUI({
    receita_2023 <- dados_anuais %>%
      filter(ano == "2023") %>%
      summarise(total = sum(soma_anual)) %>%
      pull(total)
    
    receita_2024_projecao <- dados_anuais %>%
      filter(ano == "2024") %>%
      summarise(total = sum(soma_anual)) %>%
      pull(total)
    
    variacao_total <- ((receita_2024_projecao - receita_2023) / receita_2023) * 100
    
    HTML(paste0(
      "<ul>",
      "<li><strong>Receita Total de 2023:</strong> R$ ", 
      format(round(receita_2023 / 1e9, 2), big.mark = ".", decimal.mark = ","), " bilhões.</li>",
      "<li><strong>Receita Projetada para 2024:</strong> R$ ", 
      format(round(receita_2024_projecao / 1e9, 2), big.mark = ".", decimal.mark = ","), " bilhões.</li>",
      "<li><strong>Variação Anual:</strong> A projeção para 2024 representa um aumento de ", 
      round(variacao_total, 2), "% em relação a 2023.</li>",
      "<li><strong>Implicações:</strong> A projeção é útil para planejamento financeiro e alocação de recursos estratégicos.</li>",
      "</ul>"
    ))
  })
  
  
  # Gráficos de Consumo
  output$grafico_consumo <- renderPlotly({
    p <- ggplot(base_soma_consumo, aes(x = Data, y = media_consumo_energia_kwh / 1e6)) +
      geom_line(color = "#FF1493", size = 1.2) +
      labs(x = "Mês", y = "Consumo (em GWh)", title = "Consumo de Energia Mensal") +
      theme_light()+
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    ggplotly(p)
  })
  
  output$texto_consumo_mensal <- renderUI({
    # Determinar o mês com maior e menor consumo
    mes_maior_consumo <- base_soma_consumo %>%
      arrange(desc(media_consumo_energia_kwh)) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    mes_menor_consumo <- base_soma_consumo %>%
      arrange(media_consumo_energia_kwh) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Valores de consumo máximo e mínimo
    consumo_maior_mes <- max(base_soma_consumo$media_consumo_energia_kwh)
    consumo_menor_mes <- min(base_soma_consumo$media_consumo_energia_kwh)
    
    # Consumo médio mensal para 2024
    consumo_medio_2024 <- base_soma_consumo %>%
      filter(ano == "2024") %>%
      summarise(media = mean(media_consumo_energia_kwh)) %>%
      pull(media)
    
    # Variação do consumo entre 2023 e 2024
    consumo_total_2023 <- base_soma_consumo %>%
      filter(ano == "2023") %>%
      summarise(total = sum(media_consumo_energia_kwh)) %>%
      pull(total)
    
    consumo_total_2024 <- base_soma_consumo %>%
      filter(ano == "2024") %>%
      summarise(total = sum(media_consumo_energia_kwh)) %>%
      pull(total)
    
    variacao_consumo <- ((consumo_total_2024 - consumo_total_2023) / consumo_total_2023) * 100
    
    HTML(paste0(
      "<h4>Insights sobre Consumo Mensal:</h4>",
      "<ul>",
      "<li><strong>Pico de Consumo:</strong> O maior consumo foi registrado em ", mes_maior_consumo,
      ", totalizando ", round(consumo_maior_mes / 1e6, 2), " GWh.</li>",
      "<li><strong>Mínimo de Consumo:</strong> O menor consumo foi registrado em ", mes_menor_consumo,
      ", com ", round(consumo_menor_mes / 1e6, 2), " GWh.</li>",
      "<li><strong>Variação do Consumo:</strong> O consumo médio mensal em 2024 foi de ", 
      round(consumo_medio_2024 / 1e6, 2), " GWh, representando uma variação de ", 
      round(variacao_consumo, 2), "% em relação a 2023.</li>",
      "<li><strong>Implicações:</strong> A análise sugere que picos de consumo precisam de planejamento para evitar sobrecarga na rede e garantir eficiência operacional.</li>",
      "</ul>"
    ))
  })
  
  
  # Gráficos de Clientes
  output$grafico_clientes <- renderPlotly({
    p <- ggplot(base_soma_consumidores, aes(x = Data, y = soma_consumidores / 1e6)) +
      geom_line(color = "#E0218A", size = 1.2) +
      labs(x = "Mês", y = "Número de Clientes (em milhões)", title = "Número de Clientes Mensal") +
      theme_light()+
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    ggplotly(p)
  })
  
  output$texto_numero_clientes <- renderUI({
    # Determinar o mês com maior e menor número de clientes
    mes_maior_clientes <- base_soma_consumidores %>%
      arrange(desc(soma_consumidores)) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    mes_menor_clientes <- base_soma_consumidores %>%
      arrange(soma_consumidores) %>%
      slice(1) %>%
      mutate(mes = format(Data, "%b-%Y")) %>%
      pull(mes)
    
    # Valores máximo e mínimo de clientes
    max_clientes <- max(base_soma_consumidores$soma_consumidores)
    min_clientes <- min(base_soma_consumidores$soma_consumidores)
    
    # Variação do número de clientes
    clientes_total_2023 <- base_soma_consumidores %>%
      filter(ano == "2023") %>%
      summarise(total = sum(soma_consumidores)) %>%
      pull(total)
    
    clientes_total_2024 <- base_soma_consumidores %>%
      filter(ano == "2024") %>%
      summarise(total = sum(soma_consumidores)) %>%
      pull(total)
    
    variacao_clientes <- ((clientes_total_2024 - clientes_total_2023) / clientes_total_2023) * 100
    
    HTML(paste0(
      "<h4>Insights sobre Número de Clientes:</h4>",
      "<ul>",
      "<li><strong>Pico de Clientes:</strong> O maior número de clientes foi registrado em ", mes_maior_clientes,
      ", totalizando ", max_clientes, " clientes.</li>",
      "<li><strong>Menor Número de Clientes:</strong> O menor número de clientes foi registrado em ", mes_menor_clientes,
      ", com ", min_clientes, " clientes.</li>",
      "<li><strong>Variação Mensal:</strong> A base de clientes apresentou uma variação de ", 
      round(variacao_clientes, 2), "% em 2024 em relação a 2023.</li>",
      "<li><strong>Implicações:</strong> A empresa deve monitorar o crescimento e identificar estratégias para mitigar possíveis reduções na base de clientes.</li>",
      "</ul>"
    ))
  })
  
  
  # Mapa
  output$mapa_edp <- renderTmap({
    tmap_mode("view")
    tm_shape(shape_sp1) + 
      tm_fill(col = "area_edp", title = "Área de Atuação", palette = c("red", "lightyellow", "blue")) +
      tm_borders()
  })
  
  # Gráfico de Baixa Renda
  output$grafico_baixa_renda <- renderPlotly({
    p <- ggplot(base_consumidores_b_r2, aes(x = Data, y = soma_faixa, color = Clientes_de_Baixa_Renda)) +
      geom_line(size = 1.5) +
      labs(
        x = "Mês", 
        y = "Número de Clientes", 
        title = "Evolução de Clientes de Baixa Renda",
        color = "Faixas de Renda"  
      ) +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    ggplotly(p)
  })
  
  output$texto_baixa_renda <- renderUI({
    total_por_faixa <- base_consumidores_b_r2 %>%
      group_by(Clientes_de_Baixa_Renda, ano) %>%
      summarise(total = sum(soma_faixa), .groups = "drop") %>%
      arrange(desc(total))
    
    faixa_mais_clientes <- total_por_faixa %>%
      filter(ano == "2024") %>%
      slice(1) %>%
      pull(Clientes_de_Baixa_Renda)
    
    variacao_faixa <- base_consumidores_b_r2 %>%
      group_by(Clientes_de_Baixa_Renda) %>%
      summarise(
        total_2023 = sum(soma_faixa[ano == "2023"]),
        total_2024 = sum(soma_faixa[ano == "2024"]),
        variacao = (total_2024 - total_2023) / total_2023 * 100,
        .groups = "drop"
      )
    
    HTML(paste0(
      "<h4>Insights sobre Clientes de Baixa Renda:</h4>",
      "<ul>",
      "<li><strong>Faixa com mais clientes em 2024:</strong> ", faixa_mais_clientes, ".</li>",
      "<li><strong>Variação por faixa (2024 vs. 2023):</strong>",
      "<ul>",
      paste0("<li>", variacao_faixa$Clientes_de_Baixa_Renda, ": ", 
             round(variacao_faixa$variacao, 2), "%</li>", collapse = ""),
      "</ul></li>",
      "<li><strong>Implicações:</strong> Necessidade de monitorar inadimplências e reforçar suporte a esses grupos.</li>",
      "</ul>"
    ))
  })
  
  # Gráfico de Subgrupos Tarifários
  output$grafico_subgrupos_tarifarios <- renderPlotly({
    p <- ggplot(base_valor_monetario_count, aes(x = reorder(DscSubGrupoTarifario, -n), y = n, fill = Tensão)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribuição dos Subgrupos Tarifários", x = "Subgrupos", y = "Frequência") +
      theme_light()
    ggplotly(p)
  })
  
  # Gráfico de Classes de Consumo
  output$grafico_classes_consumo <- renderPlotly({
    p <- ggplot(base_valor_monetario_count2, aes(x = reorder(DscClasseConsumoMercado, -n), y = n, fill = DscClasseConsumoMercado)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribuição das Classes de Consumo", x = "Classes", y = "Frequência") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1))+
      theme(legend.position="none")
    ggplotly(p)
  })
  
  output$texto_classes_consumo <- renderUI({
    HTML(paste0(
      "<h4>Perfil dos principais consumidores:</h4>",
      "<ul>",
      "<li>As classes 'Industrial' e 'Residencial' têm a maior frequência, indicando que esses dois segmentos são os principais responsáveis pelo consumo de energia elétrica na área de atuação da EDP São Paulo.</li>",
      "</ul>",
      
      "<h4>Diversificação do consumo:</h4>",
      "<ul>",
      "<li>Classes como 'Comercial' e 'Serviço Público' também possuem frequências significativas, mostrando a relevância desses setores para a base de clientes da empresa, embora em menor escala comparada aos segmentos industrial e residencial.</li>",
      "</ul>",
      
      "<h4>Potenciais áreas de expansão ou ajustes:</h4>",
      "<ul>",
      "<li>As categorias de menor frequência, como 'Iluminação Pública' e 'Consumo Próprio', podem ser exploradas para estratégias de nicho.</li>",
      "<li>A baixa representatividade da categoria 'Não se aplica' sugere um padrão claro de classificação dos clientes.</li>",
      "</ul>"
    ))
  })
  
  
  # Gráfico de Sazonalidade
  output$grafico_sazonalidade <- renderPlotly({
    p <- ggplot(base_sazonalidade, 
                aes(x = mes, y = consumo_total/1e6, 
                    group = ano, color = as.factor(ano))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("2023" = "#00509E", "2024" = "#FF6B6B"),
                         name = "Ano") +
      labs(x = "Mês", 
           y = "Consumo Total (GWh)",
           title = "Comparativo Mensal de Consumo de Energia") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  output$texto_sazonalidade <- renderUI({
    # Calculando algumas estatísticas
    media_2023 <- base_sazonalidade %>%
      filter(ano == "2023") %>%
      summarise(media = mean(consumo_total)) %>%
      pull(media)
    
    media_2024 <- base_sazonalidade %>%
      filter(ano == "2024") %>%
      summarise(media = mean(consumo_total)) %>%
      pull(media)
    
    variacao_percentual <- ((media_2024 - media_2023) / media_2023) * 100
    
    mes_pico_2023 <- base_sazonalidade %>%
      filter(ano == "2023") %>%
      arrange(desc(consumo_total)) %>%
      slice(1) %>%
      pull(mes)
    HTML(paste0(
      "<h4>Principais Insights:</h4>",
      "<ul>",
      "<li><strong>Variação Anual:</strong> O consumo médio em 2024 apresentou uma variação de ", 
      round(variacao_percentual, 2), "% em relação a 2023.</li>",
      "<li><strong>Pico de Consumo:</strong> Em 2023, o maior consumo foi registrado em ", 
      mes_pico_2023, ", indicando um período de maior demanda.</li>",
      "<li><strong>Implicações:</strong> A análise de sazonalidade é crucial para:",
      "<ul>",
      "<li>Planejamento de manutenção em períodos de menor demanda</li>",
      "<li>Gestão de capacidade da rede em períodos de pico</li>",
      "<li>Previsão de demanda e ajuste de tarifas</li>",
      "</ul></li>",
      "</ul>"
    ))
  })
  
  # Conclusão
  
  output$texto_conclusoes <- renderUI({
    HTML(paste0(
      "<h4>Pontos Positivos:</h4>",
      "<ul>",
      "<li><strong>Crescimento da Receita:</strong> A receita total demonstra um padrão de crescimento, suportada por aumento no consumo e na base de clientes.</li>",
      "<li><strong>Atendimento Diversificado:</strong> A EDP atende a uma gama variada de consumidores (residenciais, industriais, comerciais), garantindo estabilidade nas receitas.</li>",
      "<li><strong>Impacto Social:</strong> Forte presença em regiões de baixa renda, alinhando-se a objetivos de sustentabilidade e inclusão.</li>",
      "</ul>",
      "<h4>Pontos de Atenção:</h4>",
      "<ul>",
      "<li><strong>Sazonalidade do Consumo:</strong> A sazonalidade pode impactar os resultados financeiros, principalmente em períodos de menor consumo.</li>",
      "<li><strong>Dependência Regulatória:</strong> Mudanças nas tarifas de energia e regulamentações do setor podem afetar a receita.</li>",
      "<li><strong>Clientes de Baixa Renda:</strong> O aumento dessa base exige monitoramento contínuo para mitigar inadimplências e garantir a sustentabilidade do modelo.</li>",
      "</ul>"
    ))
  })
  
  output$texto_recomendacoes <- renderUI({
    HTML(paste0(
      "<ul>",
      "<li><strong>Investir:</strong>",
      "<ul>",
      "<li>Se a estratégia for voltada para crescimento sustentável em um setor essencial.</li>",
      "<li>A empresa apresenta potencial de valorização no médio e longo prazo, respaldada por tendências de consumo e uma base sólida de clientes.</li>",
      "</ul></li>",
      "<li><strong>Riscos:</strong>",
      "<ul>",
      "<li>Acompanhar de perto os impactos regulatórios e sazonais.</li>",
      "<li>Avaliar projeções financeiras em cenários adversos.</li>",
      "</ul></li>",
      "<li><strong>Estratégias Futuras:</strong>",
      "<ul>",
      "<li>Investir em eficiência energética e redes inteligentes para reduzir perdas e aumentar a confiabilidade.</li>",
      "<li>Explorar parcerias público-privadas para expandir a atuação.</li>",
      "</ul></li>",
      "</ul>"
    ))
  })
  
}

# Rodar o aplicativo
shinyApp(ui, server)
