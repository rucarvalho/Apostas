# Pacotes utilizados
library(tidyverse)
library(readxl)

options(scipen = 999) # configurando para retir a notação cientifica

# Subindo base

df <- read_excel("apos.xlsx")

## Visão geral da base 

count(df) # Nº de apostas
min(df$crash) # Valor mínimo do crash
max(df$crash) # Valor máximo do crash
mean(df$crash) # Média do valor do crash
median(df$crash) # Mediana do valor do crash

quantile(df$crash, 0.25) # Primeiro quartil (25%)
quantile(df$crash, 0.50) # Mediana
quantile(df$crash, 0.75) # Terceiro quartil (75%)

table(df$`valor apostado`) # Quantidade de apostas por valor apostado
table(df$retirada) # Valores do multiplicador apostado 

# Histograma das apostas por valor do "crash"

ggplot(data = df, aes(x = crash)) +
  geom_histogram(bins = 60, fill = "red", color = "white") + # com 60 barras/ grupos
  scale_x_continuous(breaks = seq(1,60,2)) +
  scale_y_continuous(breaks = seq(0,60,5)) +
  labs(y = "Nº de apostas",
       x = "Valor do 'crash'",
       title = "Distribuição das apostas por valor do 'crash'") +
  theme_minimal() # tela de fundo do gráfico

# Criando grupos p/ melhor visualizar a distribuição dos "crash", grupos de 0,5 em 0,5

df["grupo_crash"] <- ifelse(df$crash <= 1.5,"De 1,00x a 1,50x",
                     ifelse(df$crash <= 2 ,"De 1,51x a 2,00x",
                     ifelse(df$crash <= 2.5,"De 2,01x a 2,50x",
                     ifelse(df$crash <= 3,"De 2,51x a 3,00x",
                     ifelse(df$crash <= 3.5,"De 3,01x a 3,50x",
                     ifelse(df$crash <= 4,"De 3,51x a 4,00x",
                     ifelse(df$crash <= 4.5,"De 4,01x a 4,50x",
                     ifelse(df$crash <= 5,"De 4,51x a 5,00x",
                     ifelse(df$crash <= 5.5,"De 5,01x a 5,50x",
                     ifelse(df$crash <= 6,"De 5,51x a 6,00x",
                     ifelse(df$crash <= 6.5,"De 6,01x a 6,50x",
                     ifelse(df$crash <= 7,"De 6,51x a 7,00x",
                     "Maior que 7x"))))))))))))

# Criando o gráfico com os grupos criados anteriormente

df %>% 
  count(grupo_crash) %>%
  mutate(grupo_crash = forcats::lvls_reorder(grupo_crash, c(13,12,11,10,9,8,7,6,5,
                                                            4,3,2,1))) %>% # Reordenando os grupos para ficarem na ordem crescentes
  ggplot() +
  geom_col(
    aes(x = n, y = grupo_crash, label = n), fill = "darkgreen") +
  geom_label(aes(x = n, y = grupo_crash, label = n)) +
  labs(y = "Grupos de 'crash'",
       x = "Nº de 'crash'",
       title = "Distribuição de 'crash' por grupo") +
  theme_minimal()

# Construindo o mesmo gráfico  talvez excluir 
# 
# df %>% 
#   filter(crash <= 2) %>% 
#   ggplot(aes(x = crash)) +
#   geom_histogram(bins = 10, fill = "red", color = "black") +
#   scale_y_continuous(breaks = seq(0,20,2)) +
#   labs(y = "Nº de apostas",
#        x = "Valor do 'crash'",
#        title = "Distribuição das apostas com 'crash' menor que 2 por valor do 'crash'") +
#   theme_minimal() 

# Criando um grupo mais detalhado para os crash menores que 2

df["grupo_crash_menor_2"] <- ifelse(df$crash <= 1.10,"De 1,00x a 1,10x",
                             ifelse(df$crash <= 1.20,"De 1,11x a 1,20x",
                             ifelse(df$crash <= 1.30,"De 1,21x a 1,30x",
                             ifelse(df$crash <= 1.40,"De 1,31x a 1,40x",
                             ifelse(df$crash <= 1.50,"De 1,41x a 1,50x",
                             ifelse(df$crash <= 1.60,"De 1,51x a 1,60x",
                             ifelse(df$crash <= 1.70,"De 1,61x a 1,70x",
                             ifelse(df$crash <= 1.80,"De 1,71x a 1,80x",
                             ifelse(df$crash <= 1.90,"De 1,81x a 1,90x",
                             ifelse(df$crash <= 2.00,"De 1,91x a 2,00",
                                    "mais de 2,00x"))))))))))

# Criando o gráfico com os grupos criados anteriormente e com crash menores de 2

df %>% 
  filter(crash <= 2) %>% # filtrando os crash
  count(grupo_crash_menor_2) %>%
  mutate(grupo_crash_menor_2 = forcats::lvls_reorder(grupo_crash_menor_2, c(10,9,8,7,6,5,
                                                           4,3,2,1))) %>% # reordenando os grupos
  ggplot() +
  geom_col(
    aes(x = n, y = grupo_crash_menor_2, label = n), fill = "darkgreen") +
  geom_label(aes(x = n, y = grupo_crash_menor_2, label = n)) +
  labs(y = "Grupos de 'crash'",
       x = "Nº de 'crash'",
       title = "Distribuição de 'crash' por grupo") +
  theme_minimal()

# Linha do tempo do crash, valor do crash por ordem da jogada

ggplot(data = df, aes(x = ordem, y = crash)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0,60,5)) +
  scale_x_continuous(breaks = seq(0,156,10)) +
  geom_hline(yintercept = c(3.78, 1.91), color = c("red","blue")) +
  labs(y = "Valor do 'crash'",
       x = "Ordem da aposta",
       title = "Valor do 'crash' pelo ordem da jogada") +
  theme_minimal()

# Criando grupos por ordem da aposta

df["grupo_aposta"] <- ifelse(df$ordem <= 15,"Da 1º a 15º",
                      ifelse(df$ordem <= 30,"Da 16º a 30º",
                      ifelse(df$ordem <= 45,"Da 31º a 45º",
                      ifelse(df$ordem <= 60,"Da 46º a 60º",
                      ifelse(df$ordem <= 75,"Da 61º a 75º",
                      ifelse(df$ordem <= 90,"Da 76º a 90º",
                      ifelse(df$ordem <= 105,"Da 91º a 105º",
                      ifelse(df$ordem <= 120,"Da 106º a 120º",
                      ifelse(df$ordem <= 135,"Da 121º a 135º",
                      ifelse(df$ordem <= 150,"Da 136º a 150º","demais"))))))))))

# Tabela com média e mediana, dos grupos de ordens das apostas 

ordem_aposta <- df %>% 
  group_by(grupo_aposta) %>% 
  summarise(media = mean(crash),
            mediana = median(crash))

# Calculo do Intervalo de confiança por grupo, repetir o comando para cada grupo
df_ic <- df %>% 
  filter(grupo_aposta == "Da 136º a 150º") 

t.test(df_ic$crash) # calculo do IC

# Gráfico de box plot 

df %>% 
  filter(grupo_aposta != "demais") %>% 
  mutate(grupo_aposta = forcats::lvls_reorder(grupo_aposta, c(5,4,6,7,8,9,
                                                              10,1,2,3))) %>% # Reordenando as caixas do gráfico
  ggplot(aes(y = crash, x = grupo_aposta)) +
  geom_boxplot() +
  labs(y = "Valor do 'crash'",
       x = "Grupo ordem da aposta",
       title = "Valor do 'crash' pelo ordem da jogada") +
  theme_minimal()

# teste de Kruskal-Wallis para avaliar a distribuição dos crash por grupo

kruskal.test(df$crash~factor(df$grupo_aposta))

# Tabela com média e mediana, dos grupos do valor da aposta

caracteristicas <- df %>% 
  group_by(`valor apostado`) %>% 
  summarise(volume = n(),
            media = mean(crash),
            mediana = median(crash))

# Calculo do Intervalo de confiança por grupo, repetir o comando para cada grupo

aposta_1 <- df %>% filter(`valor apostado` == 1)
aposta_2 <- df %>% filter(`valor apostado` == 2)

t.test(aposta_1$crash) 
t.test(aposta_2$crash) 

# teste de média
t.test(aposta_1$crash, aposta_2$crash) 

# teste de Kruskal-Wallis para avaliar a distribuição dos crash por valor apostado
kruskal.test(df$crash~factor(df$`valor apostado`))

#### Classificando as apostas entra ganhos e perdas 

df["ganho"] <- ifelse(df$crash > df$retirada,"ganho","perda")

apostas_05 <- df %>% 
  filter(retirada == 1.05) %>% 
  group_by(ganho) %>% 
  summarise(vol = n())

apostas_05["prop"] <- (apostas_05$vol / sum(apostas_05$vol))*100 # Distribuição relativa das apostas por resultado

apostas_10 <- df %>% 
  filter(retirada == 1.10) %>% 
  group_by(ganho) %>% 
  summarise(vol = n())

apostas_10["prop"] <- (apostas_10$vol / sum(apostas_10$vol))*100 # Distribuição relativa das apostas por resultado

apostas_15 <- df %>% 
  filter(retirada == 1.15) %>% 
  group_by(ganho) %>% 
  summarise(vol = n())

apostas_15["prop"] <- (apostas_15$vol / sum(apostas_15$vol))*100 # Distribuição relativa das apostas por resultado


