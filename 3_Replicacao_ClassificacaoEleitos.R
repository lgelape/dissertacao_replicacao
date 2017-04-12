##############################################
### REPLICACAO - CLASSIFICACAO DOS ELEITOS ###
##############################################

### PACOTES: serao usados e devem ser carregados desde o inicio.

library(psych)
library(ggplot2)
library(electionsBR)
library(dplyr)

### Definir o diretorio (deve ser modificado de acordo com o computador do usuario)

setwd("/Users/lucasgelape/Dropbox/Replicacao_Dissertacao_LucasGelape")

### Abrindo cada um dos bancos de eleitos, e unindo todos num unico banco

bh2008 <- read.csv2("BH2008_eleitos.csv")
bh2012 <- read.csv2("BH2012_eleitos.csv")
rj2008 <- read.csv2("RJ2008_eleitos.csv")
rj2012 <- read.csv2("RJ2012_eleitos.csv")
sp2008 <- read.csv2("SP2008_eleitos.csv")
sp2012 <- read.csv2("SP2012_eleitos.csv")

### Unindo os bancos de cada municipio em um unico banco

base_unida <- rbind(bh2008, bh2012, rj2008, rj2012, sp2008, sp2012)

### Calculo das estatisticas descritivas das variaveis

# Funcao summary apresenta o min, 1o quartil, mediana, media, 3o quartil, maximo.

summary(base_unida$dominancia)
summary(base_unida$HH)
summary(base_unida$indiceG)

# Funcao describe apresenta, dentre outros, o n, desvio-padrao, media, min, mediana, max.

describe(base_unida$dominancia)
describe(base_unida$HH)
describe(base_unida$indiceG)

# Criar os histogramas para cada um dos indices

hist(base_unida$HH, main = NULL, xlab = "HHi", ylab = "Frequencia")
hist(base_unida$dominancia, main = NULL, xlab = "Dominancia", ylab = "Frequencia")
hist(base_unida$indiceG, main = NULL, xlab = "G", ylab = "Frequencia")

### Calculo das correlacoes

cor.test(base_unida$dominancia, base_unida$HH, method = c("pearson"), conf.level = 0.99)
cor.test(base_unida$dominancia, base_unida$indiceG, method = c("pearson"), conf.level = 0.99)
cor.test(base_unida$HH, base_unida$indiceG, method = c("pearson"), conf.level = 0.99)

### Fazendo os agrupamentos de kmeans para a base unida

# 2 Grupos

# Cria uma variavel aplicando a funcao kmeans em 2 grupos sendo que o valor das observacoes sera o do cluster designado

base_unida$kmeans2_dom <- kmeans(base_unida$dominancia, 2, nstart = 1000)$cluster
kmeans(base_unida$dominancia, 2, nstart = 1000) ## apresenta as estatisticas do kmeans para o indice D

base_unida$kmeans2_hhi <- kmeans(base_unida$HH, 2, nstart = 1000)$cluster 
kmeans(base_unida$HH, 2, nstart = 1000) ## apresenta as estatisticas do kmeans para o indice HHi

# 4 Grupos (a partir do mesmo procedimento descrito para o agrupamento acima)

base_unida$kmeans4_dom <- kmeans(base_unida$dominancia, 4, nstart = 1000)$cluster
kmeans(base_unida$dominancia, 4, nstart = 1000)

base_unida$kmeans4_hhi <- kmeans(base_unida$HH, 4, nstart = 1000)$cluster
kmeans(base_unida$HH, 4, nstart = 1000)

# 6 Grupos

base_unida$kmeans6_dom <- kmeans(base_unida$dominancia, 6, nstart = 1000)$cluster
kmeans(base_unida$dominancia, 6, nstart = 1000)

base_unida$kmeans6_hhi <- kmeans(base_unida$HH, 6, nstart = 1000)$cluster
kmeans(base_unida$HH, 6, nstart = 1000)

table(base_unida$kmeans6_dom)
table(base_unida$kmeans6_hhi)
prop.table(table(base_unida$kmeans6_dom))
prop.table(table(base_unida$kmeans6_hhi))

### Classificar os vereadores a partir do resultado do kmeans

# 2 grupos:

base_unida$padraokmeans2dom<- NA
base_unida$padraokmeans2dom <- ifelse(base_unida$dominancia > 0.08783435, "Dominante", "Compartilhado")

base_unida$padraokmeans2hhi <- NA
base_unida$padraokmeans2hhi <- ifelse(base_unida$HH > 0.11630686, "Concentrado", "Disperso")

base_unida$AmesKmeans2 <- NA
base_unida$AmesKmeans2[base_unida$padraokmeans2hhi == "Concentrado" & base_unida$padraokmeans2dom == "Dominante"] <- 1
base_unida$AmesKmeans2[base_unida$padraokmeans2hhi == "Concentrado" & base_unida$padraokmeans2dom == "Compartilhado"] <- 2
base_unida$AmesKmeans2[base_unida$padraokmeans2hhi == "Disperso" & base_unida$padraokmeans2dom == "Dominante"] <- 3
base_unida$AmesKmeans2[base_unida$padraokmeans2hhi == "Disperso" & base_unida$padraokmeans2dom == "Compartilhado"] <- 4

table(base_unida$AmesKmeans2) # Apresenta uma tabela de frequencia absoluta com a classificacao em 2 grupos
prop.table(table(base_unida$AmesKmeans2)) # Apresenta uma tabela de frequencia relativa com a classificacao em 2 grupos

# 4 Grupos:

base_unida$padraokmeans4dom <- NA
base_unida$padraokmeans4dom <- ifelse(base_unida$dominancia > 0.08275569, "Dominante", "Compartilhado")
# Foi feito um pequeno ajuste na linha acima, colocando-se um valor 0.00000001 maior do que o valor da ultima observacao
# que deveria ser considerada como "compartilhado", pois ela estava sendo erroneamente atribuida como "dominante"

base_unida$padraokmeans4hhi <- NA
base_unida$padraokmeans4hhi <- ifelse(base_unida$HH > 0.11630686, "Concentrado", "Disperso")

base_unida$AmesKmeans4 <- NA
base_unida$AmesKmeans4[base_unida$padraokmeans4hhi == "Concentrado" & base_unida$padraokmeans4dom == "Dominante"] <- 1
base_unida$AmesKmeans4[base_unida$padraokmeans4hhi == "Concentrado" & base_unida$padraokmeans4dom == "Compartilhado"] <- 2
base_unida$AmesKmeans4[base_unida$padraokmeans4hhi == "Disperso" & base_unida$padraokmeans4dom == "Dominante"] <- 3
base_unida$AmesKmeans4[base_unida$padraokmeans4hhi == "Disperso" & base_unida$padraokmeans4dom == "Compartilhado"] <- 4

table(base_unida$AmesKmeans4)
prop.table(table(base_unida$AmesKmeans4))


# 6 grupos:

base_unida$padraokmeans6dom<- NA
base_unida$padraokmeans6dom <- ifelse(base_unida$dominancia > 0.05528523, "Dominante", "Compartilhado")

base_unida$padraokmeans6hhi <- NA
base_unida$padraokmeans6hhi <- ifelse(base_unida$HH > 0.13754156, "Concentrado", "Disperso")

base_unida$AmesKmeans6 <- NA
base_unida$AmesKmeans6[base_unida$padraokmeans6hhi == "Concentrado" & base_unida$padraokmeans6dom == "Dominante"] <- 1
base_unida$AmesKmeans6[base_unida$padraokmeans6hhi == "Concentrado" & base_unida$padraokmeans6dom == "Compartilhado"] <- 2
base_unida$AmesKmeans6[base_unida$padraokmeans6hhi == "Disperso" & base_unida$padraokmeans6dom == "Dominante"] <- 3
base_unida$AmesKmeans6[base_unida$padraokmeans6hhi == "Disperso" & base_unida$padraokmeans6dom == "Compartilhado"] <- 4

table(base_unida$AmesKmeans6)
prop.table(table(base_unida$AmesKmeans6))


## Pela media

base_unida$padraomediadom <- ifelse(base_unida$dominancia > mean(base_unida$dominancia), "Dominante", "Compartilhado")
base_unida$padraomediaHH <- ifelse(base_unida$HH > mean(base_unida$HH), "Concentrado", "Disperso")

base_unida$Amesmedia <- NA
base_unida$Amesmedia[base_unida$padraomediaHH == "Concentrado" & base_unida$padraomediadom == "Dominante"] <- 1
base_unida$Amesmedia[base_unida$padraomediaHH == "Concentrado" & base_unida$padraomediadom == "Compartilhado"] <- 2
base_unida$Amesmedia[base_unida$padraomediaHH == "Disperso" & base_unida$padraomediadom == "Dominante"] <- 3
base_unida$Amesmedia[base_unida$padraomediaHH == "Disperso" & base_unida$padraomediadom == "Compartilhado"] <- 4

table(base_unida$Amesmedia)
prop.table(table(base_unida$Amesmedia))

write.csv2(base_unida, "baseunida_eleitos.csv")

### Para calcular por partidos

## Para fazer o calculo por partidos, juntamos as bases dos eleitos com a base dos candidatos em cada municipio, pelo pacote electionsBR.
## Em seguida, juntamos a base com as classificacoes. Enfim, calculamos a frequencia dos partidos, para a variavel com a classificacao desejada.

# 2012

cands2012 <- candidate_local(2012)

sp2012cands <- cands2012[cands2012$SIGLA_UE == 71072,]
sp2012eleitos <- left_join(sp2012, sp2012cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
sp2012eleitos2 <- merge(sp2012eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
sp2012final <- sp2012eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

rj2012cands <- cands2012[cands2012$SIGLA_UE == 60011,]
rj2012eleitos <- left_join(rj2012, rj2012cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
rj2012eleitos2 <- merge(rj2012eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
rj2012final <- rj2012eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

bh2012cands <- cands2012[cands2012$SIGLA_UE == 41238,]
bh2012eleitos <- left_join(bh2012, bh2012cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
bh2012eleitos2 <- merge(bh2012eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
bh2012final <- bh2012eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

# 2008

cands2008 <- candidate_local(2008)

sp2008cands <- cands2008[cands2008$SIGLA_UE == 71072,]
sp2008eleitos <- left_join(sp2008, sp2008cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
sp2008eleitos2 <- merge(sp2008eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
sp2008final <- sp2008eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

rj2008cands <- cands2008[cands2008$SIGLA_UE == 60011,]
rj2008eleitos <- left_join(rj2008, rj2008cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
rj2008eleitos2 <- merge(rj2008eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
rj2008final <- rj2008eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

bh2008cands <- cands2008[cands2008$SIGLA_UE == 41238,]
bh2008eleitos <- left_join(bh2008, bh2008cands, by = c("NUM_VOTAVEL" = "NUMERO_CANDIDATO", "ANO_ELEICAO"))
bh2008eleitos2 <- merge(bh2008eleitos, base_unida, by = c("NUM_VOTAVEL", "NM_MUN", "ANO_ELEICAO"))
bh2008final <- bh2008eleitos2 %>%
  dplyr::select(NM_MUN, ANO_ELEICAO, NUM_VOTAVEL, NOME_URNA_CANDIDATO, SIGLA_PARTIDO, dominancia.x, HH.x, AmesKmeans6) %>%
  distinct()

base_unidapartidos <- rbind(bh2008final, bh2012final, rj2008final, rj2012final, sp2008final, sp2012final)

table(base_unidapartidos$SIGLA_PARTIDO, base_unidapartidos$AmesKmeans6)

write.csv2(base_unidapartidos, "baseunida_completa.csv")

### Para fazer os graficos de dispersao apresentados no texto

# Grafico de Dispersao com a Classificacao pela media

ggplot(base_unida, aes(HH, dominancia, colour = factor(Amesmedia))) + geom_point() + 
  scale_color_manual(labels = c("Concentrado-Dominante", "Concentrado-Compartilhado", 
                                "Disperso-Dominante", "Disperso-Compartilhado"), 
                     values = c("blue", "red", "black", "orange")) + 
  labs(title = "", x = "HH", y = "Dominancia", color = "Classificacao") + 
  geom_hline(yintercept = 0.056114085, linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 0.138970565, linetype = "dashed", size = 0.5) + theme_classic()

# Grafico de Dispersao com a Classificacao pelo kmeans em 2 grupos

ggplot(base_unida, aes(HH, dominancia, colour = factor(AmesKmeans2))) + geom_point() + 
  scale_color_manual(labels = c("Concentrado-Dominante", "Concentrado-Compartilhado", 
                                "Disperso-Dominante", "Disperso-Compartilhado"), 
                     values = c("blue", "red", "black", "orange")) + 
  labs(title = "", x = "HH", y = "Dominancia", color = "Classificacao") + 
  geom_hline(yintercept = 0.056114085, linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 0.138970565, linetype = "dashed", size = 0.5) + theme_classic()

# Grafico de Dispersao com a Classificacao pelo kmeans em 4 grupos

ggplot(base_unida, aes(HH, dominancia, colour = factor(AmesKmeans4))) + geom_point() + 
  scale_color_manual(labels = c("Concentrado-Dominante", "Concentrado-Compartilhado", 
                                "Disperso-Dominante", "Disperso-Compartilhado"), 
                     values = c("blue", "red", "black", "orange")) + 
  labs(title = "", x = "HH", y = "Dominancia", color = "Classificacao") + 
  geom_hline(yintercept = 0.056114085, linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 0.138970565, linetype = "dashed", size = 0.5) + theme_classic()

# Grafico de Dispersao com a Classificacao pelo kmeans em 6 grupos

ggplot(base_unida, aes(HH, dominancia, colour = factor(AmesKmeans6))) + geom_point() + 
  scale_color_manual(labels = c("Concentrado-Dominante", "Concentrado-Compartilhado", 
                                "Disperso-Dominante", "Disperso-Compartilhado"), 
                     values = c("blue", "red", "black", "orange")) + 
  labs(title = "", x = "HH", y = "Dominancia", color = "Classificacao") + 
  geom_hline(yintercept = 0.056114085, linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 0.138970565, linetype = "dashed", size = 0.5) + theme_classic()


# Grafico com 6 Faixas de Concentracao
# As cores e legendas podem sair diferentes daquelas encontradas no texto da dissertacao,
# pois elas dependem da numeracao que o R designou ao cluster, que varia a cada vez
# que a funcao e' usada, apesar do tamanho e media do cluster ser o mesmo.
# Assim, a associacao entre cores e faixas devem ser adaptada de acordo com o resultado do kmeans.

ggplot(base_unida, aes(HH, dominancia, colour = factor(kmeans6_hhi))) + geom_point() + 
  scale_color_manual(labels = c("Dispersao Baixa", "Dispersao Media", "Dispersao Alta", 
                                "Concentracao Baixa", "Concentracao Media", "Concentracao Alta"), 
                     values = c("green", "blue", "purple", "yellow", "orange", "red")) + 
  labs(title = "", x = "HH", y = "Dominancia", color = "Classificacao") + 
  geom_vline(xintercept = 0.138970565, linetype = "dashed", size = 0.5) + theme_classic()

# Grafico com 6 Faixas de Dominancia
# As cores e legendas podem sair diferentes daquelas encontradas no texto da dissertacao,
# pois elas dependem da numeracao que o R designou ao cluster, que varia a cada vez
# que a funcao e' usada, apesar do tamanho e media do cluster ser o mesmo.
# Assim, a associacao entre cores e faixas devem ser adaptada de acordo com o resultado do kmeans.

ggplot(base_unida, aes(HH, dominancia, colour = factor(kmeans6_dom))) + geom_point() + 
  scale_color_manual(labels = c("Compartilhamento Baixo", "Dominancia Media", "Compartilhamento Alto", 
                                "Dominancia Alta", "Dominancia Baixa", "Compartilhamento Medio"), 
                     values = c("green", "orange", "purple", "red", "yellow", "blue")) + 
  labs(title = "", x = "HH", y = "Domin?ncia", color = "Classifica??o") + 
  geom_hline(yintercept = 0.056114085, linetype = "dashed", size = 0.5) + theme_classic()
