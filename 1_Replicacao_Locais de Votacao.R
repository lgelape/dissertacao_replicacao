############################
#### LOCAIS DE VOTACAO  ####
############################

### PACOTES: serao usados e devem ser carregados desde o inicio.

library(ggmap)

### Definir o diretorio que sera usado (adaptar ao usuario):

setwd("C:/Users/Lucas Gelape/Dropbox/Replicacao/")

### ORGANIZANDO A BASE DE LOCAIS DE VOTACAO, PARA REALIZAR A BUSCA DE ENDERECOS 
## o exemplo aqui apresentado e' de Sao Paulo, devendo ser adaptado para os outros municipios, caso seja de interesse.
## as bases construidas nessa etapa sao corrigidas manualmente, em etapa posterior. Esta e' portanto uma ilustracao sobre uma maneira possivelmente mais simples de se construir este banco.

# Funcao para limpar acentos, que sera util posteriormente.

gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, perl=TRUE, ...)
  x
}

clean.accent <- function(x) {	gsub2(c('?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?','?'), c('a','a','a','a','a','e','e','e','e','i','i','i','o','o','o','o','o','u','u','u','u','A','A','E','E','I','O','U','n','n','c','C','_','_','O','O','A','A','U'),x)
}

# Carregar a base de locais de votacao do municipio (foram disponibilizadas as bases correspondentes a cada um dos tres municipios)

lv <- read.csv2("lv_sp2012.csv")

# Retirando os acentos dos enderecos, para evitar inconsistencias

lv$ENDERECO_LOCALVOTACAO <- clean.accent(lv$ENDERECO_LOCALVOTACAO)

# Criando lista de enderecos unicos, que sera utilizada para buscar as coordenadas geograficas

busca <- unique(lv$ENDERECO_LOCALVOTACAO)

# loop para coletar coordenada do api do google
basegeo <- NULL

for (i in 1:length(busca)){
  
  x <- geocode(paste("Sao Paulo,",busca[i] ))
  x$ENDERECO_LOCALVOTACAO <- busca[i]             
  
  basegeo <- rbind.data.frame(basegeo, x)   
  
  print(i)
}

# Unindo a base dos locais com as coordenadas

lvcoord <- merge(lv, basegeo, by = "ENDERECO_LOCALVOTACAO", all.x = T)

# Retirar acentos de todas as variaveis, para evitar erros

lvcoord$NM_BAIRRO <- clean.accent(lvcoord$NM_BAIRRO)
lvcoord$NM_LOCALVOTACAO <- clean.accent(lvcoord$NM_LOCALVOTACAO)
lvcoord$ENDERECO_LOCALVOTACAO <- clean.accent(lvcoord$ENDERECO_LOCALVOTACAO)

# Salvar a base com os locais de votacao e suas respectivas coordenadas

write.csv2(lvcoord, "lv_coord_sp_2012.csv")

## Conforme explicado no texto da dissertacao, a base aqui construida foi plotada no arquivo shapefile com o mapa de Sao Paulo. 
## Os casos de NA ou que foram plotados fora dos limites do municipio, foram corrigidos manualmente, por meio de busca no Google Maps.

## A exploracao desses dados no mapa foi feita por meio do QGis, um software livre.
## No QGis, deve ser seguido o seguinte procedimento para plotar os pontos no mapa, e serem feitas as correcoes
## Para abrir o mapa: Camada > Adicionar Camada > Vetorial > Buscar (e selecionar o arquivo de extensao ".shp" que se deseja abrir)
## Para plotar no mapa o banco de dados com os locais de votacao: 
# Camada > Adicionar Camada > "A partir de um texto delimitado"> Procurar (e selecionar o arquivo - observar que o delimitador deve ser somente "Ponto e Virgula", no "Campo X" deve ser a variavel lon e no "Campo Y" a variavel lat)
## Assim, podem ser vistos os pontos que foram plotados fora dos limites do municipio, que devem ser reconhecidos e corrigos na base original (.csv)

## Uma vez corrigidos, eles devem ser novamente plotados, segundo o processo descritos acima.
## Para unir os pontos ao mapa, de forma a unir os dois bancos de dados (o que sustenta o .shp e o de locais de votacao):
# Vetor > Gerenciar Dados > Unir atributo pela posicao (a camada vetorial alvo e' a de pontos, e a camada a ser unida e' o shp; marcar a caixa "dentro de"; estat?sticas para o sum?rio: sum - ou qualquer outra, mas somente uma; unir tabelas: apenas manter os registros correspondentes) 
## Com as camadas unidas, exporta-se a tabela de atributos em formato .xls (posteriormente salva em .csv), que sera utilizada posteriormente no R:
# Selecionar a camada unida e: Vetor > XY Tools > Save Attribute Table as Excel File.