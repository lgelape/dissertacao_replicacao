#############################
#### RIO DE JANEIRO 2008 ####
#############################

### PACOTES: serao usados e devem ser carregados desde o inicio 
# [caso nao estejam instalados, podem ser instalados pelo comando install.packages()].

library(dplyr)
library(tidyr)
library(data.table)

### Definir o diretorio (deve ser modificado de acordo com o computador do usuario)

setwd("/Users/lucasgelape/Dropbox/Replicacao_Dissertacao_LucasGelape")

# Funcao para limpar acentos, que sera util posteriormente.

clean.accent <- function(x) iconv(x, to = "ASCII//TRANSLIT")

# Carregar a base com resultado total do estado do RJ, para obter a base com os resultados do municipio

getTse<-function(link){
  
  # Cria um nome temporario que o arquivo baixado recebera
  
  pasta.temporaria = file.path(getwd(), "/temp_folder")
  dir.create(pasta.temporaria)
  nome.temporario = file.path(pasta.temporaria, "temp")
  
  # Faz o donwload do link e armazena arquivo temporario
  download.file(link, nome.temporario, quiet = T)
  print("Download concluido")
  
  # Unzip do arquivo temporario
  unzip(nome.temporario, exdir = pasta.temporaria)
  
  # Produz uma lista dos arquivos novos na pasta
  lista.arquivos <- list.files(pasta.temporaria)
  
  # Gera um data frame vazio que será o output da funcao
  dados.output<-data.frame()
  
  # Loop - para cada i de 1 até o tamanho da lista
  for (i in lista.arquivos){
    
    # Gerar o caminho e nome do arquivo combinando pasta e o arquivo i
    nome.arquivo <- file.path(pasta.temporaria, i)
    
    # Extrai a extensao do arquivo (ultimos 3 caracteres do nome)
    extensao.arquivo <- substr(nome.arquivo, (nchar(nome.arquivo)-2), nchar(nome.arquivo))
    
    # Se extensao do arquivo eh igual a txt, seguir
    if (extensao.arquivo=="txt"){
      
      # Obtem as 10 primeiras linhas do arquivo (se houver)
      linhas.arquivo <- length(readLines(nome.arquivo, n=10))
      
      # Se o numero de linhas for maior que 9, seguir
      if (linhas.arquivo>9){
        
        # Imprime no console o nome do arquivo
        print(paste("Arquivo", i, "aberto com sucesso!"))
        
        # Abre o arquivo de dados com o nome 'dados'
        # Opcoes: separador = ; , quote = " e enconding = latin1
        dados <- read.table(nome.arquivo, sep=";", quote="\"",
                            fill = TRUE, fileEncoding="latin1",
                            stringsAsFactors = F, skip = 1)
        
        # Acrescente os dados ao data frame dados.output (empilhar) 
        dados.output <- rbind(dados.output, dados)        
      }
    } 
    
    # Remove o arquivo aberto
    file.remove(nome.arquivo)
  }
  # Remove a pasta temporaria
  file.remove(pasta.temporaria)
  
  # Mensagem final
  print("Arquivo gerado com sucesso!")
  
  # Retorna dados.output
  return(dados.output)
}

estado <- getTse("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_2008_RJ.zip")

# Renomear as variaveis

names(estado) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUM_ZONA", "NUM_SECAO", "CODIGO_CARGO", "DESCRICAO_CARGO", "NUM_VOTAVEL", "QTDE_VOTOS")

# Excluir variaveis que nao serao utilizadas

estado$HORA_GERACAO <- NULL
estado$DESCRICAO_ELEICAO <- NULL
estado$SIGLA_UF <- NULL
estado$SIGLA_UE <- NULL

# Selecionar somente as observacoes correspondentes a cidade do RJ

cidade <- estado[estado$CODIGO_MUNICIPIO == 60011,]
rm(estado) # exclui o objeto dos resultados estaduais da rotina do R, pois nao o usaremos mais

# Carregando a base de locais de votacao, que sera unida com a de resultados eleitorais
# a partir das variaveis NUM_ZONA (n. da zona eleitoral), e NUM_SECAO (n. da secao eleitoral)

lv <- read.csv2("https://raw.githubusercontent.com/lgelape/dissertacao_replicacao/master/Atributos_lv_RJ2008.csv", fileEncoding = "Latin1")

# Renomeando variaveis, para evitar criacao de colunas desnecessarias, quando juntarmos as tabelas

names(cidade)[4] <- "CD_MUN_TSE"
names(cidade)[5] <- "NM_MUN"
names(lv)[1] <- "END_LV"
names(lv)[2] <- "CD_MUN_TSE"
names(lv)[3] <- "NM_MUN"
names(lv)[4] <- "NUM_ZONA"
names(lv)[5] <- "NUM_SECAO"
names(lv)[6] <- "NM_BAIRRO_TSE"
names(lv)[7] <- "NUM_LV"
names(lv)[8] <- "NM_LV"
names(lv)[9] <- "CD_CEP"
names(lv)[13] <- "NM_BAIRRO_IBGE"

# Unindo as duas bases

banco <- left_join(cidade, lv, by = c("NUM_ZONA","NUM_SECAO", "CD_MUN_TSE", "NM_MUN"))

# agregando base por bairro (no caso de SP, por distrito), de acordo com o nome do bairro (distrito)

banco$NM_BAIRRO_TSE <- clean.accent(banco$NM_BAIRRO_TSE) #limpando o acento das duas variaveis, por precaucao
banco$NM_BAIRRO_IBGE <- clean.accent(banco$NM_BAIRRO_IBGE)

base_agr <- aggregate(QTDE_VOTOS ~  NM_BAIRRO_IBGE + CD_GEOCODB + NM_MUN + CD_MUN_TSE + DATA_GERACAO + ANO_ELEICAO + NUM_TURNO + CODIGO_CARGO + DESCRICAO_CARGO + NUM_VOTAVEL, data = banco, sum)

# Retirando os brancos e nulos, pois so os votos validos sao computados para o calculo dos indices

base_agr <- base_agr[!base_agr$NUM_VOTAVEL %in% 95:97,]

# Separando somente as observacoes correspondentes a candidatos a vereadores, pois a base de resultados eleitorais inclui candidatos a prefeito.

base_vereadores <- base_agr[base_agr$CODIGO_CARGO == 13,]

# Criando uma variavel que faca a distincao entre vereadores eleitos (1) e vereadores nao-eleitos (0)

num_eleitos <- c(10123, 10789, 11111, 11120, 11211, 12345, 12580, 12787, 13333, 13444, 13620, 14123, 15101, 15123, 15633, 15688, 15800, 20010, 20126, 22007, 22222, 23000, 23123, 25008, 25101, 25105, 25123, 25622, 25625, 25640, 25678, 27777, 28123, 31031, 33123, 36500, 40044, 40603, 43001, 43123, 43333, 45001, 45007, 45123, 45245, 45620, 50000, 65123, 70070, 70633, 70670)
base_vereadores$ELEITOS <- ifelse(base_vereadores$NUM_VOTAVEL %in% num_eleitos, 1, 0)

# Criar variavel com o percentual que cada bairro corresponde ao total do candidato (que sera posteriormente utilizada para a construcao dos mapas, incluidos no apendice)

base_vereadores <- base_vereadores %>%
  group_by(NUM_VOTAVEL) %>%
  mutate(tot_cand = sum(QTDE_VOTOS),
         perc_bairrocand = (QTDE_VOTOS / tot_cand) * 100)

# Criar variavel com o percentual que cada candidato corresponde ao total do bairro (que sera posteriormente utilizada para a construcao dos mapas, incluidos no apendice)

base_vereadores <- base_vereadores %>%
  group_by(NM_BAIRRO_IBGE) %>%
  mutate(tot_bairro = sum(QTDE_VOTOS),
         perc_candbairro = (QTDE_VOTOS / tot_bairro) * 100)

# Calculo do indice de dominancia media

dom <- function(base, NUM_VOTAVEL, bairro, votos){
  
  
  base[, c(NUM_VOTAVEL, bairro, votos)] %>%
    setNames(c("NUM_VOTAVEL", "bairro", "votos")) %>%
    group_by(NUM_VOTAVEL) %>%
    mutate(tot_cand = sum(votos, na.rm = T)) %>%
    group_by(bairro) %>%
    mutate(tot_bairro = sum(votos, na.rm = T)) %>%
    group_by(NUM_VOTAVEL, bairro) %>%
    summarise(cand_bairro = sum(votos, na.rm = T),
              dominancia = (cand_bairro / tot_bairro[1]) * (cand_bairro / tot_cand[1])) %>%
    group_by(NUM_VOTAVEL) %>%
    summarise(dominancia = sum(dominancia, na.rm = T))
}

dominancia <- dom(base_vereadores, "NUM_VOTAVEL", "NM_BAIRRO_IBGE", "QTDE_VOTOS")

base_vereadores <- merge(base_vereadores, dominancia, by = "NUM_VOTAVEL", all.x = T)


# Calculo do indice HHi

hhi <- function(base, NUM_VOTAVEL, bairro, votos){
  
  
  base[, c(NUM_VOTAVEL, bairro, votos)] %>%
    setNames(c("NUM_VOTAVEL", "bairro", "votos")) %>%
    group_by(NUM_VOTAVEL) %>%
    summarise(HH = sum((votos / sum(votos, na.rm = T))^2))
}

hh <- hhi(base_vereadores, "NUM_VOTAVEL", "NM_BAIRRO_IBGE", "QTDE_VOTOS")

base_vereadores <- merge(base_vereadores, hh, by = "NUM_VOTAVEL", all.x = T)

# Calculo do indice G

G <- function(base, candidato, bairro, votos){
  
  
  x <- base[, c(candidato, bairro, votos)] %>%
    setNames(c("NUM_VOTAVEL", "bairro", "votos")) %>%
    group_by(NUM_VOTAVEL, bairro) %>%
    summarise(votos = sum(votos, na.rm = T)) %>%
    ungroup() 
  
  expand(x, `NUM_VOTAVEL`, `bairro`) %>%
    left_join(x) %>%
    mutate(votos = ifelse(is.na(votos), 0, votos),
           total = sum(votos, na.rm = T)) %>%
    group_by(NUM_VOTAVEL) %>%
    mutate(tot_cand = sum(votos, na.rm = T)) %>%
    group_by(bairro) %>%
    mutate(tot_bairro = sum(votos, na.rm = T)) %>%
    group_by(NUM_VOTAVEL, bairro) %>%
    summarise(indiceG = ((votos / tot_cand) - (tot_bairro/ total))^2) %>%
    group_by(NUM_VOTAVEL) %>%
    summarise(indiceG = sum(indiceG, na.rm = T))
}

indiceG <- G(base_vereadores, "NUM_VOTAVEL", "NM_BAIRRO_IBGE", "QTDE_VOTOS")

base_vereadores <- merge(base_vereadores, indiceG, by = "NUM_VOTAVEL", all.x = T)

# Criar e salvar um banco somente com as observacoes unicas dos eleitos, incluindo somente informacoes basicas e o valor dos indices

base_eleitos <- base_vereadores %>%
  dplyr::select(NUM_VOTAVEL, NM_MUN, ANO_ELEICAO, ELEITOS, dominancia, HH, indiceG) %>%
  distinct() %>%
  filter(ELEITOS == 1)

write.csv2(base_eleitos, "RJ2008_eleitos.csv", row.names = F)
