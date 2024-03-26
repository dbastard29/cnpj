#############################
# DEU CERTO? AGRADECE A GENTE PELO PIX
#
# (43) 991701144
#
# $5, $10 ou $30 ou o que vc puder.
# SE NAO DEU CERTO, A GENTE AJUDA
# santosdias.com
#############################


# Instalação e Carregamento de Todos os Pacotes----
pacotes <- c("RColorBrewer", "wordcloud", "grDevices", "data.table", 
             "tidyverse", "readxl", "dplyr", "tm", "readr", "stringi", 
             "writexl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
# ----

setwd("%%%%YOUR FOLDER FILE%%%%%")

###TABELAS E DICIONARIOS ADICIONAIS###
# Carregar dados totais
arquivos <-
  c(
    "F.K03200$Z.D30708.CNAECSV"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  cnae <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                     header = FALSE,
                     colClasses = c("character", "character"),
                     sep = ";",
                     fill = TRUE,
                     encoding = 'latin1',
                     quote = "\""
  )
  lista[[i]] <- cnae
}
write_csv(cnae, "cnae.csv")

#Carregar dados Natureza Juridica
arquivos <-
  c(
    "F.K03200$Z.D30812.NATJUCSV"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  natureza_juridica <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                                  header = FALSE,
                                  colClasses = c("character", "character"),
                                  sep = ";",
                                  fill = TRUE,
                                  encoding = 'latin1',
                                  quote = "\""
  )
  lista[[i]] <- natureza_juridica
}
write_csv(natureza_juridica, "natureza_juridica.csv")

#Carregar dados Natureza Juridica
arquivos <-
  c(
    "F.K03200$Z.D30812.QUALSCSV"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  qualificacao <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                             header = FALSE,
                             colClasses = c("character", "character"),
                             sep = ";",
                             fill = TRUE,
                             encoding = 'latin1',
                             quote = "\""
  )
  lista[[i]] <- qualificacao
}
write_csv(qualificacao, "qualificacao.csv")

#Carregar dados Natureza Juridica
arquivos <-
  c(
    "F.K03200$Z.D30812.PAISCSV"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  pais <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                     header = FALSE,
                     colClasses = c("character", "character"),
                     sep = ";",
                     fill = TRUE,
                     encoding = 'latin1',
                     quote = "\""
  )
  lista[[i]] <- pais
}
write_csv(pais, "pais.csv")

#Carregar dados Natureza Juridica
arquivos <-
  c(
    "F.K03200$Z.D30812.MUNICCSV"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  cidade <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                       header = FALSE,
                       colClasses = c("character", "character"),
                       sep = ";",
                       fill = TRUE,
                       encoding = 'latin1',
                       quote = "\""
  )
  lista[[i]] <- cidade
}
write_csv(cidade, "cidade.csv")

### Layout dos Arquivos ESTABELECIMENTO ###------------------------
# V1 CNPJ Básico : NÚMERO BASE DE INSCRIÇÃO NO CNPJ (OITO PRIMEIROS DÍGITOS
#DO CNPJ).
# V2 CNPJ ORDEM NÚMERO DO ESTABELECIMENTO DE INSCRIÇÃO NO CNPJ (DO
#NONO ATÉ O DÉCIMO SEGUNDO DÍGITO DO CNPJ)
# V3 CNPJ DV DÍGITO VERIFICADOR DO NÚMERO DE INSCRIÇÃO NO CNPJ (DOIS
#ÚLTIMOS DÍGITOS DO CNPJ)
# V4 IDENTIFICADOR
#MATRIZ/FILIAL CÓDIGO DO IDENTIFICADOR MATRIZ/FILIAL: 1 – MATRIZ 2 – FILIAL
# V5 NOME FANTASIA CORRESPONDE AO NOME FANTASIA
# V6 SITUAÇÃO CADASTRAL CÓDIGO DA SITUAÇÃO CADASTRAL:
#01 – NULA
#2 – ATIVA
#3 – SUSPENSA
#4 – INAPTA
#08 – BAIXADA
# V7 DATA SITUAÇÃO CADASTRAL DATA DO EVENTO DA SITUAÇÃO CADASTRAL
# V8 MOTIVO SITUAÇÃO CADASTRAL CÓDIGO DO MOTIVO DA SITUAÇÃO CADASTRAL
# V9 NOME DA CIDADE NO EXTERIOR 
# V10 PAIS
# V11 DATA DE INÍCIO DA ATIVIDADE
# V12 CNAE FISCAL PRINCIPAL
# V13 CNAE FISCAL SECUNDÁRIA
# V14 TIPO DE LOGRADOURO DESCRICAO DO TIPO DE LOGRADOURO
# V15 LOGRADOURO NOME DO LOGRADOURO ONDE SE LOCALIZA O ESTABELECIMENTO
# V16 NUMERO NÚMERO ONDE SE LOCALIZA O ESTABELECIMENTO. QUANDO NÃO HOUVER 
#PREENCHIMENTO DO NÚMERO HAVERÁ ‘S/N’
# V17 COMPLEMENTO COMPLEMENTO PARA O ENDEREÇO DE LOCALIZAÇÃO DO ESTABELECIMENTO
# V18 BAIRRO
# V19 CEP
# V20 UF
# V21 MUNICIPIO CÓDIGO DO MUNICÍPIO DE JURISDIÇÃO ONDE SE ENCONTRA O 
#ESTABELECIMENTO
# V22 DDD 1
# V23 TELEFONE 1
# V24 DDD 2
# V25 TELEFONE 2
# V26 DDD FAX
# V27 FAX
# V28 CORREIO ELETRONICO CONTEM O EMAIL DO CONTRIBUINTE
# V29 SITUACAO ESPECIAL DA EMPRESA
# V30 DATA DA SITUACAO ESPECIAL

#Definicao dos filtros
filtro_V20 <- "PR"
filtro_V6 <- "02"

#Carregar dados totais
arquivos <-
  c("K3241.K03200Y6.D31111.ESTABELE",
    "K3241.K03200Y7.D31111.ESTABELE",
    "K3241.K03200Y8.D31111.ESTABELE",
    "K3241.K03200Y9.D31111.ESTABELE",
    "K3241.K03200Y0.D31111.ESTABELE",
    "K3241.K03200Y1.D31111.ESTABELE",
    "K3241.K03200Y2.D31111.ESTABELE",
    "K3241.K03200Y3.D31111.ESTABELE",
    "K3241.K03200Y4.D31111.ESTABELE",
    "K3241.K03200Y5.D31111.ESTABELE"
  )

lista <- list()

for (i in 1:length(arquivos)) {
  data <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                     header = FALSE,
                     colClasses = c("character", "character", "character", "character", "character", "character",
                                    "NULL", "NULL", "NULL", "NULL", "character", "character", "NULL", "character",
                                    "character", "character", "character", "character", "character", "character",
                                    "character", "character", "character", "character", "character", "NULL",
                                    "NULL", "character", "NULL", "NULL"),
                     sep = ";",
                     fill = TRUE,
                     encoding = 'ISO-8859',
                     quote = "\""
  )
  data <- subset(data, V20 == filtro_V20 & V6 == filtro_V6)
  
  lista[[i]] <- data
}

cnpj_pr <- 
  do.call("rbind", lista)
rm(lista)

#Comecar por aqui apos passar pelas tabelas ESTABELE
load("%%%%YOUR FOLDER FILE%%%%%/cnpj_pr_ESTABELE.RData")
data <- cnpj_pr
data$estado <- "PR"
data$id_cnpj = data$V1
#save(cnpj_pr, file = "cnpj_pr_ESTABELE.RData")
#write_csv(cnpj_pr, "cnpj_pr_ESTABELE.csv")


# Corrigindo os numeros dos imoveis
data$V16 <- ifelse(data$V16=="000", "", data$V16)
data$V16 <- ifelse(data$V16=="00", "", data$V16)
data$V16 <- ifelse(data$V16=="0", "", data$V16)
data$V16 <- ifelse(data$V16=="SN", "S/N", data$V16)

# Corrigindo o tipo de logradouro
data$V14 <- iconv(data$V14, from = "UTF-8", to = "ASCII//TRANSLIT")
data$V14 <- gsub("^.*\\bRUA\\b.*$", "RUA", data$V14)
data$V14 <- gsub("^.*\\bAVENIDA\\b.*$", "AVENIDA", data$V14)
data$V14 <- gsub("^.*\\bTRAVESSA\\b.*$", "TRAVESSA", data$V14)

# Criando a variavel com o cnpj completo
data$cnpj = paste(data$V1, data$V2, data$V3, sep = "")
data <- subset(data, select = -c(V1, V2, V3))

# Endereco completo
data <- merge(data, cidade, 
              by.x = "V21", by.y = "V1", all.x = TRUE)
data$gps = paste(data$V14, data$V15, sep = " ")
data$gps = paste(data$gps, data$numero_gps, sep = ", ")
data$gps = paste(data$gps, data$V1, sep = "")
data$gps = paste(data$gps, data$V16, sep = "")
data$gps = paste(data$gps, data$V2, sep = " - ")
data$gps = paste(data$gps, data$V20, sep = "/")
data$endereco = paste(data$gps, data$V17, sep = " ")
data$endereco = paste(data$endereco, data$V18, sep = " ")

# Matriz ou filial
data$V4 = ifelse(data$V4==1, "matriz", "filial")

# Transformacao da Data de Atividade
ano <- substr(data$V11,1,4)
mes <- substr(data$V11,5,6)
dia <- substr(data$V11,7,8)
data$data_atividade = paste0(dia,"/",mes,"/",ano)

# Transformando o telefone
data$telefone = paste0("(", data$V22, ")", " ", data$V23)
data$telefone <- ifelse(data$telefone == "() ", "NA", data$telefone)

# Criando variavel botao de whats
data$whats = paste0("https://api.whatsapp.com/send?phone=55", data$V22, 
                    data$V23, "&text=Ol%C3%A1%2C%20tudo%20bom%3F%20Pe%C3%A7o%20licen%C3%A7a%20para%20entrar%20em%20contato...")

# Criando a variavel email
data$email = ifelse(data$V28=="NULL", "NA", data$V28)
data$email <- ifelse(data$V28 == "NULL" | data$V28 == "", "NA", data$V28)

# Criando o botao de busca no google
data$google = paste0("https://www.google.com/search?q=", "cnpj", " ", data$cnpj," ", 
                     data$V5, " ", data$V2, "/", data$V20)

# O telefone e celular?
data$tem_whats <- ifelse(substr(data$V23, 1, 1) %in% c("8", "9"), "SIM", "NAO")

# Selecionando as variaveis, renomeando e apagando o restante
data$matriz = data$V4
data$nome = data$V5
data$estado = data$V20
data$cidade = data$V2
data$ramo = data$V12
data <- data[,c("id_cnpj", "cnpj", "gps", "endereco", "data_atividade", 
                "telefone", "whats", "email", "google", "tem_whats", 
                "matriz", "nome", "estado", "cidade", "ramo")]

# Layout dos Arquivos EMPRESA---------------------------
# V1 CNPJ Básico : NÚMERO BASE DE INSCRIÇÃO NO CNPJ (OITO PRIMEIROS DÍGITOS
#DO CNPJ).
# V2 RAZÃO SOCIAL / NOME EMPRESARIAL NOME EMPRESARIAL DA PESSOA JURÍDICA
# V3 NATUREZA JURÍDICA CÓDIGO DA NATUREZA JURÍDICA
# V4 QUALIFICAÇÃO DO RESPONSÁVEL QUALIFICAÇÃO DA PESSOA FÍSICA RESPONSÁVEL
#PELA EMPRESA
# V5 CAPITAL SOCIAL DA EMPRESA CAPITAL SOCIAL DA EMPRESA
# V6 PORTE DA EMPRESA CÓDIGO DO PORTE DA EMPRESA:
#00 – NÃO INFORMADO
#01 - MICRO EMPRESA
#03 - EMPRESA DE PEQUENO PORTE
#05 - DEMAIS
# V7 ENTE FEDERATIVO RESPONSÁVEL O ENTE FEDERATIVO RESPONSÁVEL É PREENCHIDO
#PARA OS CASOS DE ÓRGÃOS E ENTIDADES DO GRUPO DE NATUREZA JURÍDICA 1XXX. PARA
#AS DEMAIS NATUREZAS, ESTE ATRIBUTO FICA EM BRANCO.
# ----

# Carregar dados totais
arquivos <-
  c(
    "K3241.K03200Y1.D31111.EMPRECSV",
    "K3241.K03200Y2.D31111.EMPRECSV",
    "K3241.K03200Y3.D31111.EMPRECSV",
    "K3241.K03200Y4.D31111.EMPRECSV",
    "K3241.K03200Y5.D31111.EMPRECSV",
    "K3241.K03200Y6.D31111.EMPRECSV",
    "K3241.K03200Y7.D31111.EMPRECSV",
    "K3241.K03200Y8.D31111.EMPRECSV",
    "K3241.K03200Y9.D31111.EMPRECSV",
    "K3241.K03200Y0.D31111.EMPRECSV"
  )

lista <- list()

# Assuming you have a dataset called 'data' and you want to match with its V1 column
V1_values <- data$id_cnpj

for (i in 1:length(arquivos)) {
  data_empresa <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                             header = FALSE,
                             colClasses = c("character", "character", "character", "character", 
                                            "character", "character", "character"),
                             sep = ";",
                             fill = TRUE,
                             encoding = 'latin1',
                             quote = "\""
  )
  
  # Filter rows based on matching V1 values
  data_empresa <- data_empresa[data_empresa$V1 %in% V1_values, ]
  
  lista[[i]] <- data_empresa
}


data_empresa <- 
  do.call("rbind", lista)
rm(lista)
save(data_empresa, file = "cnpj_pr_EMPRECSV.RData")
#load("cnpj_pr_EMPRECSV.RData")
data_empresa$id_cnpj = data_empresa$V1

# Criando uma unica base com todos os dados
data <- merge(data, data_empresa, by = "id_cnpj")
data$razao_social = data$V2

# Substituicao dos Codigos
data <- merge(data, natureza_juridica, 
              by.x = "V3", by.y = "V1", all.x = TRUE)
data <- merge(data, qualificacao, 
              by.x = "V4", by.y = "V1", all.x = TRUE)
data$porte = data$V6

# Montando a base de dados final
# Selecionando as variaveis, renomeando e apagando o restante
data$capital = data$V5
data$capital <- as.numeric(gsub(",", ".", data$capital))
data$qualificacao = data$V2
data$porte = data$V6
data$natureza_juridica = data$V2.y
data <- data[,c("id_cnpj","cnpj", "matriz", "nome", "data_atividade", "ramo", "gps",
                "endereco", "estado", "telefone", "whats", "email",
                "cidade", "google", "tem_whats", "qualificacao",
                "natureza_juridica", "capital", "porte", "razao_social")]

# Layout dos Arquivos SOCIO---------------------------
# V1* CNPJ Básico : NÚMERO BASE DE INSCRIÇÃO NO CNPJ (OITO PRIMEIROS DÍGITOS
#DO CNPJ).
# V2* IDENTIFICADOR DE SÓCIO CÓDIGO DO IDENTIFICADOR DE SÓCIO
#1 – PESSOA JURÍDICA
#2 – PESSOA FÍSICA
#3 – ESTRANGEIRO
# V3* NOME DO SÓCIO (NO CASO PF) OU RAZÃO SOCIAL (NO CASO PJ) NOME DO SÓCIO
#PESSOA FÍSICA OU A RAZÃO SOCIAL E/OU NOME EMPRESARIAL DA PESSOA JURÍDICA 
#E/OU NOME DO SÓCIO/RAZÃO SOCIAL DO SÓCIO ESTRANGEIRO
# V4 CNPJ/CPF DO SÓCIO CPF OU CNPJ DO SÓCIO (SÓCIO ESTRANGEIRO NÃO TEM ESTA
#INFORMAÇÃO)
# V5* QUALIFICAÇÃO DO SÓCIO CÓDIGO DA QUALIFICAÇÃO DO SÓCIO
# V6 DATA DE ENTRADA SOCIEDADE DATA DE ENTRADA NA SOCIEDADE
# V7* PAIS CÓDIGO PAÍS DO SÓCIO ESTRANGEIRO
# V8 REPRESENTANTE LEGAL NÚMERO DO CPF DO REPRESENTANTE LEGAL
# V9* NOME DO REPRESENTANTE NOME DO REPRESENTANTE LEGAL
# V10 QUALIFICAÇÃO DO REPRESENTANTE LEGAL CÓDIGO DA QUALIFICAÇÃO DO 
#REPRESENTANTE LEGAL
# V11* FAIXA ETÁRIA CÓDIGO CORRESPONDENTE À FAIXA ETÁRIA DO SÓCIO
# ----

# Carregar dados totais
arquivos <-
  c(
    "K3241.K03200Y0.D31111.SOCIOCSV",
    "K3241.K03200Y1.D31111.SOCIOCSV",
    "K3241.K03200Y2.D31111.SOCIOCSV",
    "K3241.K03200Y3.D31111.SOCIOCSV",
    "K3241.K03200Y4.D31111.SOCIOCSV",
    "K3241.K03200Y5.D31111.SOCIOCSV",
    "K3241.K03200Y6.D31111.SOCIOCSV",
    "K3241.K03200Y7.D31111.SOCIOCSV",
    "K3241.K03200Y8.D31111.SOCIOCSV",
    "K3241.K03200Y9.D31111.SOCIOCSV"
  )

lista <- list()

# Assuming you have a dataset called 'data' and you want to match with its V1 column
V1_values <- data$id_cnpj

for (i in 1:length(arquivos)) {
  data_socio <- read.table(file = paste('%%%%YOUR FOLDER FILE%%%%%/', arquivos[[i]], sep = ""),
                           header = FALSE,
                           colClasses = c("character", "character", "character", "character", 
                                          "character", "character", "character", "character", 
                                          "character", "character", "character"),
                           sep = ";",
                           fill = TRUE,
                           encoding = 'latin1',
                           quote = "\""
  )
  
  # Filter rows based on matching V1 values
  data_socio <- data_socio[data_socio$V1 %in% V1_values, ]
  
  lista[[i]] <- data_socio
}

data_socio <- 
  do.call("rbind", lista)
rm(lista)
save(data_socio, file = "cnpj_pr_SOCIOCSV.RData")
#load("data_socio_original.RData")
data_socio$id_cnpj = data_socio$V1

# Criando uma unica base com todos os dados
data <- merge(data, data_socio, by = "id_cnpj")

# Substituicao dos Codigos
data$tipo_socio = data$V2
data$tipo_socio <- ifelse(data$V2 == "1", "pessoa_juridica",
                          ifelse(data$V2 == "2", "pessoa_fisica",
                                 ifelse(data$V2 == "3", "estrangeiro", data$tipo_socio)))
data <- merge(data, qualificacao, 
              by.x = "V5", by.y = "V1", all.x = TRUE)
data <- merge(data, pais, 
              by.x = "V7", by.y = "V1", all.x = TRUE)
data$faixa_etaria = data$V11
data$faixa_etaria <- ifelse(data$V11 == "0", "Nao_se_aplica",
                            ifelse(data$V11 == "1", "0 a 12",
                                   ifelse(data$V11 == "2", "13 a 20",
                                          ifelse(data$V11 == "3", "21 a 30",
                                                 ifelse(data$V11 == "4", "31 a 40",
                                                        ifelse(data$V11 == "5", "41 a 50",
                                                               ifelse(data$V11 == "6", "51 a 60",
                                                                      ifelse(data$V11 == "7", "61 a 70",
                                                                             ifelse(data$V11 == "8", "71 a 80",
                                                                                    ifelse(data$V11 == "9", "Mais_de_80", data$faixa_etaria))))))))))


# Transformacao da Data de Entrada
ano <- substr(data$V6,1,4)
mes <- substr(data$V6,5,6)
dia <- substr(data$V6,7,8)
data$data_entrada_socio = paste0(dia,"/",mes,"/",ano)

# Montando a base de dados final
# Selecionando as variaveis, renomeando e apagando o restante
data$nome_socio = data$V3
data$rep_socio = data$V9
data$pais_socio = data$V2
data$qualificacao_socio = data$V2.y

# Colocando os ramos/cnae
data$V1 = data$ramo
data <- merge(data, cnae, 
              by.x = "ramo", by.y = "V1", all.x = TRUE)
data$ramo_nome = data[,41]

# Substituting values in 'porte'
data$porte <- paste("#", data$porte, sep = "")

# Create a new variable 'new_category' with the updated values
data$new_category <- replace(data$porte, data$porte == "#01", "MICRO")
data$new_category <- replace(data$new_category, data$porte == "#03", "PEQUENA")
data$new_category <- replace(data$new_category, data$porte == "#05", "GRANDE/OUTROS")

# If you want to handle other cases or use NA for unmatched values
data$new_category <- replace(data$new_category, !(data$porte %in% c("#01", "#03", "#05")), NA_character_)

# Assign the updated 'porte2' values to 'porte'
data$porte = data$new_category

data <- data[,c("cnpj", "matriz", "nome", "data_atividade", "ramo", "gps",
                "endereco", "estado", "telefone", "whats", "email",
                "cidade", "google", "tem_whats", "qualificacao",
                "natureza_juridica", "capital", "tipo_socio", "razao_social",
                "data_entrada_socio", "qualificacao_socio", "nome_socio",
                "pais_socio", "rep_socio", "faixa_etaria", "porte", "id_cnpj")]

#Contando a frequencia do ramo
data$contagem <- table(data$ramo)[as.character(data$ramo)]

data$data_atividade <- paste("#", data$data_atividade, sep = "")
data$cnpj <- paste("#", data$cnpj, sep = "")
data$id_cnpj <- paste("#", data$id_cnpj, sep = "")
data$ramo <- paste("#", data$ramo, sep = "")
data$capital <- paste("#", format(data$capital, scientific = FALSE), sep = "")
data$data_entrada_socio <- paste("#", data$data_entrada_socio, sep = "")
data$contagem <- paste("#", data$contagem, sep = "")

# Removendo os acentos latinos
char_cols <- sapply(data, is.character)

# Apply the transformation to each character column
data[, char_cols] <- lapply(data[, char_cols], function(x) stri_trans_general(x, "Latin-ASCII"))

write_csv(data, "cnpj_pr.csv")
save(data, file = "cnpj_pr.RData")
#load("%%%%YOUR FOLDER FILE%%%%%/cnpj_pr.RData")
