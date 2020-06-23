########################################################
#         Kmeans cluster Algorithm - Hackaton          #
########################################################

### Pacotes
library(RPostgreSQL)
library(caret)
library(factoextra)

### Conexão com o banco de dados
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv,
                 dbname = "equipe701",
                 host = "bd.inova.serpro.gov.br",
                 port = 5433,
                 user = "equipe701",
                 password =  "CWVxdWlwZTcwMQk3ZGQ4")

########################################################
# Kmeans cluster Selects                               #
########################################################

dfEstado = dbGetQuery(con, statement = paste(
  "SELECT estado.id_estado,
    estado.sigla,
    max(atualizacao.data) as data,
    sum(atualizacao.novos_casos) as casos,
    sum(atualizacao.novos_obitos) as obitos,
    (sum(novos_obitos::real) / sum(novos_casos::real)) as taxa,
    sum(cidade.populacao) as populacao_estimada",
  "FROM atualizacao
    join cidade on atualizacao.id_cidade = cidade.id_cidade
    join estado on cidade.id_estado = estado.id_estado",
  "GROUP BY estado.id_estado, estado.sigla",
  "ORDER BY estado.sigla"))

dfCidade = dbGetQuery(con, statement = paste(
  "SELECT cidade.id_cidade,
    max(atualizacao.data) as data,
    sum(atualizacao.novos_casos) as casos,
    sum(atualizacao.novos_obitos) as obitos,
	case sum(novos_casos)
		when 0 then 0
		else (sum(novos_obitos::real) / sum(novos_casos::real))
  	end as taxa,
    cidade.populacao as populacao_estimada",
  "FROM atualizacao
    join cidade on atualizacao.id_cidade = cidade.id_cidade",
  "GROUP BY cidade.id_cidade",
  "ORDER BY cidade.id_estado, cidade.id_cidade"))

#########################################################
# Insert na tb.cluster_estado                           #
#########################################################

dfClusterEstado <- data.frame(data = as.character(unique(dfEstado$data)))
queryClusterEstado <- DBI::sqlAppendTable(con = con, table = 'cluster_estado', values = dfClusterEstado, row.names = FALSE)
dbExecute(conn = con , statement = queryClusterEstado)

#########################################################
# Insert na tb.cluster_estado_item                      #
#########################################################

dfClusterEstado <- dbGetQuery(con, statement = paste(
  "SELECT id_cluster_estado",
  "FROM cluster_estado"))

### Nivelamento da escala dos dados
dfAjust <- preProcess(dfEstado[,c(4:7)], method=c("center", "scale"))
dfAjust <- predict(dfAjust, dfEstado[,c(4:7)])

### Ajuste da tabela
dfAjust <- cbind(id_estado = dfEstado$id_estado, dfAjust)

### Rodando o modelo de KMeans
for (i in 2:ncol(dfAjust)) {
  dfAjust[,i] <- ifelse(is.na(dfAjust[,i]), 0, dfAjust[,i])
}

result <- eclust(dfAjust[,c(2:5)], 'kmeans', nstart = 25, k = 4)

### Criando data.frame para subir na tabela
dfResult <- cbind(id_estado = dfAjust$id_estado, result$clust_plot$data[,c(2:3,5)],
                  id_cluster_estado <- dfClusterEstado$id_cluster_estado)
dfResult <- dfResult[,c(1,5,2:4)]

### Função para carregar os dados na tabela
query <- DBI::sqlAppendTable(con = con, table = 'cluster_estado_item', values = dfResult, row.names = FALSE)
dbExecute(conn = con , statement = query)

#########################################################
# Insert na tb.cluster_cidade                           #
#########################################################

dfClusterCidade <- data.frame(data = as.character(unique(dfCidade$data)))
queryClusterCidade <- DBI::sqlAppendTable(con = con, table = 'cluster_cidade', values = dfClusterCidade, row.names = FALSE)
dbExecute(conn = con , statement = queryClusterCidade)

#########################################################
# Insert na tb.cluster_cidade_item                      #
#########################################################

dfClusterCidade <- dbGetQuery(con, statement = paste(
  "SELECT id_cluster_cidade",
  "FROM cluster_cidade"))

### Nivelamento da escala dos dados
dfAjust2 <- preProcess(dfCidade[,c(3:6)], method=c("center", "scale"))
dfAjust2 <- predict(dfAjust2, dfCidade[,c(3:6)])

### Ajuste da tabela
dfAjust2 <- cbind(id_cidade = dfCidade$id_cidade,dfAjust2)

### Rodando o modelo de KMeans
for (i in 2:ncol(dfAjust2)) {
  dfAjust2[,i] <- ifelse(is.na(dfAjust2[,i]), 0, dfAjust2[,i])
}

result2 <- eclust(dfAjust2[,c(2:5)], 'kmeans', nstart = 25, k = 5)

### Criando data.frame para subir na tabela
dfResult2 <- cbind(id_cidade = dfAjust2$id_cidade, result2$clust_plot$data[,c(2:3,5)],
                   id_cluster_cidade = dfClusterCidade$id_cluster_cidade)
dfResult2 <- dfResult2[,c(1,5,2:4)]

### Função para carregar os dados na tabela
query2 <- DBI::sqlAppendTable(con = con, table = 'cluster_cidade_item', values = dfResult2, row.names = FALSE)
dbExecute(conn = con , statement = query2)

#########################################################
# Desconectar do banco                                  #
#########################################################

dbDisconnect(con)
