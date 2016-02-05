setwd("~/Documentos/desafio_PD_datasets/")
require(jsonlite)
require(plyr)
require(rpart)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)

options(digits = 4)

############# Lendo os arquivos ##################

frie_conn <- fromJSON("friend_connection.json")
stu_agg <- fromJSON("student_aggregate.json")
stu_down <- fromJSON("student_download.json")
frie_resq <- fromJSON("friend_request.json")
stu_ans <- fromJSON("student_answer.json")
stu_com <- fromJSON("student_comment.json")
stu_eval <- fromJSON("student_evaluation.json")
stu_upl <- fromJSON("student_file_upload.json")
stu_log <- fromJSON("student_loginlog_aggreagate.json")
stu_ques <- fromJSON("student_question.json")

############# gerando DF from list

frie_conn <- as.data.frame(lapply(frie_conn$friend_connection,cbind))
stu_down <- as.data.frame(lapply(stu_down$student_download,cbind))
frie_resq <- as.data.frame(lapply(frie_resq$friendrequest,cbind))
stu_ans <- as.data.frame(lapply(stu_ans$student_answer,cbind))
stu_com <- as.data.frame(lapply(stu_com$student_comment,cbind))
stu_eval<-as.data.frame(lapply(stu_eval$student_evaluation,cbind))
stu_upl <- as.data.frame(lapply(stu_upl$student_file_upload,cbind))
stu_log <- as.data.frame(lapply(stu_log$student_loginlog_aggreagate,cbind))
stu_ques <- as.data.frame(lapply(stu_ques$student_question,cbind))

########### Estatística descritiva dados agregados
stu_agg$ESTADO<-toupper(stu_agg$stateName)
regiao<-read.csv(file = "estado_regiao_brasil.csv",sep=";")
stu_agg<-merge(x = stu_agg, y = regiao, by="ESTADO",all.x = TRUE)
stu_agg<-stu_agg[,-1]

estados <- count(stu_agg,"stateName")
estados$rel <- prop.table(estados$freq)
estados <- estados[order(estados$rel,decreasing = TRUE),]
barplot(estados$rel,names.arg = 1:length(estados$rel),ylim = c(0,0.3),main="% de usuários por estado")
head(estados,10)

universidade <- count(stu_agg,"UniversityName")
universidade$rel <- prop.table(universidade$freq)
universidade <- universidade[order(universidade$rel,decreasing = TRUE),]
barplot(universidade$rel,names.arg = 1:length(universidade$rel),ylim = c(0,0.3))
head(universidade,10)



########### Analise no tempo
tempo_upload <- as.Date(stu_upl$LastLoginDate)-as.Date(stu_upl$RegisteredDate)
tempo_down <- as.Date(stu_down$LastLoginDate)-as.Date(stu_down$RegisteredDate)
tempo_com <- as.Date(stu_com$LastLoginDate)-as.Date(stu_com$RegisteredDate)
tempo_ans <- as.Date(stu_ans$LastLoginDate)-as.Date(stu_ans$RegisteredDate)
tempo_ques <- as.Date(stu_ques$LastLoginDate)-as.Date(stu_ques$RegisteredDate)
tempo_eval <- as.Date(stu_eval$LastLoginDate)-as.Date(stu_eval$RegisteredDate)

mean(tempo_upload)
mean(tempo_down)
mean(tempo_com)
mean(tempo_ans)
mean(tempo_ques)
mean(tempo_eval)

summary(as.numeric(tempo_down))

par(mfrow=(c(2,3)))
hist(as.numeric(tempo_upload),main = "Numero de cliente que fizeram uploads por \n tempo de registro",
     xlab = "Tempo de registro",col = "gray")
hist(as.numeric(tempo_down),main = "Numero de cliente que fizeram downloads por \n  tempo de registro",
     xlab = "Tempo de registro",col = "gray")
hist(as.numeric(tempo_com),main = "Numero de cliente que fizeram comentários por \n tempo de registro",
     xlab = "Tempo de registro",col = "gray")
hist(as.numeric(tempo_ans),main = "Numero de cliente que responderam por \n tempo de registro",
     xlab = "Tempo de registro",col = "gray")
hist(as.numeric(tempo_ques),main = "Numero de cliente que questionaram por \n tempo de registro",
     xlab = "Tempo de registro",col = "gray")
hist(as.numeric(tempo_eval),main = "Numero de cliente que curtiram por \n tempo de registro",
     xlab = "Tempo de registro",col = "gray")


############ Frequencia de ocorrencia

qtd_down<-count(stu_down,"Id")
names(qtd_down)[2]<-"qtd_down"
qtd_ans<- count(stu_ans,"Id")
names(qtd_ans)[2]<-"qtd_ans"
qtd_com<- count(stu_com,"Id")
names(qtd_com)[2]<-"qtd_com"
qtd_eval<- count(stu_eval,"Id")
names(qtd_eval)[2]<-"qtd_eval"
qtd_upl<- count(stu_upl,"Id")
names(qtd_upl)[2]<-"qtd_upl"
qtd_log<- count(stu_log,"Id")
names(qtd_log)[2]<-"qtd_log"
qtd_ques<- count(stu_ques,"Id")
names(qtd_ques)[2]<-"qtd_ques"


############ Add info ao banco agregado

stu_agg<-merge(x = stu_agg,y = qtd_down,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_ans,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_com,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_eval,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_upl,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_log,by = "Id",all.x = TRUE)
stu_agg<-merge(x = stu_agg,y = qtd_ques,by = "Id",all.x = TRUE)
stu_agg[is.na(stu_agg)] <- 0

############# Criando tabela por sexo, estado, regiao, universidade

info_agg<-stu_agg[,c(3,5,6,7,8,20)]
cont_agg<-stu_agg[,c(21,22,23,24,25,26)]
cont_agg<-apply(cont_agg,2,as.numeric)
Contagem_agg<-cbind(info_agg,cont_agg)


por_estado<-aggregate(cont_agg,by = list(info_agg$stateName),FUN = mean,na.rm=TRUE)
por_universidade<-aggregate(cont_agg,by = list(info_agg$UniversityName),FUN = mean,na.rm=TRUE)
por_sexo<-aggregate(cont_agg,by = list(info_agg$Gender),FUN = mean,na.rm=TRUE)
por_Regiao<-aggregate(cont_agg,by = list(info_agg$REGIAO),FUN = mean,na.rm=TRUE)


############## Analise de agrupamento dos maiores produtores

### Criando variavel indicadora para as regioes

stu_agg$isSuldeste<-ifelse(stu_agg$REGIAO == "SUDESTE", 1 ,0) 
stu_agg$isSul<-ifelse(stu_agg$REGIAO == "SUL", 1 ,0)
stu_agg$isNorte<-ifelse(stu_agg$REGIAO == "NORTE", 1 ,0)
stu_agg$isNordeste<-ifelse(stu_agg$REGIAO == "NORDESTE", 1 ,0)
stu_agg$isCentro<-ifelse(stu_agg$REGIAO == "CENTRO-OESTE", 1 ,0)

###### Dataset pra kmeans

maior_produtor<-stu_agg[,-c(1:20)]
maior_produtor[is.na(maior_produtor)] <- 0

###### Determinando numero de clusters
wss <- (nrow(maior_produtor)-1)*sum(apply(maior_produtor,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(maior_produtor, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Clusters",
     ylab="Soma dos quadrados entre grupos")

###### K-Means Clusters 
fit <- kmeans(maior_produtor, 8) # 8 cluster 
# cluster means 
aggregate(maior_produtor,by=list(fit$cluster),FUN=mean)
# add cluster ao dado
maior_produtor <- data.frame(maior_produtor, fit$cluster)

