########
# mar/2017
# Esse codigo analise os dados de lancamentos imobiliarios
# author: Joao Meirelles
########

#####################
#### SET WD #########
#####################
setwd("/home/jm/DATABASE-RJ/2017.03---Emprego---Rio")
#####################

#####################
####Load Packages####
#####################
library(ggplot2)
library(plyr)
library("readxl")
#####################

#######################
#####Load Data#########
#######################

#cria um vetor com as sheets do arquivo
sheets <- excel_sheets("./data/3175_cagedjan17.XLS")


salta_emprego <- data.frame(matrix(ncol = 28, nrow = 0))
colnames(salta_emprego) <- c("data", 
                                        "total_admitidos", "total_desligados", "total_saldo",
                                        "extrativa_mineral_admitidos", "extrativa_mineral_desligados", "extrativa_mineral_saldo",
                                        "ind_transformacao_admitidos", "ind_transformacao_desligados", "ind_transformacao_saldo",
                                        "serv_util_publica_admitidos", "serv_util_publica_desligados", "serv_util_publica_saldo",
                                        "construcao_civil_admitidos", "construcao_civil_desligados", "construcao_civil_saldo",
                                        "comercio_admitidos", "comercio_desligados", "comercio_saldo",
                                        "servicos_admitidos", "servicos_desligados", "servicos_saldo",
                                        "adm_publica_admitidos", "adm_publica_desligados", "adm_publica_saldo",
                                        "agropecuaria_admitidos", "agropecuaria_desligados", "agropecuaria_saldo"
                                        )  


ex = c(1, 14, 27, 40, 53, 66, 79, 92, 105, 118, 131, 144, 157, 170, 183, 196, 209, 222)

data <- as.data.frame(seq(as.Date("2000/1/1"), as.Date("2017/1/1"), by = "month"))
colnames(data) <- "data"
                 
for(sheet in sheets) 
{ print(sheet)
  if (sheet != "Total"){
   d <- read_excel("./data/3175_cagedjan17.XLS", sheet = sheet, skip=6)
   d <- d[-ex,]
   d <- d[c(0:205),] 
  
    colnames(d) <- c("data", paste (sheet, "_admitidos"), paste (sheet, "_desligados"), paste (sheet, "_saldo"))
    data <- cbind(data, d)
  }
}

emprego_saldo <- data[,-c(2,6,10,14,18,22,26,30)]
emprego_saldo$data <- seq(as.Date("2000/1/1"), as.Date("2017/1/1"), by = "month")
##########


########################
######### VIZ ##########
########################

# Palette
LtoM <-colorRampPalette(c('red', 'yellow' ))
Mid <- "snow3"
MtoH <-colorRampPalette(c('lightgreen', 'darkgreen'))

#plot
ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,13], fill=emprego_saldo[,13])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("DATA")+ylab("SALDO DE CONTRATAÇÃO")+ggtitle("CONSTRUÇÃO CIVIL")+ theme(legend.position="none")
ggsave("./plots/post.png", width = 20, height = 10, units = "cm")

ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,4], fill=emprego_saldo[,4])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Extrativa Mineral")+ theme(legend.position="none")
ggsave("./plots/extrativa_mineral.png", width = 15, height = 10, units = "cm")

ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,7], fill=emprego_saldo[,7])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Industria de Transformação")+ theme(legend.position="none")
ggsave("./plots/industria_de_transformacao.png", width = 15, height = 10, units = "cm")


ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,10], fill=emprego_saldo[,10])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Servico de Utilidade Pública")+ theme(legend.position="none")
ggsave("./plots/servico_de_utilidade_publica.png", width = 15, height = 10, units = "cm")


ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,13], fill=emprego_saldo[,13])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Construção Civil")+ theme(legend.position="none")
ggsave("./plots/construção_civil.png", width = 15, height = 10, units = "cm")

ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,16], fill=emprego_saldo[,16])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Comércio")+ theme(legend.position="none")
ggsave("./plots/comercio.png", width = 15, height = 10, units = "cm")


ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,19], fill=emprego_saldo[,19])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Serviços")+ theme(legend.position="none")
ggsave("./plots/servicos.png", width = 15, height = 10, units = "cm")


ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,22], fill=emprego_saldo[,22])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Administração Pública")+ theme(legend.position="none")
ggsave("./plots/extrativa_mineral.png", width = 15, height = 10, units = "cm")

ggplot(data = emprego_saldo, aes(x = emprego_saldo[,1], y = emprego_saldo[,25], fill=emprego_saldo[,25])) +
  geom_bar(stat = 'identity')+ 
  scale_fill_gradient2(low=LtoM(100), mid='snow3', high=MtoH(100), space='Lab')+
  theme_bw()+xlab("data")+ylab("Agropecuária")+ggtitle("Agropecuária")+ theme(legend.position="none")
ggsave("./plots/agropecuaria.png", width = 15, height = 10, units = "cm")






## lines ##

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,4]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Extrativa Mineral")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,7]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Industria de Transformação")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,10]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Servico de Utilidade Pública")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,13]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Construção Civil")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,16]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Comércio")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,19]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Serviços")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,22]))+ 
  theme_bw()+xlab("data")+ylab("saldo de contratações")+ggtitle("Administração Pública")

ggplot(emprego_saldo)+
  geom_line(aes(emprego_saldo[,1],emprego_saldo[,25]))+ 
  theme_bw()+xlab("data")+ylab("Agropecuária")+ggtitle("Agropecuária")

