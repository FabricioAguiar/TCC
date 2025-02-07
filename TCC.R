library(readr)
library(magrittr)
library(tidyverse)
dentrodoprazo <- read_delim("C:/Users/faabr/Downloads/TCC/dentrodoprazo.csv", 
                            ";", escape_double = FALSE, col_types = cols(competencia = col_date(format = "%d/%m/%Y")), 
                            locale = locale(encoding = "ISO-8859-1"), 
                            trim_ws = TRUE)

foradoprazo <- read_delim("C:/Users/faabr/Downloads/TCC/foradoprazo.csv", 
                            ";", escape_double = FALSE, col_types = cols(competencia = col_date(format = "%d/%m/%Y")), 
                            locale = locale(encoding = "ISO-8859-1"), 
                            trim_ws = TRUE)


caged<-merge.data.frame(dentrodoprazo,foradoprazo,all=TRUE)

caged<-gather(caged,"cnae","saldo",2:23)

caged<-caged %>% 
  group_by(competencia,cnae) %>% 
  summarise(saldo = sum(saldo))

caged<-spread(caged,cnae,saldo)

series<-ts(caged[2:23],frequency=12, start=c(2007,01), end=c(2019,12))
series<-lapply(caged[2:23], function(x) FUN= ts(x,frequency=12, start=c(2007,01), end=c(2019,12)))

series$A

decomposicao<-lapply(series, function(x) FUN= decompose(x,type ="additive", filter=NULL))




plot.ts(ts(caged[2:3],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[4:5],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[6:7],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[8:9],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[10:11],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[12:13],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[14:15],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[16:17],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[18:19],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[20:21],frequency=12, start=c(2007,01), end=c(2019,12)), main="")
plot.ts(ts(caged[22:23],frequency=12, start=c(2007,01), end=c(2019,12)), main="")

plot.ts(ts(A[3:10],frequency=12, start=c(2007,01), end=c(2019,12)), main="")

lapply(caged[2:23], function(x) FUN=plot.ts(x,frequency=12, start=c(2007,01), end=c(2019,12)))
ts.plot(A$F, A$G,A$H,A$I,A$J, gpars=list(xlab='',ylab='',lty=1:1))

Alibrary(forecast)
library(fpp2)
library(readxl)

ggseasonplot(decomposicao[["A"]][["x"]], main="Agricultura")
seasonal::
  seas

plot(A$A,main='Agricultura',
     ylab='Saldo', xlab = 'Ano',
     bty='l',col='red',lty=1)
grid(col='grey',
     lwd=2)

Q <- ordered(cycle(A$A))
pib.reg <- lm(A$A~Q)

summary(pib.reg)
levels(factor(caged$cnae))

d<-lapply(caged, function(x) FUN= decompose(x,type ="multiplicative", filter=NULL))

lapply(decomposicao, function(x) FUN=plot(x,main=x))
figure<-c('jan', 'fev', 'mar', 'abr','mai','jun','jul','ago','set','out','nov','dez')
m�s<-c(figure)
m�s<-as.data.frame(m�s)
m�s$A=decomposicao[["A"]][["figure"]]
m�s$B=decomposicao[["B"]][["figure"]]
m�s$C=decomposicao[["C"]][["figure"]]
m�s$D=decomposicao[["D"]][["figure"]]
m�s$E=decomposicao[["E"]][["figure"]]
m�s$F=decomposicao[["F"]][["figure"]]
m�s$G=decomposicao[["G"]][["figure"]]
m�s$H=decomposicao[["H"]][["figure"]]
m�s$I=decomposicao[["I"]][["figure"]]
m�s$J=decomposicao[["J"]][["figure"]]
m�s$K=decomposicao[["K"]][["figure"]]
m�s$L=decomposicao[["L"]][["figure"]]
m�s$M=decomposicao[["M"]][["figure"]]
m�s$N=decomposicao[["N"]][["figure"]]
m�s$O=decomposicao[["O"]][["figure"]]
m�s$P=decomposicao[["P"]][["figure"]]
m�s$Q=decomposicao[["Q"]][["figure"]]
m�s$R=decomposicao[["R"]][["figure"]]
m�s$S=decomposicao[["S"]][["figure"]]
m�s$T=decomposicao[["T"]][["figure"]]
m�s$U=decomposicao[["U"]][["figure"]]


sumfigure<-c()
sumfigure<-as.data.frame(sumfigure)
sumfigure$A=prod(figure$A)
sumfigure$B=prod(figure$B)
sumfigure$C=prod(figure$C)
sumfigure$D=prod(figure$D)
sumfigure$E=prod(figure$E)
sumfigure$F=prod(figure$F)
sumfigure$G=prod(figure$G)
sumfigure$H=prod(figure$H)
sumfigure$I=prod(figure$I)
sumfigure$J=prod(figure$J)
sumfigure$K=prod(figure$K)
sumfigure$L=prod(figure$L)
sumfigure$M=prod(figure$M)
sumfigure$N=prod(figure$N)
sumfigure$O=prod(figure$O)
sumfigure$P=prod(figure$P)
sumfigure$Q=prod(figure$Q)
sumfigure$R=prod(figure$R)
sumfigure$S=prod(figure$S)
sumfigure$T=prod(figure$T)
sumfigure$U=prod(figure$U)


random$competencia = caged$competencia

plot(decomposicao$A, main= 'A')

write.table(figure,"figure.csv",
            
            sep=";", row.names = F,dec = ",", na = "")

attach(decomposicao[["A"]])

A<-print((rbind(decomposicao$A)))
A<-print(cbind(decomposicao[["A"]][["x"]],decomposicao[["A"]][["trend"]],decomposicao[["A"]][["random"]],decomposicao[["A"]][["seasonal"]]))
print(cbind(decomposicao))
dec<-c()
d<-lapply(decomposicao, function(x) FUN= print(decomposicao[[]][["trend"]]))

library(readr)
raisrr <- read_delim("C:/Users/faabr/Downloads/raisrr.csv", 
                     ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
View(raisrr)
      


RAIS<-gather(raisrr,"ANO","ESTOQUE",3:14)
raisssss<-RAIS %>% dplyr::select(ANO,desc_cnae,ESTOQUE)

ggplot(data=raisssss, aes(x=ANO, y=ESTOQUE )) + 
  geom_area(
    mapping =aes (y=ESTOQUE ) ,
    data = NULL,
    stat = "identity",
    position = "stack",
    na.rm = FALSE,
    show.legend = TRUE,
    inherit.aes = TRUE)

ggplot(raisssss,aes(x=ANO, y=ESTOQUE, fill=desc_cnae)) + 
  geom_area()
 raisssss$desc_cnae <-factor(raisssss$desc_cnae)
 
 ggplot(raisssss,aes(x=ANO, y=ESTOQUE, fill=desc_cnae)) + 
   geom_area(alpha=0.6 , size=.5, colour="white") +
   ggtitle("The race between ...")
 
 
 x <- xtabs(~desc_cnae +ANO, data = raisssss)
 cols <- c("#660d32", "#bc1a5e")
 
 # Barras empilhadas.
 barplot(x,
         beside = FALSE,
         xlab = "Ano",
         ylab = "Frequ�ncia absoluta",
         col = cols)
 
 qw = xts(raisssss, order.by=)
 dbgg = data.frame(time = index(dbgg), melt(as.data.frame(dbgg)))
 colnames(dbgg) = c('time', 'Indexador', 'value')
 ## Gerar gr�fico
 theme_set(theme_bw())
 
 ggplot(raisssss, aes(x = ANO, y = ESTOQUE)) + 
   geom_area(aes(colour = desc_cnae, fill = desc_cnae))
 
 
 labs(title='Indexadores da D�vida Bruta brasileira')
 
 plot=ggplot(raisssss, aes(x = ANO, y = ESTOQUE,fill = desc_cnae))
 
 plot+
   geom_area(colour="black", size=.2, alpha=.8) +
   theme_bw()
 
 install.packages("ploty")
 library(plotly)
 
 ggplot(figure, aes(x = mes, y = A)) + 
   geom_bar(colour="black" ,stat="sum")
 