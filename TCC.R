#unindo as tabela dentro e fora do prazo
dentrodoprazov<-gather(dentrodoprazo,"cnae","saldo",2:23)
foradoprazov<-gather(foradoprazo,"cnae","saldo",2:23)


caged<-merge.data.frame(dentrodoprazo,foradoprazo,all=TRUE)


caged<-caged %>% 
  group_by(cnae,competencia) %>% 
  summarise(saldo = sum(saldo))



 caged<-spread(caged,cnae,saldo)
 
 
 caged<-gather(caged,"cnae","saldo",2:23)
 
 
a<-lapply(caged,function(x) FUN=plot.ts(x,frequency=12, start=c(2007,01)))

lapply(caged[2:5], function(x)  plot.ts(x, freq=12, start=c(2007,01), end=c(2019,12)))
