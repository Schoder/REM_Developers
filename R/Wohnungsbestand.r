setwd("D:\\OneDrive - ISBA - Internationale Studien- und Berufsakademie gGmbH\\Lehrveranstaltungen\\Immo\\Einführung_Immo\\EigeneMaterialien\\Immobilienwirtschaft_als_Branche\\Daten")
imageDirectory<-"D:\\OneDrive - ISBA - Internationale Studien- und Berufsakademie gGmbH\\Lehrveranstaltungen\\Immo\\Einführung_Immo\\EigeneMaterialien\\Folien\\Intake_2020A\\images"

saveInImageDirectory<-function(filename){
  imageFile <- file.path(imageDirectory, filename)
  ggsave(imageFile, width=8,height=5)  
}

#install.packages('tidyselect',dep=T)
library(openxlsx)
library(tidyverse)
library(tidyselect)
library(patchwork)
library(lubridate)
library(scales)
library(reshape2)

#Wohnungsbestand

#Datensätze laden
#Wohnungen
wohn.wgb.data<-read.xlsx('31231-0001_Wohnungsbestand_Wohngebäude_(Destatis_2020).xlsx',sheet = 1)
wohn.tot.data<-read.xlsx('31231-0002_Wohnungsbestand_gesamt_(Destatis_2020).xlsx',sheet = 1)
wohn.tot.BL<-read.xlsx('31231-0004_Wohnungsbestand_gesamt_BL_(Destatis_2020).xlsx',sheet = 1)
#Bevölkerung
bev.data<-read.csv('12411-0001_Bevölkerung_seit 1950_flat.csv',sep = ';')
hh.data<-read.xlsx('haushalte-familien-2010300197005.xlsx',sheet = 'Tabelle_1_5_0')



#####################################  
#####################################  
########   Data Wrangling   #########
#####################################    
##################################### 
year.list<-c(1995:2019)

##############
#Wohngebäude
##############
wohn.plot<-wohn.wgb.data%>%
      rename(Jahr=names(wohn.wgb.data)[1]) %>%
      select(Jahr,X15,X16) %>%
      rename(WE.WG=X15,WF.WG=X16) %>%
      mutate(Jahr=year(dmy(Jahr)),WE.WG=as.numeric(WE.WG)/1000000,WF.WG=as.numeric(WF.WG)/1000)%>%
      filter(Jahr%in%year.list&!is.na(WE.WG)) 

class(wohn.plot$Jahr)
class(wohn.plot$WE.WG)

#####
#Gesamt: Wohn- und Nichtwohngebäude
#####
wohn.temp<-wohn.tot.data%>%
  rename(Jahr=names(wohn.tot.data)[1],WE.tot=X9) %>%
  select(Jahr,WE.tot) %>%
  mutate(Jahr=year(dmy(Jahr)),WE.tot=as.numeric(WE.tot)/1000000)%>%
  filter(Jahr%in%year.list&!is.na(WE.tot))

class(wohn.temp$WE.tot)

#Mergen und Wachstumsrate der Wohneinheiten berechnen
wohn.plot<-left_join(wohn.temp,wohn.plot)%>%
  mutate(diff.t = Jahr-lag(Jahr),  # Zeitintervall (just in case there are gaps)
         diff.niv = WE.tot - lag(WE.tot), # Niveauveränderung
         g.rate = (diff.niv/diff.t)/lag(WE.tot)*100) # Wachstumsrate
#Reshape
wohn.long<-wohn.plot%>%
  melt(id.vars = c('Jahr','g.rate')) %>%
  mutate(Merkmal=as.factor(variable),Wert=as.numeric(value))


#pivot_longer(!Jahr,names_to='Merkmal',values_to='Wert',values_drop_na = TRUE)

class(wohn.long$Wert)


######
#Räume
######
wohn.rooms.temp<-wohn.tot.BL%>%
  rename(Land=names(wohn.tot.BL)[1]) %>%
  mutate(R1=as.numeric(X2),R2=as.numeric(X3),R3=as.numeric(X4),R4=as.numeric(X5),
         R5=as.numeric(X6),R6=as.numeric(X7),R7=as.numeric(X8),WE.tot=as.numeric(X9)) %>%
  mutate(Jahr=ifelse(!is.na(dmy(Land)),year(dmy(Land)),NA)) %>%
  fill(Jahr,.direction = 'down') %>%
  filter(!is.na(WE.tot)&(!is.na(Jahr))) %>%
  relocate(Jahr,.before=Land) 


unique(wohn.rooms.temp$Land)
ost.list<-unique(wohn.rooms.temp$Land)[c(3,4,8,13,14,16)]
#oder: c('Brandenburg','Mecklenburg-Vorpommern','Sachsen','Sachsen-Anhalt','Thüringen')


wohn.rooms<-wohn.rooms.temp %>% 
  mutate(Ost=if_else(Land%in%ost.list,1,if_else(Land=='Insgesamt',2,0))) 

rooms.ow<-wohn.rooms%>%
  group_by(Ost,Jahr)%>%
  summarise_at(vars(R1:R7,WE.tot),sum) %>%   #absolute Anzahl nach Räumen
  mutate_at(vars(R1:R7),funs(./ WE.tot))     #Anteilswerte
  
#testen
tmp<-0
for (i in 3:9) {tmp<-tmp+rooms.ow[75,i]}
tmp

#alternative Befehle?
#vars_pull(wohn.rooms,var=-2)
#?tally()
#?add_tally()
#?select_vars()

#Überführen ins Long-Format
year<-2019
rooms.long <-wohn.rooms %>%
  melt(id.vars = c('Jahr','Land','Ost')) %>%
  mutate(Var=as.factor(variable),Wert=as.numeric(value))%>%
  select(Jahr,Land,Ost,Var,Wert) %>%
  filter(Jahr==year)

#Generieren eines Datensatzs mit Anteilen der verschiedenen Raum-Anzahl
rooms.long %>%
  group_by(Ost,Var) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

class(rooms.long$Var)
  

##############
#Bevölkerung
##############
  
bev.tmp<-bev.data %>%
  rename(Jahr=Zeit,bev.zahl=names(bev.data)[ncol(.)]) %>%
  select(Jahr,bev.zahl)%>%
  mutate(Jahr=year(dmy(Jahr)),bev.zahl=bev.zahl/1000000)
class(bev.tmp$Jahr)


##############
#Haushalte
##############

hh.tmp<-hh.data%>%
            rename(Index=names(hh.data)[1],Jahr=X3,hh.zahl=X4)%>%
            filter(Index%in%as.character(c(1:29)))%>%
            select(Jahr,hh.zahl)%>%
            mutate(Jahr=parse_number(Jahr),hh.zahl=as.numeric(hh.zahl)/1000)
class(hh.tmp$hh.zahl)


#######
#Mergen
#######

base.year<-2000
wohn.demo<-wohn.plot%>%
            select(Jahr,WE.tot,WE.WG,WF.WG)%>%
            left_join(bev.tmp)%>%
            left_join(hh.tmp)%>%
            mutate_at(.vars=vars(!Jahr),.funs=funs(./(.)[which(Jahr==base.year)]*100)) 
     #       mutate_at(.vars=vars(!Jahr),.funs=funs(lag(lead(.)/.,))) #Alternativ: jährliche Wachstumsfaktoren berechnen



#####################################  
#####################################  
##########     Plots        #########
#####################################    
#####################################  


#Wohneinheiten
var.list<-c('WE.tot','WE.WG')
wohn.long.fin<-wohn.long %>%
     filter(Merkmal%in%var.list) %>%
      mutate(Merkmal=factor(Merkmal,levels=c('WE.WG','WE.tot'),
              labels = c('Wohngebäude (inkl. Wohnheime)','gesamt')))

class(wohn.long.fin$Jahr)  

p <- ggplot(data=wohn.long.fin, aes(x=Jahr,y=Wert,fill=Merkmal)) 
p <- p + geom_bar(stat='identity',position = 'dodge')
#p <- p + labs(y='Anzahl (in Millionen)', x='Jahr',caption='Ab 2010: Ergebnisse auf Grundlage der Gebäude- und Wohnungszählung.') 
p <- p + labs(y='Anzahl (in Millionen)',x='Jahr')
p <- p + scale_y_continuous(breaks = pretty_breaks())
p <- p + coord_cartesian(ylim = c(35, 43)) 
#p <- p + scale_x_continuous(breaks=seq(first(year.list),last(year.list),5))
#p <- p + scale_fill_discrete(guide=guide_legend(title='Kennzahl'),name='')
p <- p + scale_fill_brewer(name='Wohnungen',palette = 'Set1') 
p <- p + theme_light() 
p <- p + theme(legend.position='bottom',
               axis.text.x=element_text(angle=45,size=14,hjust=1,vjust=1),
               axis.text.y=element_text(size=14),
               axis.title=element_text(size=14,face='bold'),
               strip.text.x = element_text(size = 16, face = 'bold'),
               legend.title=element_text(size = 16, face = 'bold'),
               legend.text=element_text(size=14))
#p <- p + annotate('segment',x=1965,xend=1952, y=900, yend=mean(mig.plot$Saldo), arrow=arrow)
p  
saveInImageDirectory("WE_tot_Balken.pdf")

wohn.plot<-wohn.plot%>%
  mutate(g.rate2=ifelse(Jahr==2010,NA,g.rate))


p1<-ggplot(wohn.plot,aes(x=Jahr))
p1 <- p1 + geom_point(aes(y=g.rate),size = 3, shape = 21,fill='black') 
p1 <- p1 + geom_line(aes(y=g.rate2))  
p1 <- p1 + labs(y='Veränderung ggb. Vorjahr (in %)', x='Jahr') 
p1 <- p1 + scale_y_continuous(breaks = pretty_breaks())
#p1 <- p1 + coord_cartesian(ylim = c(35, 43)) 
#p1 <- p1 + scale_x_continuous(breaks=seq(first(year.list),last(year.list),5))
#p1 <- p1 + scale_fill_discrete(guide=guide_legend(title='Kennzahl'),name='')
#p1 <- p1 + scale_fill_brewer(name='Wohnungen',palette = 'Set1') 
p1 <- p1 + theme_light() 
p1 <- p1 + theme(legend.position='bottom',
               axis.text.x=element_text(angle=45,size=14,hjust=1,vjust=1),
               axis.text.y=element_text(size=14),
               axis.title=element_text(size=14,face='bold'),
               strip.text.x = element_text(size = 16, face = 'bold'),
               legend.title=element_text(size = 16, face = 'bold'),
               legend.text=element_text(size=14))
#p <- p + annotate('segment',x=1965,xend=1952, y=900, yend=mean(mig.plot$Saldo), arrow=arrow)
p1

combined <- p + p1 & theme(legend.position = 'bottom')
combined + plot_layout(guides = 'collect')+
         plot_annotation(caption='Ab 2010: Ergebnisse auf Grundlage der Gebäude- und Wohnungszählung.')
saveInImageDirectory("Wohnungsbestand.pdf")


#statt annotate https://drawar.github.io/posts/add-borders-annotate-outside-ggplot/
#Mit Datumsangaben: scale_x_date(name = 'Month', breaks = seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month"), labels = function(date){return(month(date, label = TRUE))}) + 
  



#Anzahl Räume
rooms.plot<-rooms.ow %>%
  filter(Ost==0|Ost==1) %>%
  select(Jahr,Ost,R1:R7) %>%
  melt(id.vars = c('Jahr','Ost')) %>%
  mutate(Ost=factor(Ost,labels=c('Alte Bundesländer','Neue Bundesländer (mit Berlin)')),
         R.Anzahl=factor(str_remove(variable, '^R'),labels=c(1:6,'7+')),Anteil=as.numeric(value)) %>%   #Alternativ: Anzahl = as.numeric(variable))
  select(Jahr,Ost,R.Anzahl,Anteil)


#Plot für 2019
year<-'2019'
rooms.plot1<-rooms.plot %>%
  filter(Jahr==year)
  
p<-ggplot(rooms.plot1,aes(x=R.Anzahl,y=Anteil,fill=Ost))
p<-p+geom_bar(aes(fill=Ost),stat='identity',position = 'dodge')
p<-p+geom_text(aes(label = paste0(round(Anteil*100,1), '%'), vjust=1.5),size =2,
               position=position_dodge(0.9),color = "white")
p<-p+scale_y_continuous('Anteil (in Prozent)',labels=percent)
p<-p+labs(x='Anzahl der Zimmer')
p <- p + scale_fill_brewer(name='Wohnungen',palette = 'Set1') 
p <- p + theme_light() 
p <- p + theme(legend.position='bottom',
               axis.text.x=element_text(size=14,hjust=1,vjust=1),
               axis.text.y=element_text(size=14),
               axis.title=element_text(size=14,face='bold'),
               strip.text.x = element_text(size = 16, face = 'bold'),
               legend.title=element_blank(),
               legend.text=element_text(size=14))
#p <- p + annotate('segment',x=1965,xend=1952, y=900, yend=mean(mig.plot$Saldo), arrow=arrow)
p 
saveInImageDirectory('Verteilung_Zimmer.pdf')


#plot mit Entwicklung
year.list<-c('1995','2005','2015','2019')
rooms.plot2<-rooms.plot %>%
  filter(Jahr%in%year.list)

p<-ggplot(rooms.plot2,aes(x=R.Anzahl,y=Anteil,fill=Ost))
p<-p+geom_bar(aes(fill=Ost),stat='identity',position = 'dodge')
p<-p+geom_text(aes(label = paste0(round(Anteil*100,1), '%'), vjust=1.1),size =2.2,
               position=position_dodge(0.9),color = "white")
p<-p+scale_y_continuous('Anteil (in Prozent)',labels=percent)
p<-p+labs(x='Anzahl der Zimmer')
p <- p + scale_fill_brewer(name='Wohnungen',palette = 'Set1') 
p <- p + facet_wrap(~Jahr)#+scale_fill_discrete(guide = FALSE)
p <- p + theme_light() 
p <- p + theme(legend.position='bottom',
               axis.text.x=element_text(size=14,hjust=1,vjust=1),
               axis.text.y=element_text(size=14),
               axis.title=element_text(size=14,face='bold'),
               strip.text.x = element_text(size = 16, face = 'bold'),
               legend.title=element_blank(),
               legend.text=element_text(size=14))
#p <- p + annotate('segment',x=1965,xend=1952, y=900, yend=mean(mig.plot$Saldo), arrow=arrow)
p 
saveInImageDirectory('Verteilung_Zimmer_facet.pdf')




#Soziodemographie

wohn.demo.plot<-wohn.demo%>%
                   melt(id.vars = c('Jahr')) %>%
                   rename(Variable=variable,Wert=value)

p<-ggplot(wohn.demo.plot,aes(x=Jahr,y=Wert,color=Variable))
p<-p+geom_line(size=1.2)
p

