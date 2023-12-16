#Wohnungsmarkt
source(xfun::from_root("R/00_setup.R"))

# DESTATIS Data -----
library(wiesbaden)

## Setup -----
test_login(genesis=c(db='de', user="DEAVXJR0SO", password="DatenSammler#78"))

save_credentials(db='de',user="DEAVXJR0SO",
                 password="DatenSammler#78")

## find dataset ----

#Verbraucherpreisindizes für Deutschland - Jahresbericht
#https://www.destatis.de/DE/Themen/Wirtschaft/Preise/Verbraucherpreisindex/_inhalt.html#_7l80r4olp

#findet sich in der Datenbank GENESIS-Online im Themenbereich 61111.
retrieve_datalist(tableseries="4000W*",
                  genesis=c(db='de'))

# 11 - Kauffälle, Veräußerte Fläche, Kaufsumme, Durchschnittlicher Kaufwert, Deutschland insgesamt, Gemeindegrößenklassen, Art der Baufläche, Jahr

## Download data ----

## Baulandpreise, Gemeindegrößenklassen -----
tbl_blp <- retrieve_data(tablename="61511BJ011",
                         genesis=c(db='de'))

blp_meta <- retrieve_metadata(tablename="61511BJ011",
                              genesis=c(db='de'))

blp_labels <- retrieve_valuelabel("GEMGK4",
                                  genesis=c(db='de'))





# Gebäudebestand ------

## Baugenehmigungen ----



my_in_file <- '31111-0001_$F_flat.csv'
tbl_permits <- read_csv2(xfun::from_root('data','raw',my_in_file))



#ARTBT3 ARTBT5
tbl_permits %>% 
  select(Zeit,`2_Auspraegung_Label`,starts_with('FLC001'),starts_with('WOHN01')) %>% 
  rename(Jahr=Zeit,type=`2_Auspraegung_Label`,WFL=starts_with('FLC001'),Wohnungen=starts_with('Wohn01')) %>% 
  pivot_longer(c(WFL,Wohnungen),names_to = 'var',values_to = 'val') %>% 
  filter(type=='Insgesamt') %>% 
  ggplot(aes(x=Jahr,y=as.numeric(val),fill=var)) + 
  geom_bar(stat = 'identity',position = 'dodge')



## Abgänge -----

retrieve_datalist(tableseries="31141*",
                  genesis=c(db='de'))
tbl_abgang <- retrieve_data(tablename="31141BJ002",
                         genesis=c(db='de'))
retrieve_metadata(tablename="31141BJ002",
                  genesis=c(db='de'))



my_in_file <- '31141-0002_$F_flat.csv'
tbl_abgang <- read_csv2(xfun::from_root('data','raw',my_in_file))

tbl_abgang %>% 
  select(Zeit,`2_Auspraegung_Label`,starts_with('FLC001'),starts_with('WOHN01')) %>% 
  rename(Jahr=Zeit,type=`2_Auspraegung_Label`,WFL=starts_with('FLC001'),Wohnungen=starts_with('Wohn01')) %>% 
  pivot_longer(c(WFL,Wohnungen),names_to = 'var',values_to = 'val') %>% 
  filter(type=='Insgesamt') %>% 
  ggplot(aes(x=Jahr,y=as.numeric(val),fill=var)) + 
  geom_bar(stat = 'identity',position = 'dodge')
