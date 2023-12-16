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
retrieve_datalist(tableseries="61511*",
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

library(tidyverse)
tbl_tmp <- left_join(tbl_blp,blp_labels)


my_in_file <- '61511-0011_$F_flat.csv'
tbl_blp <- read_csv2(xfun::from_root('data','raw',my_in_file))


tbl_blp %>% 
  ggplot(aes(y=`KAU004__Durchschnittlicher_Kaufwert__EUR/qm`,
             x=Zeit,
             color=`2_Auspraegung_Label`)) +
  geom_line() + facet_wrap(~`3_Auspraegung_Label`)




my_in_file <- '61511-0017_$F_flat.csv'
tbl_blp <- read_csv2(xfun::from_root('data','raw',my_in_file))


tbl_blp %>% 
  ggplot(aes(y=`KAU004__Durchschnittlicher_Kaufwert__EUR/qm`,
             x=Zeit,
             color=`2_Auspraegung_Label`)) +
  geom_line() + facet_wrap(~`3_Auspraegung_Label`)


my_in_file <- '61511-0102_$F_flat.csv'
tbl_blp <- read_csv2(xfun::from_root('data','raw',my_in_file))

tbl_blp %>% 
  ggplot(aes(y=`KAU004__Durchschnittlicher_Kaufwert__EUR/qm`,
             x=Zeit,
             color=`2_Auspraegung_Label`)) +
  geom_line() + facet_wrap(~`3_Auspraegung_Label`)

my_in_file <- '61511-0106_$F_flat.csv'
tbl_blp <- read_csv2(xfun::from_root('data','raw',my_in_file))

tbl_tmp <- tbl_blp %>% 
  mutate(Kaufwert=as.numeric(str_replace_all(`KAU004__Durchschnittlicher_Kaufwert__EUR/qm`, ",", ".")))

tbl_tmp %>%  
  filter(`4_Merkmal_Code`=='ERWAT4'&
           `3_Auspraegung_Label`=='Insgesamt') %>% 
  ggplot(aes(y=Kaufwert, x=Zeit,color=`2_Auspraegung_Code`)) +
  geom_line() +
  facet_wrap(~`4_Auspraegung_Code`)
class(tbl_tmp$Kaufwert)



tbl_chord <- tbl_tmp %>% 
                mutate(Fläche=as.numeric(FLC023__Veraeusserte_Flaeche__1000_qm)) %>% 
                filter(`2_Auspraegung_Code`=='BAULAND01'&Zeit=='2010') %>% 
                select(`3_Auspraegung_Code`,`4_Auspraegung_Code`,Fläche) %>% 
                rename(from=`3_Auspraegung_Code`,
                       to=`4_Auspraegung_Code`,
                       value=Fläche) %>% 
                drop_na(c(from,to)) %>% 
                replace(is.na(.), 0)

tbl_chord %>% 
  circlize::chordDiagram(tbl_chord,symmetric = TRUE)

## Originaldaten speichern ----

### tibble im "raw data Ordner" speichern ----
date<-'2023-04-16'

## save data ----
my_out_file<-glue('vpi_raw_{date}.rds')
save(vpi_m,file=xfun::from_root("data","raw",my_out_file))




# Eurostat Data -----
library(eurostat)
## Headline- und Kerninflation ----
inflation <- get_data("ICP.M.DE.N.000000+XEF000.4.ANR",
                  filter = list(startPeriod = "2000"))


### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('inflation_raw_{dl_date}.rds')
save(inflation,file=xfun::from_root("data","raw",my_out_file))


## Construction Data -----

search_eurostat("construction")

tbl_con <- get_eurostat("STS_COPR_A",
                        filters = list(
                          time = c(1980:2022)
                        ))


### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('EU_construction_raw_{dl_date}.rds')
save(tbl_con,file=xfun::from_root("data","raw",my_out_file))


## Baugenehmigungen

#monatlich sts_cobp_m; teiis550
#quartale sts_cobp_q
#jährlich sts_cobp_a

# does not work
#tbl_permits <- get_eurostat("STS_COBP_A")
#tbl_permits <- get_eurostat("STS_COBP_A",
#                    filter = list(time = c("2005-Q1","2022-Q4"))
#                    )
# works
#tbl_permits <- get_eurostat("STS_COBP_A", legacy_bulk_download = FALSE)
# works
#tbl_sts_cobp <- get_eurostat("STS_COBP_A", filter = list(untilTimePeriod = 2020))
# works
tbl_permits <- get_eurostat("STS_COBP_A", filter = list(untilTimePeriod = 2020))

### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('EU_buildpermits_raw_{dl_date}.rds')
save(tbl_permits,file=xfun::from_root("data","raw",my_out_file))


## House Price Data ----

### EU -----

#funktioniert nicht.
#search_eurostat("TIPSHO40")
#tbl_con <- get_eurostat("TIPSHO40")
# does not work
#tbl_hp <- get_eurostat("TIPSHO40")

#works
tbl_hp <- get_eurostat("TIPSHO40", legacy_bulk_download = FALSE)
# works
tbl_hp <- get_eurostat("TIPSHO40", filter = list(sinceTimePeriod = 2005))


#downloaded manually
#https://ec.europa.eu/eurostat/databrowser/view/tipsho40/default/line
my_in_file <- "tipsho40_linear.csv"
tbl_hp_raw <- read_csv(xfun::from_root("data","raw","tipsho40_linear.csv",my_in_file))

#### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

#### save data ----
my_out_file<-glue('houseprices_1980-2022_raw_{dl_date}.rds')
save(tbl_hp_raw,file=xfun::from_root("data","raw",my_out_file))

### Schweiz -----

library(readxl)
## specify file name and import -----
my_in_file <- "su-q-05.06.03.01.02_(BFS_CH_2023).xlsx"
tbl_impi_raw <- read_excel(xfun::from_root("data","raw",my_in_file),sheet=1,skip=10)

## save file -----
dl_date<-Sys.Date()
my_out_file <- glue("IMPI_2019-2023_{dl_date}.rds")
save(tbl_impi_raw,file = xfun::from_root("data","raw",my_out_file))



# Durchschnittsmieten -----

#Average rent per month in cities by type of dwelling
#CODE: prc_colc_rents UPDATED: 1 February 2023

# does not work
#tbl_rents <- get_eurostat("PRC_COLC_RENTS")
# works
tbl_rents <- get_eurostat("PRC_COLC_RENTS", legacy_bulk_download = FALSE)
tbl_rents <- get_eurostat("PRC_COLC_RENTS", filter = list(sinceTimePeriod = 2005))

tbl_rents <- get_eurostat("PRC_COLC_RENTS",
                          filter = list(time = c(2005:2022)),
                          type = "label", time_format = "num")
tbl_rents %>% distinct(geo)







# ECB Data -----

## Interest rates -----



# Colliers -----

## webscraping

# Laden Sie die erforderliche Bibliothek rvest
library(rvest)

# Definieren Sie die URL der zu scrapenden Website
url <- "https://citysurvey.colliers.de/makrookonomisches-umfeld/"

# Lesen Sie die HTML-Seite ein
page <- read_html(url)

# Extrahieren Sie die gewünschten Elemente mit dem angegebenen CSS-Selektor
elements <- html_nodes(page, "div.component-container.component-container--bg.component-container--bg-theme-1")

# Erstellen Sie eine leere Liste, um die gescrapten Daten zu speichern
data <- list()

# Durchlaufen Sie die ausgewählten Elemente und speichern Sie den Text in der Liste
for (element in elements) {
  data <- c(data, html_text(element))
}

# Erstellen Sie eine tidy tibble aus den gescrapten Daten
tibble(data = data)

combined_tibble <- data %>%
  rowwise() %>%
  mutate(x = str_extract(data, "x: new Date\\('([^']+)'\\)")[1],
         y = as.numeric(str_extract(data, "y: ([0-9.]+)")[1])) %>%
  ungroup()



# Spitzenrenditen vs. 10J

library(rvest)

# Definieren Sie die URL der zu scrapenden Website
url <- "https://citysurvey.colliers.de/makrookonomisches-umfeld/"

# Lesen Sie die HTML-Seite ein
page <- read_html(url)

# Extrahieren Sie die gewünschten Elemente mit dem angegebenen CSS-Selektor
elements <- html_nodes(page, "div.sf-chart.vendor/SdCharts.data-chart-id=1280")

#<div class="component-container component-container--bg component-container--bg-theme-1 component-container--margin-bottom-small"   >
#<div class="sf-chart" data-component="vendor/SfCharts" data-chart-id="1280"></div><script>var config_1280 = {

#div.sf-chart.vendor/SdCharts.data-chart-id="1280"

# Erstellen Sie eine leere Liste, um die gescrapten Daten zu speichern
data <- list()

# Durchlaufen Sie die ausgewählten Elemente und speichern Sie den Text in der Liste
for (element in elements) {
  data <- c(data, html_text(element))
}

