source(xfun::from_root("R/00_setup.R"))

#Data Import ----

## Data description -----
desc <- head(get_description("MIR.M.U2.B.A2C.AM.R.A.2250.EUR.N"), 3)
strwrap(desc, width = 80)


## Kreditzinsen -----
i_hloans <- "MIR.M.U2.B.A2C.AM.R.A.2250.EUR.N"
i_corploans <- "MIR.M.U2.B.A2I.AM.R.A.2240.EUR.N"

tbl_hloans <- ecb::get_data("MIR.M.U2.B.A2C.AM.R.A.2250.EUR.N",
                  filter = list(startPeriod = "2003"))

### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('Eurozone_Kreditzinsen_Immo_raw_{dl_date}.rds')
save(tbl_hloans,file=xfun::from_root("data","raw",my_out_file))



tbl_corploans <- ecb::get_data("MIR.M.U2.B.A2I.AM.R.A.2240.EUR.N",
                            filter = list(startPeriod = "2003"))


### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('Eurozone_Kreditzinsen_Corps_raw_{dl_date}.rds')
save(tbl_corploans,file=xfun::from_root("data","raw",my_out_file))



## Anteil variabler Verzinsung ----

hhnfi <- "RAI.M.U2.SVLHHNFC.EUR.MIR.Z"
hloans <- "RAI.M.U2.SVLHPHH.EUR.MIR.Z"

tbl_hhnfc <- ecb::get_data("RAI.M.U2.SVLHHNFC.EUR.MIR.Z",
                            filter = list(startPeriod = "2003"))

tbl_hloans <- ecb::get_data("RAI.M.U2.SVLHPHH.EUR.MIR.Z",
                            filter = list(startPeriod = "2003"))

### tibble im "raw data Ordner" speichern ----
dl_date<-Sys.Date()

### save data ----
my_out_file<-glue('Eurozone_VariableZinsen_HH-NFC_raw_{dl_date}.rds')
save(tbl_hhnfc,file=xfun::from_root("data","raw",my_out_file))

my_out_file<-glue('Eurozone_VariableZinsen_Immokredite_raw_{dl_date}.rds')
save(tbl_hloans,file=xfun::from_root("data","raw",my_out_file))



# Data Wrangling -----

## Kreditzinsen -----

### load data ----
dl_date <- "2023-05-26"
my_in_file<-glue('Eurozone_Kreditzinsen_Corps_raw_{dl_date}.rds')
load(xfun::from_root("data","raw",my_in_file))
my_in_file<-glue('Eurozone_Kreditzinsen_Immo_raw_{dl_date}.rds')
load(xfun::from_root("data","raw",my_in_file))

### clean and transform ----
tbl_corps <- tbl_corploans %>%
                       mutate(type="Unternehmenskredite",
                              value=obsvalue/100,
                              time=convert_dates(obstime)) %>%
                              select(time,type,value)

tbl_hous <- tbl_hloans %>%
                      mutate(type="Immobilienkredite (Private)",
                             value=obsvalue/100,
                             time=convert_dates(obstime)) %>%
                      select(time,type,value)
### merge ----
tbl_plot <- rbind(tbl_hous,tbl_corps)


### tibble im "tidy data Ordner" speichern ----

#### filename ----
dl_date <- "2023-05-26"
my_out_file<-glue('Eurozone_Kreditzinsen_2003-2023_raw_{dl_date}.rds')
#### save data ----
save(tbl_plot,file=xfun::from_root("data","tidy",my_out_file))




## Variable Verzinsung ----

### load data ----
dl_date <- "2023-05-26"
my_in_file<-glue('Eurozone_VariableZinsen_HH-NFC_raw_{dl_date}.rds')
load(xfun::from_root("data","raw",my_in_file))
my_in_file<-glue('Eurozone_VariableZinsen_Immokredite_raw_{dl_date}.rds')
load(xfun::from_root("data","raw",my_in_file))

### clean and transform ----
tbl_nfc <- tbl_hhnfc %>%
            mutate(type="Haushalte und nichtfinanzielle Unternehmen",
            value=obsvalue/100,
            time=convert_dates(obstime)) %>%
            select(time,type,value)

tbl_hh <- tbl_hloans  %>%
                 mutate(type="Immobilienkredite (Private)",
                 value=obsvalue/100,
                 time=convert_dates(obstime)) %>%
                 select(time,type,value)

### merge ----
tbl_plot <- rbind(tbl_hh,tbl_nfc)


### tibble im "tidy data Ordner" speichern ----

#### filename ----
dl_date <- "2023-05-26"
my_out_file<-glue('Eurozone_VariableDarlehenszinsen_2003-2023_raw_{dl_date}.rds')
#### save data ----
save(tbl_plot,file=xfun::from_root("data","tidy",my_out_file))






# Dataviz -----

## Kreditzinsen -----

### load data ----
#### filename ----
dl_date <- "2023-05-26"
my_in_file<-glue('Eurozone_Kreditzinsen_2003-2023_raw_{dl_date}.rds')
#### save data ----
load(xfun::from_root("data","tidy",my_in_file))

### plot Kreditzinsen -----
p <- tbl_plot %>%
          ggplot(aes(x=time,y=value,color=type))
p <- p + geom_line()
p <- p + expand_limits(y=c(0,0.065))
p <- p + scale_y_continuous(labels = scales::percent,
                            breaks = seq(0,.7,by=.01),
                            name = "Zins (in %)") +
         scale_x_date(date_breaks = "1 year", date_labels = "%Y", name= "Jahr")
p <- p + ggtitle("Entwicklung der Finanzierungskosten in der Eurozone")
p <- p + theme_light() + theme(title= element_text(size=rel(.5)),
                               legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','Immo_vs_Unternehmenskredite.svg'),width=1250,height = 900,units="px")

## Anteil Variable Verzinsung  -----

#### filename ----
dl_date <- "2023-05-26"
my_in_file<-glue('Eurozone_VariableDarlehenszinsen_2003-2023_raw_{dl_date}.rds')
#### save data ----
load(xfun::from_root("data","tidy",my_in_file))

### plot Kreditzinsen -----
p <- tbl_plot %>%
      ggplot(aes(x=time,y=value,color=type))
p <- p + geom_line()
#p <- p + expand_limits(y=c(0,0.065))
p <- p + scale_y_continuous(labels = scales::percent,
                            #breaks = seq(0,.7,by=.01),
                            name = "Anteil (in %)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", name= "Jahr")
p <- p + ggtitle("Anteil Darlehen mit variabler Verzinsung in der Eurozone")
p <- p + theme_light() + theme(title= element_text(size=rel(.5)),
                               legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','Anteil_VariableZinsen.svg'),width=1250,height = 900,units="px")



