# Quelle: SCOPE
#https://www.scopegroup.com/dam/jcr:b461c4db-421f-4ebd-bead-e3668875f5c9/Scope%20Renditekennzahlen%20Offene%20Immobilienfonds%20Mai%202023.pdf
#https://www.scopegroup.com/dam/jcr:a70369a9-0afd-484b-823c-aa15e0c64e55/Scope%20Kreditquoten%20Offene%20Immobilienfonds%20Mai%202023.pdf
#https://www.scopegroup.com/dam/jcr:5d0634bc-c9a8-4307-a34a-582c0c682747/Scope%20Markanalyse%20Offene%20Immobilienfonds%20M%C3%A4rz%202023.pdf

source(xfun::from_root("R/00_setup.R"))
# Data Import ------

## Renditen -----
library(readxl)
my_in_file <- "OffeneIF_(Scope_2023).xlsx"
tbl_returns <- read_excel(xfun::from_root("data","raw",my_in_file),sheet="Einjahresrendite")

## Vermietungsquoten -----
my_in_file <- "OffeneIF_(Scope_2023).xlsx"
tbl_letrate <- read_excel(xfun::from_root("data","raw",my_in_file),sheet="Vermietungsquoten_OIF")

## Kreditquoten -----
my_in_file <- "OffeneIF_(Scope_2023).xlsx"
tbl_liab <- read_excel(xfun::from_root("data","raw",my_in_file),sheet="Kreditquote")

## disaggregierte Daten ----
my_in_file <- "OffeneIF_(Scope_2023).xlsx"
tbl_funds <- read_excel(xfun::from_root("data","raw",my_in_file),sheet="OIF_disagg")



## Test Import mit pdf tools ----
library(pdftools)
my_in_file <- "Scope Renditekennzahlen Offene Immobilienfonds Mai 2023.pdf"
test <- pdftools::pdf_data(xfun::from_root("lit",my_in_file))


# Data Wrangling -----


## aggregierte------
tbl_ret <- tbl_returns %>%
                rename(avg_ret=`Durchschnittliche Einjahresrendite`) %>%
                mutate(avg_ret=str_replace(avg_ret,",", ".")) %>%
                #Extrahieren der Zahlen
                #mit \\d wird nach Ziffern zwischen 0 und 9 gesucht
                #mutate(avg_ret=str_extract(avg_ret,"\\d\\.\\d")) %>%
                mutate(avg=as.numeric(avg_ret)) %>%
                pivot_longer(cols=c(Anzahl,avg),names_to = "var",values_to = "value") %>%
                mutate(Art="Durchschnittliche Einjahresrendite") %>%
                select(Datum,Art,var,value)



tbl_let <- tbl_letrate %>%
                rename(avg_let=`Durchschnittliche Vermietungsquote`) %>%
                mutate(avg_let=str_replace(avg_let,",", ".")) %>%
                #Extrahieren der Zahlen
                #mit \\d wird nach Ziffern zwischen 0 und 9 gesucht
                #mutate(avg_ret=str_extract(avg_ret,"\\d\\.\\d")) %>%
                mutate(avg=as.numeric(avg_let)) %>%
                mutate(Anzahl=NA) %>%
                pivot_longer(cols=c(Anzahl,avg),names_to = "var",values_to = "value") %>%
                mutate(Art="Durchschnittliche Vermietungsquote") %>%
                select(Datum,Art,var,value)

tbl_lbs <- tbl_liab %>%
              mutate(avg=FK_Quote) %>%
              pivot_longer(cols=c(Anzahl,avg),names_to = "var",values_to = "value") %>%
              select(Datum,Art,var,value)




tbl_plot <- rbind(tbl_ret,tbl_let)

my_out_file <- "OIF_Durchschnittswerte_retlet.rds"
save(tbl_plot,file = xfun::from_root("data","tidy",my_out_file))

my_out_file <- "OIF_Durchschnittswerte_FK.rds"
save(tbl_lbs,file = xfun::from_root("data","tidy",my_out_file))


## disaggregierte Werte ----

tbl_plot <- tbl_funds %>%
                      select(Fondsname,Rendite_2021,Rendite_2022) %>%
                      pivot_longer(cols = -Fondsname,names_to = "Jahr",
                                   names_prefix = "Rendite_") %>%
                      mutate(value=str_replace(value,",", ".")) %>%
                      mutate(val=if_else(value=="NA",NA,parse_number(value)/100))

my_out_file <- "OIF_Renditen_disagg.rds"
save(tbl_plot,file = xfun::from_root("data","tidy",my_out_file))


# Dataviz -----


## Returns und Letting Rate-----
my_in_file <- "OIF_Durchschnittswerte_retlet.rds"
load(xfun::from_root("data","tidy",my_in_file))


var_list <- c("avg")

p <- tbl_plot %>%
          filter(var%in%var_list) %>%
          pivot_wider(names_from=Art,values_from = c(value)) %>%
          ggplot(aes(x=as_date(Datum)))
p <- p + geom_bar(aes(y=`Durchschnittliche Einjahresrendite`),stat = 'identity',fill='#232461',alpha=.7)
p <- p + geom_line(aes(y=`Durchschnittliche Vermietungsquote`*.1),stat = 'identity',color='#d84116')
p <- p + scale_y_continuous(name = "Rendite in %",labels = scales::percent,
                             sec.axis = sec_axis(~.*10, name="Vermietungsquote in %",
                                                 labels = scales::percent)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + labs(x="Jahr")
p <- p + theme_light() + theme(axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','OIF_Returns_LettingRate.svg'),width=1250,height = 750,units="px")



### Fremdkapitalquoten -----

p <- tbl_lbs %>%
          filter(var=="avg",Art%in%c("gewichtete FK-Quote","Regulatorische Vorgabe neu")) %>%
          ggplot(aes(x=as_date(Datum),y=value))
p <- p + geom_line(aes(color=Art),stat='identity')
p <- p + scale_y_continuous(name = "Fremdkapitalquote in %",labels = scales::percent) +
         scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + labs(x="Jahr")
p <- p + theme_light() + theme(legend.title = element_blank(),
                               legend.position = 'bottom',
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','OIF_Fremdkapital.svg'),width=1250,height = 750,units="px")


### Disaggregiert ------

my_in_file <- "OIF_Renditen_disagg.rds"
load(xfun::from_root("data","tidy",my_in_file))

p <- tbl_plot %>%
          filter(!is.na(val)) %>%
          ggplot(aes(x=Jahr,y=val))
p <- p + geom_boxplot(color='#232461',alpha=.7)
p <- p + scale_y_continuous(name = "Rendite %",labels = scales::percent)
p <- p + theme_light() + theme(legend.title = element_blank(),
                               legend.position = 'bottom',
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','OIF_Rendite_disagg.svg'),width=1250,height = 750,units="px")
