# Data Import ----
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Wohnen/Tabellen/wohneinheiten-nach-baujahr.html


## tibble aus "raw data Ordner" importieren ----
dl_date<-'2023-05-2023'

## load data ----
my_in_file<-glue('Wohnungen_nach Baujahr_D_{dl_date}.rds')
load(file=xfun::from_root("data","raw",my_out_file))


# Dataviz -----

tbl_plot <- tbl_bstockD %>%
                  mutate(Anteil=Anzahl_D/sum(Anzahl_D),
                         Baujahr=factor(Baujahr,
                                    levels=c("vor 1948","1949-1978","1979-1990",
                                             "1991-2010","2011 und später")
                                        ))

## generate plot ----
p <- tbl_plot %>%
          ggplot(aes(x=Baujahr,y=Anteil)) +
          geom_bar(stat='identity',fill='#232461',alpha=.6)
p <- p + scale_y_continuous(labels = scales::percent)
p <- p + labs(y="Anteil in %")
p <- p + theme_light() + theme(legend.position='bottom',
                              axis.text.x=element_text(size=rel(.5)),
                              axis.text.y=element_text(size=rel(.5)),
                              axis.title.x = element_text(size=rel(.5)),
                              axis.title.y = element_text(size=rel(.5))
                              )
p
## save static plot ----
ggsave(xfun::from_root('img','BuildStock_Age_D.svg'),width=1250,height = 900,units="px")

