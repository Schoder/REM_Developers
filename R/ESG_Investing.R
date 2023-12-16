# Data Import -----

## BNP Paribas ----

my_in_file <- "ESG_Zertifizierungen_(CBRE_2022).xlsx"

### Investitionsvolumina ----
tbl_vol <- read_excel(xfun::from_root("data","tidy",my_in_file),sheet = 1)

### Nutzungsarten -----
tbl_use <- read_excel(xfun::from_root("data","tidy",my_in_file),sheet = 2)

### Käufergruppe -----
tbl_buyers <- read_excel(xfun::from_root("data","tidy",my_in_file),sheet = 3)

### Marktanteil Zertifizierungen -----
tbl_cert <- read_excel(xfun::from_root("data","tidy",my_in_file),sheet = 4)



## CBRE ----



# Dataviz -----

## Volumina -----

p <- tbl_vol %>%
        ggplot(aes(x=as_date(Datum),y=Volumen,fill=ESG))
p <- p + geom_bar(stat='identity',position='stack')
p <- p + labs(x="Jahr")
p <- p + scale_y_continuous(name = "Volumen (in Mio €)") +
         scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_text(size=rel(.8)),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','Invest_vol_ESG.svg'),width=1250,height = 750,units="px")

## Volumina -----

p <- tbl_use %>%
          ggplot(aes(x=Typ,y=Anteil,fill=Nutzungsart))
p <- p + geom_bar(stat='identity',position='stack')
p <- p + labs(x="",y="Anteil in %")
p <- p + scale_y_continuous(labels = scales::percent)
p <- p + scale_fill_brewer(palette='Accent')
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_text(size=rel(.8)),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','Sector_ESG.svg'),width=1250,height = 750,units="px")

## Käufergruppen ----

p <- tbl_buyers %>%
        ggplot(aes(y=reorder(Investor,Anteil_Gesamtinvestmentvolumen),x=Anteil_Gesamtinvestmentvolumen))
p <- p + geom_bar(stat='identity',fill='#232461',alpha=.7)
p <- p + scale_x_continuous(labels = scales::percent)
p <- p + labs(x="",y="")
p
ggsave(xfun::from_root('img','Buyers_ESG.svg'),width=1250,height = 750,units="px")
