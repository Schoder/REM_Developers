#Quelle: https://www.morningstar.de/de/news/231440/sfdr-verm%C3%B6gen-von-artikel-9-fonds-nach-welle-von-herabstufungen-fast-halbiert.aspx
#abgerufen 26.05.2023
tbl_funds <- tibble(Anteile=c(.445,.522,.033),
                    Typ=c("Artikel 6","Artikel 8","Artikel 9"))

my_cols <- c('#232461','#39B070','#145431')

p <- tbl_funds %>%
        mutate(fmax=cumsum(Anteile),fmin=if_else(is.na(lag(fmax)),0,lag(fmax))) %>%
        ggplot(aes(ymax=fmax, ymin=fmin,
             xmax=4, xmin=3))
p <- p + geom_rect(aes(fill=Typ),stat='identity')
p <- p + scale_fill_manual(values=my_cols)
p <- p + coord_polar(theta="y")
p <- p + xlim(c(2, 4))
p <- p + guides(fill=guide_legend(title="SFDR Produkttyp"))
p <- p + ggtitle("Marktanteile der Produkttypen gemäß EU-Offenlegungsverordnung (Stand: 31.12.2022)")
p <- p + theme_void()
p <- p + theme(title= element_text(size=rel(.9)),
               legend.title =element_text(size=rel(.9)),
               legend.text = element_text(size=rel(.5)),
)
p
ggsave(xfun::from_root('img','Anteil_SFDR_Typen.svg'),width=1250,height = 900,units="px")
