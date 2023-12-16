inc <- 1000
i <- seq(.02,.10,.01)


tbl_plot <- tibble(i=i,pv=inc/i) %>%
                  mutate(chg_by1=pv/lag(pv)-1,
                         chg_comp1=pv/first(pv)-1)

p <- tbl_plot %>%
          ggplot(aes(x=i,y=chg_by1)) +
          geom_bar(stat='identity',fill='#232461',alpha=.6)
p <- p + scale_x_continuous(labels = scales::percent,breaks = i) +
         scale_y_continuous(labels = scales::percent,position = "right")  +
         coord_flip() +
         labs(x="...auf ein (neues) Zinsniveau von...",
              y="Veränderung Ewige Rente bei Zinsanstieg um 1%-Punkt...") +
       # theme(title = element_text("Veränderung Barwert im Vergleich
       #                            zum Barwert bei einem Zins von 2%")) +
         theme_light() + theme(legend.position='none',
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
         )
p
ggsave(xfun::from_root("img/zinssensitiv.svg"),width=1250,height = 900,units="px")
