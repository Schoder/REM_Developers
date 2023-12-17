library(tidyverse)
library(readxl)

# Data Import -----

my_in_file <- 'Yield_Curve_(ECB).xlsx'
tbl_yields <- read_excel(xfun::from_root('data','raw',my_in_file), 
                         col_names = T)



#names(tbl_yields)[str_detect(names(tbl_yields), "\\d{5}")] <- format(as.Date(as.numeric(names(tbl_yields)[str_detect(names(tbl_yields), "\\d{5}")]), origin = "1899-12-30"), "%d-%b-%y")


head(tbl_yields)
tbl_plot <- tbl_yields %>% 
                  pivot_longer(cols = !c(Maturity,Fälligkeit),
                               names_to = 'date',values_to = 'Rendite') %>% 
                  mutate(Datum=as_date(as.numeric(date), origin="1899-12-30"))
?as_date
p <- tbl_plot %>%
          ggplot(aes(x=Fälligkeit,y=Rendite,color=factor(Datum)))
p <- p + geom_line(linewidth=0.4,alpha=.7)
p
#p <- p + geom_col(aes(y=volumen*5,fill=pos_chg),show.legend = FALSE)
p <- p + labs(x="Restlaufzeit in Jahren",
              caption = "AAA-Staatsanleihen (Kassamarkt)")
p <- p + scale_y_continuous(name = "Rendite in %") +
         scale_x_continuous(breaks = seq(0,30,5))
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5)),
                               plot.caption = element_text(hjust = 0)
) 
p
ggsave(xfun::from_root('img','yieldcurve_Euro.svg'),width=1250,height = 900,units="px")
