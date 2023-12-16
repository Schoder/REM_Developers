library(tidyquant)
library(tidyverse)



# Datenimport ----
## get data ---
tbl_prices <- tq_get(c("VNA.DE","UBS.VI"),
                     get  = "stock.prices",
                     from = "2013-01-01",
                     to   = Sys.Date())

tbl_prices %>%
  ggplot(aes(x = date, y = adjusted, color=symbol)) +
  geom_line()


## save file -----
my_out_file <- "Dev_stocks_Dez23.rds"
save(tbl_prices,file = xfun::from_root("data","raw",my_out_file))


# Combine data ----

## load Dev stock data ----
my_in_file <- "Dev_stocks_Dez23.rds"
load(file = xfun::from_root("data","raw",my_in_file))


# Load Index-Data ----
my_in_file <- "stoxx600_europe_RE_fin_Dez23.rds"
load(file = xfun::from_root("data","tidy",my_in_file))

tbl_plot <- tbl_plot %>% 
                rename(date=datum,symbol=index)

period <- tbl_plot %>%
                summarise(start=min(date),end=max(date))

tbl_devs <- tbl_prices %>%
                rename(price=adjusted) %>% 
                filter(date>=period$start&date<=period$end) %>% 
                full_join(tbl_plot)

tbl_fin <- tbl_devs %>%
                  group_by(symbol) %>%
                  mutate(p_index=price/last(price)*100)

symbl_list <- c("UBS.VI","VNA.DE")

p <- tbl_fin %>%
        filter(symbol%in%symbl_list) %>% 
          ggplot(aes(x=date,y=p_index,color=symbol))
p <- p + geom_line(linewidth=0.4,alpha=.7)
#p <- p + geom_col(aes(y=volumen*5,fill=pos_chg),show.legend = FALSE)
p <- p + labs(x="Jahr")
p <- p + scale_y_continuous(name = "Kursentwicklung (2013=100)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
ggsave(xfun::from_root('img','Dev_stocks.svg'),width=1250,height = 900,units="px")
