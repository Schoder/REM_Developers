# Data  Import ----
# library(readxl)
# ## specify file name and import -----
# my_in_file <- "statistic_id1304069_co2-emissionsrechte_-jaehrliche-preisentwicklung-im-eu-emissionshandel-bis-2022_(statista).xlsx"
# tbl_ets_raw <- read_excel(xfun::from_root("data","raw",my_in_file),sheet=2,skip=3)
#
# ## save file -----
# my_out_file <- "ets_data_2005-2022.rds"
# save(tbl_ets_raw,file = xfun::from_root("data","raw",my_out_file))
#
#
# # Data Wrangling ----
# my_in_file <- "ets_data_2005-2022.rds"
# load(file = xfun::from_root("data","raw",my_in_file))
#
# tbl_ets <- tbl_ets_raw %>%
#                 janitor::clean_names() %>%
#                 filter(is.na(x2)==FALSE) %>%
#                 rename(Jahr=starts_with("preis"),Preis=x2) %>%
#                 filter(Jahr<=2020)
#
#
# my_out_file <- "ets_data_2005-2022_tidy.rds"
# save(tbl_ets,file = xfun::from_root("data","tidy",my_out_file))


# Dataviz -----

## load data ----
my_in_file <- "ets_data_2005-2022_tidy.rds"
load(file = xfun::from_root("data","tidy",my_in_file))


p <- tbl_ets %>%
            mutate(date=lubridate::ymd(Jahr, truncated = 2L)) %>%
            ggplot(aes(x=date,y=Preis))
p <- p + geom_line(linewidth=0.3,color='#232461',alpha=.7)
p <- p + labs(x="Jahr")
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + scale_y_continuous(name = expression(paste("C",O[2],"-Preis in € pro Tonne")))
p <- p + theme_light() + theme(legend.position='none',
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p
### save static plot ----
ggsave(xfun::from_root('img','ets-carbon-price_2005-2020.svg'),width=1250,height = 800,units="px")

