source(xfun::from_root("R/00_setup.R"))
# Load data ----

## EU ----

### tibble im "raw data Ordner"  ----
dl_date <-'2023-05-22'
my_in_file<-glue('houseprices_1980-2022_raw_{dl_date}.rds')

### load data ----
load(file=xfun::from_root("data","raw",my_in_file))


## Schweiz -----

### tibble im "raw data Ordner"  ----
dl_date <-'2023-05-22'
my_in_file<-glue('IMPI_2019-2023_{dl_date}.rds')

### load data ----
load(file=xfun::from_root("data","raw",my_in_file))



# Data Wrangling -----

## EU --------

### tidying ----
tbl_plot_EU <- tbl_hp_raw %>%
                  filter(unit=="INX_Q") %>%
                  separate(TIME_PERIOD, into = c("jahr", "quartal_num"), sep = "-") %>%
                  mutate(date = ymd(paste(jahr, quartal_num, "01", sep = "-"))) %>%
                  rename(hp_index=OBS_VALUE) %>%
                  select(date,geo,unit,hp_index)


### save tidy tbl_plot -----
my_out_file <- "EU_houseprices_1980-2022_tidy.rds"
save(tbl_plot_EU,file=xfun::from_root("data","tidy",my_out_file))

## Schweiz --------

### tidying ----
tbl_plot_CH <- tbl_impi_raw %>%
                    janitor::clean_names() %>%
                    rename(quartal=starts_with("totalindex_")) %>%
                    separate(quartal, into = c("quartal_num","jahr"), sep = " ") %>%
                    filter(jahr%in%c(2019:2023)) %>%
                    mutate(date = ymd(paste(jahr, quartal_num, "01", sep = "-")),
                           geo="CH") %>%
                    rename(hp_index=total) %>%
                    select(date,geo,hp_index)


### save tidy tbl_plot -----
my_out_file <- "CH_IMPI_2019-2023_tidy.rds"
save(tbl_plot_CH,file=xfun::from_root("data","tidy",my_out_file))




## Merge Data EU and CH ----

## load data -----
my_in_file <- "CH_IMPI_2019-2023_tidy.rds"
load(file=xfun::from_root("data","tidy",my_in_file))

ref_period <- tbl_plot_CH %>%
                      filter(hp_index==100) %>%
                      pull(date)



## Umbasierung EU-Data auf 2019 ----
my_in_file <- "EU_houseprices_1980-2022_tidy.rds"
load(xfun::from_root("data","tidy",my_in_file))

hp_index_ref <- tbl_plot_EU%>%
                    group_by(geo) %>%
                    filter(date==ref_period) %>%
                    rename(hp_ref=hp_index) %>%
                    select(geo,hp_ref)

tbl_plot_EU2019 <- tbl_plot_EU %>%
                    left_join(hp_index_ref,by=join_by(geo)) %>%
                    mutate(hp_index_2019=hp_index/hp_ref*100) %>%
                    select(date,geo,hp_index_2019) %>%
                    rename(hp_index=hp_index_2019)




### combine data ----
tbl_plot_EU_CH <- rbind(tbl_plot_CH,tbl_plot_EU2019)

### save merged tidy tbl_plot -----
my_out_file <- "EU-CH_houseprices_tidy.rds"
save(tbl_plot_EU_CH,file=xfun::from_root("data","tidy",my_out_file))


# Dataviz -----


## Plots EU ------

### load data----
my_in_file <- "EU_houseprices_1980-2022_tidy.rds"
load(xfun::from_root("data","tidy",my_in_file))

### static plot ----
p <- tbl_plot_EU %>%
  ggplot(aes(x=date,y=hp_index,color=geo))
p <- p + geom_line(linewidth=0.5,alpha=.7)
p <- p + labs(x="Jahr",y="Hauspreisindex (2015=100)")
p <- p + guides(color=guide_legend(ncol=5))
p <- p + scale_y_continuous()
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p_EU <- p
#ggplotly(p_EU)


### selected countries -----
cntries <- tbl_plot_EU %>% distinct(geo)

cntry_list <- c("HU","CZ","LT","PT","DE","AT","FR","ES","SE","IT")

p <- tbl_plot_EU %>%
          filter(geo%in%cntry_list) %>%
          ggplot(aes(x=date,y=hp_index,color=geo))
p <- p + geom_line(linewidth=0.5,alpha=.7)
p <- p + labs(x="Jahr",y="Hauspreisindex (2015=100)")
p <- p + guides(color=guide_legend(ncol=5))
p <- p + scale_y_continuous()
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p_EUselect <- p
#p_EUselect
ggsave(xfun::from_root('img','houseprices_EU_select.svg'),width=1250,height = 950,units="px")
ggsave(xfun::from_root('img','houseprices_EU_select_small.svg'),width=1250,height = 750,units="px")



## Plots EU + CH -----

### load data----

my_in_file <- "EU-CH_houseprices_tidy.rds"
load(xfun::from_root("data","tidy",my_in_file))

### static plot ----
p <- tbl_plot_EU_CH %>%
         ggplot(aes(x=date,y=hp_index,color=geo))
p <- p + geom_line(linewidth=0.5,alpha=.7)
p <- p + labs(x="Jahr",y="Hauspreisindex (2019=100)")
p <- p + guides(color=guide_legend(ncol=6))
p <- p + scale_y_continuous()
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
#p_EU_CH <- p
#ggplotly(p_EU_CH)


### plot selection
cntry_list <- c("HU","CZ","LT","PT","DE","AT","CH","FR","ES","SE","IT","CY")
p <- tbl_plot_EU_CH %>%
            filter(geo%in%cntry_list&date>="2019-01-01") %>%
            ggplot(aes(x=date,y=hp_index,color=geo))
p <- p + geom_line(linewidth=0.5,alpha=1)
p <- p + labs(x="Jahr",y="Hauspreisindex (April 2019 = 100)")
p <- p + guides(color=guide_legend(ncol=6))
p <- p + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + scale_y_continuous(breaks = scales::breaks_pretty(n=5))
p <- p + theme_light() + theme(legend.position='bottom',
                               legend.title = element_blank(),
                               legend.text = element_text(size=rel(.5)),
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
)
p


### save static plot ----
ggsave(xfun::from_root('img','houseprices_EU-CH.svg'),width=1250,height = 950,units="px")
ggsave(xfun::from_root('img','houseprices_EU-CH_small.svg'),width=1250,height = 750,units="px")

### interactive plot ----
p_EU_CH2019 <- p
#ggplotly(p_EU_CH2019)
