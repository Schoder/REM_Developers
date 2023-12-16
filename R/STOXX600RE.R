source(xfun::from_root("R","00_setup.R"))
library(fontawesome)
library(xaringanthemer)

# Data  import ----

## Stoxx 600 Real Estate ----

### data import ----

#### specify file name and import -----
my_in_file <- "stoxx europe 600 real estate price historical data_(investing_com)_Dez23.csv"
tbl_stoxxRE <- read_csv(xfun::from_root("data","raw",my_in_file))

#### save file -----
my_out_file <- "stoxx600_europe_RE_Dez23.rds"
save(tbl_stoxxRE,file = xfun::from_root("data","raw",my_out_file))


### datawrangling ----

#### load data ----
my_in_file <- "stoxx600_europe_RE_Dez23.rds"
load(xfun::from_root("data","raw",my_in_file))

#### tidying ----
library(lubridate)
tbl_plot <- tbl_stoxxRE %>%
                janitor::clean_names() %>%
                mutate(datum=lubridate::mdy(date),
                       volumen=if_else(parse_number(vol)<100,parse_number(vol),NA),
                       change=parse_number(change_percent),
                       pos_chg = change >= 0,
                       index = "STOXX600 RE"
                       )
tbl_plot %>% visdat::vis_miss()

#def skalierungsparameter, ggf. nutzbar für sec.axis
coeff <- tbl_plot %>%
              summarise(coeff=mean(open,na.rm = TRUE)/mean(volumen,na.rm = TRUE)) %>%
              pull()

mean(tbl_plot$volumen,na.rm = TRUE)

#### save tidy tbl_plot -----
my_out_file <- "stoxx600_europe_RE_tidy_Dez23.rds"
save(tbl_plot,file=xfun::from_root("data","tidy",my_out_file))


## Stoxx 600 ----

### data import ----

#### specify file name and import -----
my_in_file <- "stoxx 600 historical data_(investing_com)_Dez23.csv"
tbl_stoxx <- read_csv(xfun::from_root("data","raw",my_in_file))

my_in_file2 <- "stoxx 600 historical data_(investing_com).csv"
tbl_stoxx_alt <- read_csv(xfun::from_root("data","raw",my_in_file2))

tbl_stoxx <- tbl_stoxx %>% 
                  full_join(tbl_stoxx_alt)


#### save file -----
my_out_file <- "stoxx600_europe_Dez23.rds"
save(tbl_stoxx,file = xfun::from_root("data","raw",my_out_file))


### datawrangling ----

#### load data ----
my_in_file <- "stoxx600_europe_Dez23.rds"
load(xfun::from_root("data","raw",my_in_file))

#### tidying ----
tbl_stoxx <- tbl_stoxx %>%
                janitor::clean_names() %>%
                mutate(datum=lubridate::mdy(date),
                       volumen=if_else(parse_number(vol)<100,parse_number(vol),NA),
                       change=parse_number(change_percent),
                       pos_chg = change >= 0,
                       index="STOXX600"
                )
tbl_stoxx %>% visdat::vis_miss()

#### save tidy tbl_stoxx -----
my_out_file <- "stoxx600_europe_tidy_Dez23.rds"
save(tbl_stoxx,file=xfun::from_root("data","tidy",my_out_file))



## combine STOXX600 and STOXX600 RE -----

### load data -----
my_in_file <- "STOXX600_Europe_RE_tidy_Dez23.rds"
load(xfun::from_root("data","tidy",my_in_file))

my_in_file <- "stoxx600_europe_tidy_Dez23.rds"
load(xfun::from_root("data","tidy",my_in_file))


period <- tbl_plot %>%
                  summarise(start=min(datum),end=max(datum))

tbl_stoxx <- tbl_stoxx %>%
                  filter(datum>=period$start&datum<=period$end)

tbl_plot <- rbind(tbl_plot,tbl_stoxx) %>%
                    select(datum,price,volumen,pos_chg,index)

#### save file -----
my_out_file <- "stoxx600_europe_RE_fin_Dez23.rds"
save(tbl_plot,file = xfun::from_root("data","tidy",my_out_file))




# Dataviz -----

## load data----

my_in_file <- "stoxx600_europe_RE_fin_Dez23.rds"
load(xfun::from_root("data","tidy",my_in_file))

### Stoxx600 vs. stoxx600 RE ----
tbl_plot2 <- tbl_plot %>%
                   group_by(index) %>%
                   mutate(p_index=price/last(price)*100)

p <- tbl_plot2 %>%
  ggplot(aes(x=datum,y=p_index,color=index))
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
ggsave(xfun::from_root('img','STOXX600_vs_RE.svg'),width=1250,height = 900,units="px")





## STOXX 600 RE with volume ----
p <- tbl_plot %>%
            filter(index=="STOXX600 RE") %>%
            ggplot(aes(x=datum,y=price))
p <- p + geom_line(linewidth=0.3,color='#232461',alpha=.7)
p <- p + geom_col(aes(y=volumen*5,fill=pos_chg),show.legend = FALSE)
p <- p + labs(x="Jahr")
p <- p + scale_y_continuous(name = "Kurs in €",
                            sec.axis = sec_axis(~.*.1, name="Volumen")) +
         scale_x_date(date_breaks = "1 year", date_labels = "%Y")
p <- p + theme_light() + theme(legend.position='none',
                               axis.text.x=element_text(size=rel(.5)),
                               axis.text.y=element_text(size=rel(.5)),
                               axis.title.x = element_text(size=rel(.5)),
                               axis.title.y = element_text(size=rel(.5))
                               )
p
### save static plot ----
ggsave(xfun::from_root('img','STOXX600RE.svg'),width=1250,height = 900,units="px")

# ### apply xaringan-layout ------
# p <- p + xaringanthemer::theme_xaringan(background_color = "#FFFFFF")





## interactive plot -----


### ggplotly -----
# problem: layout
# p <- plotly::ggplotly(p,tooltip = c("x", "y")) %>%
#         layout(legend = list(title = list(text = ""),orientation = "h", x = 0.4, y = -1))
# x <- list(
#   title = "Jahr"
# )
# y <- list(
#   title = "Eröffnungskurs in €"
# )
# p <- p %>% layout(xaxis = x, yaxis = y,margin = list(l = 120, b =50))
# p
# p %>%
# htmltools::save_html(xfun::from_root("img","STOXX600RE.html"))



### plot_ly -----

#first try - problem - fill of bars wrt pos_chg does not work
# p <- tbl_plot %>%
#         plot_ly( x = ~Datum, y = ~Open, type = "scatter", mode = "lines", line = list(width = 0.5)) %>%
#         add_trace(y = ~Volumen * 5, type = "bar", marker = list(color = ~pos_chg), showlegend = FALSE) %>%
#         layout(
#           yaxis = list(title = "Eröffnungskurs in €"),
#           yaxis2 = list(
#             title = "Volumen",
#             overlaying = "y",
#             side = "right",
#             tickformat = ".0%",
#             scaleratio = 0.1
#           ),
#           legend = list(orientation = "v", x = 1, y = 1)
#         )
# p


# # kindof ok
# p <- tbl_plot %>%
#   plot_ly( x = ~Datum, y = ~Open, type = "scatter", mode = "lines", line = list(width = 0.5,
#                                                                                 color='rgb(55, 83, 109)')) %>%
#   add_trace(y = ~Volumen * 4, type = "bar", color = ~pos_chg, showlegend = FALSE) %>%
#   layout(
#     yaxis = list(title = "Eröffnungskurs in €"),
#     yaxis2 = list(
#       title = "Volumen",
#       overlaying = "y",
#       side = "right",
#       tickformat = ".0%",
#       scaleratio = 0.1
#     ),
#     legend = list(orientation = "v", x = 1, y = 1)
#   )
# p
