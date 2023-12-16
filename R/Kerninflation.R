source(xfun::from_root("R/00_setup.R"))
# # Data description -----
# desc <- head(get_description("ICP.M.DE.N.000000+XEF000.4.ANR"), 3)
# strwrap(desc, width = 80)
#
#
# # load data -----
# dl_date <- '2023-05-22'
# my_in_file<-glue('inflation_raw_{dl_date}.rds')
# load(file=xfun::from_root("data","raw",my_in_file))
#
#
# # Data Wrangling -----
#
# # tyding and selecting ----
# tbl_plot <- inflation %>%
#                 mutate(date=convert_dates(obstime),
#                        var=factor(icp_item,labels = c("HVPI","Kerninflation")),
#                        rate=obsvalue/100
#                       ) %>%
#                 select(date,var,rate)
#
#
# ## save tidy tbl_plot -----
# my_out_file <- glue('inflation_tidy_{dl_date}.rds')
# save(tbl_plot,file=xfun::from_root("data","tidy",my_out_file))


# Dataviz -----

## load data
dl_date <- '2023-05-22'
my_in_file <- glue("inflation_tidy_{dl_date}.rds")
load(file=xfun::from_root("data","tidy",my_in_file))


## headline vs. core -----

### static plot  ------
p <- tbl_plot %>%
        filter(date>2015) %>%
        ggplot(aes(x=date,y=rate,fill=var)) +
        geom_bar(stat="identity",position="dodge") +
        scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
        scale_y_continuous(breaks=seq(0,.12,by=0.01), labels = scales::percent_format(accuracy = 1)) +
        labs(x="Zeit",y="Inflationsrate (in %)") +
        theme_light() +
        theme(legend.title=element_blank(), legend.position = "bottom",
              legend.text = element_text(size=rel(2)),
              axis.text.x = element_text(size=rel(2),angle = 45, vjust = 0.5, hjust=1),
              axis.text.y = element_text(size=rel(2)),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=rel(2))
              )
#p
#ggsave(xfun::from_root('img','HVPI_Core.svg'))

### animated plot ----
library(gganimate)
library(gifski)
transition_data <- tbl_plot %>%
                        distinct(date)
p_animated <- p +
              transition_manual(date, cumulative = TRUE,states = transition_data)

### Animation als Dauerschleife -----
p_gif <- animate(p_animated, nframes = 100,fps=8,duration=18,
                 renderer = gifski_renderer(loop = TRUE),
                 width=1150,height=750)
# Animation anzeigen
p_gif
# Animation speichern
anim_save(xfun::from_root("img/hvpi_core.gif"), p_gif)

### Animation als Dauerschleife -----
p_gif <- animate(p_animated, nframes = 100,fps=8,duration=18,
                 renderer = gifski_renderer(loop = FALSE),
                 width=1150,height=750)
p_gif
# Animation speichern
anim_save(xfun::from_root("img/hvpi_core_noloop.gif"), p_gif)


#p <- p + xaringanthemer::theme_xaringan(background_color = "#FFFFFF")
#p <- plotly::ggplotly(p, tooltip = c("x", "text")) %>%
#  layout(legend = list(title = list(text = ""),orientation = "h", x = 0.4, y = -1))
#x <- list(
#  title = "Jahr"
#)
#y <- list(
#  title = "Inflationsrate (in %)"
#)
#p <- p %>%
#      layout(xaxis = x, yaxis = y,margin = list(l = 120, b =50))
#p %>%
#  htmltools::save_html(xfun::from_root("img","HVPI_Core.html"))
#
#p <- plotly::ggplotly(p)
#p




#plotly


#wide_df <- tbl_plot %>%
#                pivot_wider(names_from = var,values_from = rate) %>%
#                filter(date>='2015-01-01')


#p <- wide_df %>%
#  plot_ly(x = ~date, y = ~HVPI, type = 'bar', name = 'HVPI')  %>%
#       add_trace(y = ~Kerninflation, name = 'Kerninflation') %>%
#       layout(xaxis=list(title="Jahr"),
#             yaxis=list(tickformat = ".1%",
#                    title="Rate (in %)")
#  )
#p %>%
#  layout(xaxis = x, yaxis = y,margin = list(l = 120, b =50),
#         legend = list(title = list(text = ""),orientation = "h", x = 0.4, y = -1)) %>%
#  htmltools::save_html(xfun::from_root("img","test.html"))




#inflation %>%
#  mutate(date=convert_dates(obstime)) %>%
  #    filter(obstime>2015) %>%
#  ggplot(aes(x=date,y=obsvalue,color=icp_item)) +
#  geom_line()

