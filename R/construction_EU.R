# Data Import ----

## tibble im "raw data Ordner" speichern ----
dl_date <-'2023-05-25'

## load data ----

### Baugenehmigungen -----
my_in_file<-glue('EU_buildpermits_raw_{dl_date}.rds')
load(file=xfun::from_root("data","raw",my_in_file))

### Baugewerbe ----
my_in_file<-glue('EU_construction_raw_{dl_date}.rds')
load(file=xfun::from_root("data","raw",my_in_file))


# Data Wrangling -----


cntry_list <- c("EU_27_2020","HU","CZ","LT","PT","DE","AT","FR","ES","SE","IT","CY")


p <- tbl_permits %>%
            filter(geo%in%cntry_list&indic_bt=="PSQM"&unit=="MIO_M2") %>%
            ggplot(aes(x=time,y=values,color=geo))
p <- p + geom_line()
p
