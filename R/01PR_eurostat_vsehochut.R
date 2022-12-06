#list_of_packages <- c("tidyverse", "leaflet", "sf", "units", "scales", "cowplot", "ggthemes", "eurostat") 
#missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
#missing_packages
#if(length(missing_packages)) install.packages(missing_packages)
#lapply(list_of_packages, library, character.only = TRUE)


library(tidyverse)
library(Rcpp)
library(sf)
library(scales)
library(leaflet)
library(cowplot)
library(ggthemes)
library(eurostat)


# Get Eurostat data listing
toc <- get_eurostat_toc()


df_u <- get_eurostat("une_rt_m", time_format = "date")
df_u
df_g <- get_eurostat("namq_10_gdp", time_format = "date")
df_g
df_b <- get_eurostat("sts_rb_q", time_format = "date")
df_b

table(df_u$time)
table(df_g$time)
table(df_g$unit)

df_g2 = df_g %>% 
  filter(unit == "CLV_PCH_SM",
         na_item == "B1GQ") %>% 
  select(geo,time,values) %>% 
  rename(gdp_change = values)

df_g2
with(df_g2[df_g2$geo == "CZ",], hist(values))
with(df_g2[df_g2$geo == "CZ",], plot(time,values))

df_b2 = df_b %>% 
  filter(indic_bt == "REG",
         unit == "PCH_PRE",
         nace_r2 == "B-S_X_O_S94") %>% 
  select(geo,time,values) %>% 
  rename(br = values)

  
df_b2
with(df_b2[df_b2$geo == "PL",], hist(values))
with(df_b2[df_b2$geo == "PL",], plot(time,values))


df_u2 = df_u %>% 
  filter(age == "TOTAL",
         unit == "PC_ACT") %>% 
  mutate(time = lubridate::quarter(time, type = "date_first")) %>% 
  group_by(time,geo, sex) %>% 
  summarise(ur = mean(values)) %>% 
  pivot_wider(
    names_from = sex,
    values_from = ur)
  
df_u2
with(df_u2[df_u2$geo == "PL",], hist(values))
with(df_u2[df_u2$geo == "PL",], plot(time,values))


df = df_g2 %>% 
  left_join(df_b2) %>% 
  left_join(df_u2)
  
df

quarter

toc[toc$title == "Life expectancy by age, sex and NUTS 2 region",]$code

df <- get_eurostat("namq_10_gdp", time_format = "date")
df$time %>% unique()

NAMQ_10_GDP

df <- get_eurostat("demo_r_mlifexp", time_format = "num")
df

df <- get_eurostat("une_rt_m", time_format = "date")
df$time
UNE_RT_M   

table(df$sex)

unique(df$geo)

table(df$age)

cz_names = df$geo[startsWith(df$geo,"CZ")] %>% unique()

str_sub(df$geo, end = 2) %>% unique()

c(2000:2022)

df_cz <- df %>% 
  filter(age == "Y_LT1",
         geo %in% cz_names,
         sex %in% c("F","M")) %>% 
  mutate_if(is.character,as.factor)
  
df_cz

str(df_cz)

ggplot(df_cz, aes(x=time, y=values, color=geo)) + 
    geom_line(aes(linetype = sex))



df_eu <- df %>% 
  filter(age == "Y_LT1",
         sex %in% c("F")) 



SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 2, 
                                 year = 2016)

SHP_0$geometry

SHP_0$geometry[1]
# https://geocompr.robinlovelace.net/
# https://r-spatial.github.io/sf/articles/sf1.html
class(SHP_0)
class(SHP_0$geometry)

SHP_0$geometry[1:8][[8]] 

SHP_0$geometry[[1]]

methods(class = "sf")

SHP_0_df <- as.data.frame(SHP_0)
class(SHP_0_df$geometry)


par(mar = c(0,0,1,0))
plot(SHP_0[1], reset = FALSE) # reset = FALSE: we want to add to a plot with a legend
plot(SHP_0[1:100,1], col = 'grey', add = TRUE)

attributes(SHP_0$geometry)

st_bbox(SHP_0$geometry)$xmin
st_bbox(SHP_0$geometry)$xmax

st_bbox(SHP_0)

SHP_0[1,1:50]

SHP_europe <- SHP_0 %>% 
 # select(geo = NUTS_ID, geometry) %>% 
 # inner_join(EU27, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf() 

SHP_europe %>% 
  ggplot() +
  geom_sf()


SHP_europe %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()


df_country <- df %>% 
  filter(age == "Y_LT1",
         geo %in% cz_names,
         sex %in% c("T"),
         time %in% c(2000:2020)) %>% 
  group_by(geo) %>% 
  summarise(mean_value = mean(values))

df_country

table(df_country$time)


get_eurostat_geospatial(resolution = 10, 
                          nuts_level = 2, 
                          year = 2016) %>% 
  arrange(geo) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()



df_eu

#df_eu_shp <- df_eu %>% 
#  filter(unit == 'YR') %>% 
#  filter(time == 2020) %>% 
#  select(geo, values) %>% 
#  inner_join(SHP_27, by = "geo") %>% 
#  st_as_sf()


df_eu_to_plot <- df_eu %>% 
  filter(unit == 'YR') %>% 
  filter(time == 2020) %>% 
  select(geo, values)
df_eu_to_plot

df_eu_shp <-  SHP_europe %>% 
  left_join(df_eu_to_plot, by = "geo") %>% 
  st_as_sf()


df_eu_shp %>% 
  ggplot(aes(fill = values)) +
  scale_fill_gradientn(limits = c(70,90),
                       colours=c("red", "green"),
                       breaks=seq(70,90,2)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()

df_eu_shp %>% 
  ggplot(aes(fill = values)) +
  scale_fill_continuous(type = "viridis") +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()



df_cz_to_plot <- df_eu %>% 
  filter(unit == 'YR',
         time == 2020,
         geo %in% cz_names) %>% 
  select(geo, values)
df_cz_to_plot

df_cz_shp <-  SHP_europe %>% 
  left_join(df_cz_to_plot, by = "geo") %>% 
  st_as_sf()

df_cz_shp %>% 
  ggplot(aes(fill = values)) +
  scale_fill_continuous(type = "viridis") +
  geom_sf() +
  scale_x_continuous(limits = c(10, 20)) +
  scale_y_continuous(limits = c(48, 52)) +
  theme_void()

   