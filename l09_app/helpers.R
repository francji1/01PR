# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
#library(knitr)
#kable(tail(toc))

toc[toc$title == "Life expectancy by age, sex and NUTS 2 region",]$code




unique(df$geo)

table(df$age)

cz_names = df$geo[startsWith(df$geo,"CZ")] %>% unique()


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

SHP_europe <- SHP_0 %>% 
 # select(geo = NUTS_ID, geometry) %>% 
#  inner_join(EU27, by = "geo") %>% 
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

   