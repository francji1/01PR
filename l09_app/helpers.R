# Helpers functions
filter_df <- function(df, country_name = NA, input_age = "Y_LT1", input_sex = "T", time_min = 1990, time_max = 2020){
  
  if (is.na(country_name)){
    country_names = df$geo %>% unique()
  }else{
    country_names = df$geo[startsWith(df$geo, country_name)] %>% unique()
  }
  
  df_country <- df %>% 
    filter(age == input_age,
           geo %in% country_names,
           sex %in% input_sex,
           time %in% c(time_min:time_max))
  df_country
}