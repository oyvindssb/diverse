url <- "http://data.ssb.no/api/v0/dataset/1086.json?lang=no"
kpi_json <- fromJSONstat(url)
tabell <- kpi_json[[1]]

kpi <- 
  tabell %>%
  filter(statistikkvariabel=="Konsumprisindeks (2015=100)") %>% 
  as_tibble()

kpi <-
  kpi %>%
  separate(måned, into=c("år", "måned"), sep="M") %>% 
  mutate(dato=ymd(paste(år, måned, "1"))) %>% 
  select(dato, konsumgruppe, statistikkvariabel, value)

library(stringr)
b2019_november <- 
  kpi %>%
  mutate(year_month = str_sub(dato, 1, 7)) %>%
  filter(year_month== "2019-10") %>%
  summarise(ny_basis_2019_nov=value)

kpi <- 
  kpi %>%
  mutate(KPI_2019_nov=100*value/b2019_november$ny_basis_2019_nov)


## ----------------------------------------------------------------------------------------------------------
kpi %>%
  rename(KPI_2015=value) %>%
  select(dato, KPI_2015, KPI_2019_nov) %>% 
  pivot_longer(-dato,
               names_to = "KPI",
               values_to = "indeks") %>% 
  ggplot(aes(x=dato, y=indeks, col=KPI)) +
  geom_line() +
  labs(title="Konsumprisindeks - KPI",
       x =" ",
       y = "Totalindeks") +
  theme_bw()
