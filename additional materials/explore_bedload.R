library(imputeTS)
library(clock)
library(see)
library(readxl)

theme_set(theme_lucid() +
            theme(legend.position = "bottom"))

djan_bed16 <- read_xlsx("D:/work/Отчет по Джанкуату 2023/Влекомые наносы, кавказ гидормет/влекомые наносы Толя/sediments_2015_2017.xlsx",
                      sheet = "vlekomye2016") %>% 
  dplyr::select(datetime = 2,
                bedload = 9) %>% 
  mutate(datetime = force_tz(datetime, "Europe/Moscow")) %>% 
  drop_na(datetime) %>% 
  mutate(datetime = round_date(datetime,
                               unit = "hour"))

djan_bed17 <- read_xlsx("D:/work/Отчет по Джанкуату 2023/Влекомые наносы, кавказ гидормет/влекомые наносы Толя/sediments_2015_2017.xlsx",
                        sheet = "vlekomye2017") %>% 
  dplyr::select(datetime = 4,
                bedload = 26) %>% 
  mutate(datetime = force_tz(datetime, "Europe/Moscow")) %>% 
  drop_na(datetime) %>% 
  mutate(datetime = round_date(datetime,
                               unit = "hour"))

djan_sed_all <- djan_hydro %>% 
  mutate(ssd = ssc * q) %>% 
  mutate(ssd = imputeTS::na_interpolation(ssd)) %>% 
  right_join(
    bind_rows(djan_bed16,
              djan_bed17),
    by = "datetime"
  ) %>% 
  transmute(datetime,
            ssd,
            bsd = bedload*1000/60)

bedload_graph <- djan_sed_all %>%
  # mutate_at(vars(ssd, bsd),
            # ~log10(.)) %>%
  rename(`Взвешенные` = ssd,
         `Влекомые` = bsd) %>% 
  gather(type, rate, -datetime) %>%
  mutate(date = format(datetime,
                       format = "%d %b")) %>% 
  mutate(date = as_factor(date)) %>% 
  ggplot(aes(x = date,
             y = rate/1000,
             fill = type,
             color = type)) +
  geom_col(width = .4) +
  labs(y = "Расход наносов, кг/с",
       x = "",
       color = "",
       fill = "") +
  scale_color_metro() +
  scale_fill_metro() +
  facet_wrap(~year(datetime),
             scales = "free") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5))
  
ssd_vs_bsd <- djan_sed_all %>% 
  mutate_at(vars(ssd, bsd),
            ~log10(.)) %>%
  ggplot(aes(x = ssd,
             y = bsd,
             color = as.factor(year(datetime)))) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F) +
  # geom_point(aes(color = as.factor(year(datetime)))) +
  atslib::Add_R2(add_line = F)

djan_sed_all %>% 
  mutate(bp = 100*bsd/(bsd+ssd),
         sp = 100*ssd/(bsd+ssd)) %>% 
  ggplot(aes(x = datetime,
             y = bp)) +
  geom_line() +
  facet_wrap(~year(datetime),
             scales = "free_x")

djan_sed_all %>% 
  mutate(bp = 100*bsd/(bsd+ssd),
         sp = 100*ssd/(bsd+ssd)) %>% 
  summary()

# save
djan_sed_all %>% 
  transmute(datetime = as.character(datetime),
            `Взвешенные` = atslib::smart_round(ssd),
            `Влекомые` = atslib::smart_round(bsd)) %>% 
writexl::write_xlsx("analysis/sediments_bedload.xlsx")


ggsave("bedload-suspended.png",
       bedload_graph,
       dpi = 500,
       h = 4,
       w = 10)
