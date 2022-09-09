library(tidyverse)
library(lubridate)
library(zoo)
library(dbplyr)
library(DBI)
library(plotly)
df <- readRDS('../data_processed/df-mirror-MikeZinov.RDS')
df_FTS <- readRDS('../data_processed/df-10-znak.RDS')
df <- df %>%
  mutate(TNVED4 = case_when(
    nchar(TNVED4)==3 ~ paste0('0', TNVED4),
    nchar(TNVED4)==4 ~ TNVED4
  )) %>%
  mutate(TNVED6 = case_when(
    nchar(TNVED6)==5 ~ paste0('0', TNVED6),
    nchar(TNVED6)==6 ~ TNVED6
  ))

df_FTS <- df_FTS %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  mutate(NETTO = as.numeric(NETTO))
# mirror dataset coverage
fts21 <- df_FTS %>%
  filter(year(PERIOD) == 2021) %>%
  group_by(year = year(PERIOD), NAPR) %>%
  summarize(STOIM = sum(STOIM, na.rm=TRUE))
mirr21 <- df %>%
  filter(year(PERIOD) == 2021) %>%
  group_by(year = year(PERIOD), NAPR) %>%
  summarize(STOIM = sum(STOIM, na.rm=TRUE))
compare21 <- fts21 %>%
  left_join(mirr21, by=c('year', 'NAPR')) %>%
  rename("FTS"="STOIM.x", "MIRR"="STOIM.y") %>%
  mutate(ratio = round(MIRR/FTS*100,1)) %>%
  ungroup()
saveRDS(compare21, 'tradeShares.RDS')

# country list
country_list <- unique(df$STRANA)
saveRDS(country_list, 'countryList.RDS')

# goods codes
goods_list_2 <- unique(df$TNVED2)
saveRDS(goods_list_2, 'goods_list_2.RDS')

goods_list_4 <- unique(df$TNVED4)
saveRDS(goods_list_4, 'goods_list_4.RDS')

goods_list_6 <- unique(df$TNVED6)
saveRDS(goods_list_6, 'goods_list_6.RDS')

# main page graphs and values
con <-
  dbConnect(RSQLite::SQLite(), "df-10-znak.sqlite")
df_FTS <- tbl(con, "fts")
df <- tbl(con, "mirr")
napr <- tbl(con, "napr")
strana <- tbl(con, "strana")
tnved <- tbl(con, "tnved")
edizm <- tbl(con, "edizm")
tnved4 <- tbl(con, "tnved4")
tnved6 <- tbl(con, "tnved6")
tnved2 <- tbl(con, "tnved2")

date <- as.Date('2022-06-01')

ex_id <- dbGetQuery(con, 'SELECT id FROM napr WHERE NAPR = (?)', params = 'ЭК')$id
im_id <- dbGetQuery(con, 'SELECT id FROM napr WHERE NAPR = (?)', params = 'ИМ')$id


# tradeHistory <- df %>%
#   group_by(PERIOD, NAPR) %>%
#   summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
#   collect() %>%
#   mutate(PERIOD = as.Date(PERIOD))
# 
# FTStradeHistory <- df_FTS %>%
#   group_by(PERIOD, NAPR) %>%
#   summarise(STOIM = sum(STOIM, na.rm = TRUE)) %>%
#   collect() %>%
#   mutate(PERIOD = as.Date(PERIOD))
# 
# tradeHistoryPlotEx <-
#   tradeHistory %>% 
#   filter(NAPR == ex_id) %>%
#   ggplot() +
#   geom_line(aes(
#     x = PERIOD,
#     y = STOIM / 10 ^ 6,
#     text = paste0(
#       'Суммарный экспорт, зеркальные данные, ',
#       PERIOD,
#       ': ',
#       round(STOIM / 10 ^ 6, 1),
#       ' млн'
#     ),
#     linetype = 'Зеркальные данные',
#     group=1),
#     col = 'blue'
#   ) +
#   geom_line(
#     data = FTStradeHistory %>%
#       filter(NAPR == ex_id) %>%
#       group_by(PERIOD) %>%
#       summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
#     aes(
#       x = PERIOD,
#       y = TOTAL / 10 ^ 6,
#       group = 2,
#       linetype = 'Данные ФТС',
#       text = paste0(
#         'Суммарный экспорт, данные ФТС, ',
#         PERIOD,
#         ': ',
#         round(TOTAL / 10 ^ 6, 1),
#         ' млн'
#       )
#     ), col = 'blue',
#   ) +
#   xlab('') +
#   ylab('млн $') +
#   scale_linetype_manual(name = '', values = c('Зеркальные данные' = 'solid', 'Данные ФТС' = 'dashed'))
# 
# 
# tradeHistoryPlotEx <-
#   ggplotly(tradeHistoryPlotEx, tooltip = 'text') %>%
#   config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
#   layout(legend = list(
#     orientation = "h",
#     x = 0.3,
#     y = -0.1
#   ))
# 
# # 1.2.2 импорт
# 
# tradeHistoryPlotIm <-
#   tradeHistory %>% 
#   filter(NAPR == im_id) %>%
#   ggplot() +
#   geom_line(aes(
#     x = PERIOD,
#     y = STOIM / 10 ^ 6,
#     text = paste0(
#       'Суммарный импорт, зеркальные данные, ',
#       PERIOD,
#       ': ',
#       round(STOIM / 10 ^ 6, 1),
#       ' млн'
#     ),
#     linetype = 'Зеркальные данные',
#     group=1),
#     col = 'blue'
#   ) +
#   geom_line(
#     data = FTStradeHistory %>%
#       filter(NAPR == im_id) %>%
#       group_by(PERIOD) %>%
#       summarise(TOTAL = sum(STOIM, na.rm = TRUE)),
#     aes(
#       x = PERIOD,
#       y = TOTAL / 10 ^ 6,
#       group = 2,
#       linetype = 'Данные ФТС',
#       text = paste0(
#         'Суммарный импорт, данные ФТС, ',
#         PERIOD,
#         ': ',
#         round(TOTAL / 10 ^ 6, 1),
#         ' млн'
#       )
#     ), col = 'blue',
#   ) +
#   xlab('') +
#   ylab('млн $') +
#   scale_linetype_manual(name = '', values = c('Зеркальные данные' = 'solid', 'Данные ФТС' = 'dashed'))
# 
# 
# tradeHistoryPlotIm <-
#   ggplotly(tradeHistoryPlotIm, tooltip = 'text') %>%
#   config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
#   layout(legend = list(
#     orientation = "h",
#     x = 0.3,
#     y = -0.1
#   ))
# 
# # 1.2.3 суммарное преобразование получившихся графиков
# 
# fig <- subplot(style(tradeHistoryPlotEx, showlegend=F), tradeHistoryPlotIm, nrows=2, margin = 0.1) %>% layout(annotations = list(
#   list(x = 0.1 , y = 1.05, text = "Экспорт", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.1 , y = 0.5, text = "Импорт", showarrow = F, xref='paper', yref='paper')))
# fig
# 
# saveRDS(fig, 'tradehistplot.RDS')


countryIds <- strana %>%
  collect()

lastMEx <- df %>% 
  filter(NAPR == ex_id) %>%
  group_by(STRANA) %>% 
  collect() %>%
  slice(which.max(PERIOD)) %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  select(PERIOD, NAPR, STRANA) %>%
  arrange(STRANA) %>%
  select(-NAPR) %>% 
  left_join(countryIds, by=c("STRANA" = "id")) %>%
  select(PERIOD, STRANA=STRANA.y)
lastMIm <- df %>% 
  filter(NAPR == im_id) %>%
  group_by(STRANA) %>% 
  collect() %>%
  slice(which.max(PERIOD)) %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  select(PERIOD, NAPR, STRANA) %>%
  arrange(STRANA) %>%
  select(-NAPR) %>% 
  left_join(countryIds, by=c("STRANA" = "id")) %>%
  select(PERIOD, STRANA=STRANA.y)

if (!all(lastMEx$PERIOD == lastMIm$PERIOD)) {
  print('Something wrong: max month for exports and imports are different')
}
dataAvailEx <- df %>% 
  filter(NAPR == ex_id) %>%
  collect() %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  group_by(STRANA, m = month(PERIOD)) %>% 
  select(PERIOD, NAPR, STRANA) %>%
  distinct() %>%
  arrange(STRANA) %>% 
  left_join(countryIds, by=c("STRANA" = "id")) %>%
  select(PERIOD, STRANA=STRANA.y) %>%
  group_by(STRANA, y = year(PERIOD)) %>% 
  count(STRANA) %>%
  spread(y, n) %>%
  left_join(lastMEx) %>%
  rename("Последний месяц"="PERIOD")

dataAvailIm <- df %>% 
  filter(NAPR == im_id) %>%
  collect() %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  group_by(STRANA, m = month(PERIOD)) %>% 
  select(PERIOD, NAPR, STRANA) %>%
  distinct() %>%
  arrange(STRANA) %>% 
  left_join(countryIds, by=c("STRANA" = "id")) %>%
  select(PERIOD, STRANA=STRANA.y) %>%
  group_by(STRANA, y = year(PERIOD)) %>% 
  count(STRANA) %>%
  spread(y, n) %>%
  left_join(lastMIm) %>%
  rename("Последний месяц"="PERIOD")

saveRDS(dataAvailIm, 'dataAvailIm.RDS')
saveRDS(dataAvailEx, 'dataAvailEx.RDS')
