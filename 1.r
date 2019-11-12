# Гайдуков Сергей – для региона 28 рассчитайте урожайность пшеницы в 2011 году, 
# взяв для рассчета средние суммы активных температур за предыдущие 5 лет, с метеостанций в радиусе не более 300 км
# Амурская область - 50.268549, 127.539079 - Благовещенск

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# Получим список метеостанций
blagoveshensk = data.frame(id = "BLAGOVESHCHENSK", latitude = 55.340206,  longitude = 86.061170)
#найдем станции, соответствующие критериям
blagoveshensk_around = meteo_nearby_stations(lat_lon_df = blagoveshensk, station_data = station_data,
                                        radius = 300, var = "TAVG", 
                                        year_min = 2006, year_max = 2011)
#создадим таблицу
all_data = tibble()
for (i in 1:length(blagoveshensk_around))
{
  # Определим станцию:
  blagoveshensk_id = blagoveshensk_around[["BLAGOVESHCHENSK"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = blagoveshensk_id,
                          var="TAVG",
                          date_min="2006-01-01",
                          date_max="2011-12-31")
  #объединим данные в таблице
  all_data = bind_rows(all_data, data)
}

# Изменения в таблице сохранятся в векторе clean_data.
clean_data =   all_data %>% 
  # Группируем и находим  cумму активных температур :
  mutate(year = year(date), month = month(date)) %>%
  mutate(tavg = tavg/10) %>%
  filter(tavg > 10) %>%
  group_by(year, month, id) %>%
  summarize(summ = sum(tavg, na.rm=TRUE)) %>%
  group_by(month) %>%
  summarize(s = mean(summ, na.rm = TRUE)) %>%
  # Добавим колонки для расчета, игнорируем месяцы без данных:
  mutate (a = af[min(month):max(month)], b = bf[min(month):max(month)], d = df[min(month):max(month)]) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )

#Согласно расчету, урожайность пшеницы в Благовещенске в 2011 году составила (ц/га):
Yield = sum(clean_data$fert); Yield
