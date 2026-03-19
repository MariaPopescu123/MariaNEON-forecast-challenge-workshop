library(arrow)
library(tidyverse)

url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"
targets <- readr::read_csv(url, show_col_types = FALSE)

focal_targets <- targets|>
  filter(site_id == "SUGG", 
         datetime %in% unique(df$datetime), 
         variable == "temperature")

all_results <- arrow::open_dataset("s3://anonymous@bio230014-bucket01/challenges/forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature/model_id=cb_prophet?endpoint_override=sdsc.osn.xsede.org")
df <- all_results |> dplyr::collect()

df <- all_results |> distinct(reference_datetime) |> arrange(date(reference_datetime)) |> dplyr::collect()

df <- all_results |> filter(reference_datetime == as_datetime("2024-02-06 00:00:00"), site_id == "SUGG") |> collect()

df|>
  ggplot()+
  geom_line(aes(x = datetime, y = prediction, group = parameter))+
  geom_point(data = focal_targets, aes(x = datetime, y = observation), color = "red")


#original exmple that ended up not working
all_results <- arrow::open_dataset("s3://anonymous@bio230014-bucket01/challenges/forecasts/bundled-summaries?endpoint_override=sdsc.osn.xsede.org")
df <- all_results |>
  dplyr::filter(variable %in% c("oxygen", "temperature", "chla")) |>
  dplyr::collect()

df <- df |>
  filter(reference_datetime == as_datetime("2024-02-06 00:00:00"),
         site_id == "SUGG", 
         model_id %in% c("cb_prophet", "climatology"))|>
  dplyr::collect()


#second 

