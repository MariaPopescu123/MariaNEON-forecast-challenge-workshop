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
#3/26
#install.packages("duckdbfs", type = "binary")
library(tidyverse)
all_results <- duckdbfs::open_dataset("s3://bio230014-bucket01/challenges/forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature",
                                      s3_endpoint = "sdsc.osn.xsede.org", anonymous = TRUE)

#s3://anonymous@bio230014-bucket01/challenges/forecasts/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature/model_id=climatology?endpoint_override=sdsc.osn.xsede.org

my_forecast <- all_results|>
  filter(reference_datetime == as_datetime("2025-03-24"), 
         site_id == "BARC", 
         model_id %in% c("climatology", "persistenceRW"))|>
  collect()

my_forecast |>
  filter(model_id == "climatology")|>
  pivot_wider(names_from = "parameter", values_from = prediction)|>
  mutate(lower_2.5 = mu- 1.96*sigma, #for 95% interval
         upper_97.5 = mu + 1.96*sigma)|>
  ggplot(aes(d = datetime))+
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_97.5), fill = "lightblue")+
  geom_line(aes(y = mu))

my_forecast |>
  filter(model_id == "persistenceRW")|>
  ggplot(aes(x = datetime, y = prediction, group = parameter))+
  geom_line()

single_forecast_paramatric <- my_forecast|>
  filter(model_id == "climatology", 
         datetime == as_datetime("2025-03-27 00:00:00"))

single_forecast_paramatric <- single_forecast_paramatric|>
  pivot_wider(names_from = "parameter", values_from = prediction)
  
crps_norm(y = targets_single_forecast$observation, 
          mean = single_forecast_paramatric$mu, 
          sd = single_forecast_paramatric$sigma)

install.packages("scoringRules")
library(scoringRules)

targets <- read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz")

targets_single_forecast <- targets|>
  filter(datetime == as_datetime("2025-03-27 00:00:00"), 
         variable == "temperature", 
         site_id == "BARC")

crps_sample(y = targets_single_forecast$observation,
            dat = single_forecast$prediction)

targets_temperature <- targets|>
  filter(variable == "temperature")


multi_forecast_ensemble <- my_forecast|>
  filter(model_id == "persistenceRW")|>
  left_join(targets_temperature, by = c("datetime", "site_id"))|>
  select(datetime, prediction, observation, parameter)

multi_forecast_parametric  <- my_forecast|>
  filter(model_id == "climatology")|>
  left_join(targets_temperature, by = c("datetime", "site_id"))|>
  select(datetime, prediction, observation, parameter)

multi_forecast|>
  group_by(datetime, site_id, variable, reference_datetime) |>
  summarize(crps = crps_tidy(prediction, observation))

normal_crps <- multi_forecast_parametric|>
  

crps_tidy_ensemble <- function(prediction, observation){
  obs <- observation[1]
  crps_sample(obs,prediction)
}

ensemble_crps <- multi_forecast_ensemble|>
  group_by(datetime)|>
  summarize(crps = crps_tidy_ensemble(prediction, observation))

ensemble_crps|>
  ggplot(aes(y = crps))+
  geom_histogram()
