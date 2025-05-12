library(dplyr)
library(ggplot2)
library(lubridate)
library(here)

# This demonstrates the use of sinus and cosinus transformations of day-of-year
# for (linear regression) modelling

# Additional reading:
# https://developer.nvidia.com/blog/three-approaches-to-encoding-time-information-as-features-for-ml-models/?utm_source=chatgpt.com
# https://hess.copernicus.org/articles/23/3423/2019/

# Set seed for reproducibility
set.seed(42)

# Create cyclic data: sine wave with noise
amplitude <- 5
frequency <- 1/365  # 1 cycle per 2*pi units
noise_level <- 1

# Generate time sequence
df <- tibble(
  date = seq(
    from = ymd("2000-01-01"), 
    to = ymd("2005-12-31"), 
    by = "day"
  )
) |> 
  mutate(
    doy = yday(date)
  ) %>%
  mutate(
    y = amplitude * sin(2 * pi * frequency * (doy - 60)) + rnorm(nrow(.), mean = 0, sd = noise_level),
    sin_doy = sin(2 * pi * doy / 365),
    cos_doy = cos(2 * pi * doy / 365)
  ) |> 
  mutate(
    doy_trans = sin_doy + cos_doy
  )

df |> 
  ggplot(aes(date, y)) +
  geom_point()

df |> 
  ggplot(aes(doy, y)) +
  geom_point()

df |> 
  ggplot(aes(doy_trans, y)) +
  geom_point()

linmod_doy <- lm(y ~ doy, data = df)
linmod_doy_trans <- lm(y ~ doy_trans, data = df)
linmod_sin_cos <- lm(y ~ sin_doy + cos_doy, data = df)
linmod_sin <- lm(y ~ sin_doy, data = df)

summary(linmod_doy)
summary(linmod_doy_trans)
summary(linmod_sin_cos)
summary(linmod_sin)

df <- df %>%
  mutate(
    y_pred_doy = predict(linmod_doy, newdata = .),
    y_pred_doy_trans = predict(linmod_doy_trans, newdata = .),
    y_pred_sin_cos = predict(linmod_sin_cos, newdata = .),
    y_pred_sin = predict(linmod_sin, newdata = .)
  )

df |> 
  ggplot() +
  geom_point(aes(date, y)) +
  geom_line(aes(date, y_pred_doy), color = "red")

df |> 
  ggplot() +
  geom_point(aes(date, y)) +
  geom_line(aes(date, y_pred_doy_trans), color = "red")

df |> 
  ggplot() +
  geom_point(aes(date, y)) +
  geom_line(aes(date, y_pred_sin_cos), color = "red")

df |> 
  ggplot() +
  geom_point(aes(date, y)) +
  geom_line(aes(date, y_pred_sin), color = "red")
