library(tidyverse)
library(tsiR)
library(glmnet)
theme_set(theme_bw())

# read in England & Wales measles data
url_loc <- paste0("https://raw.githubusercontent.com/WyattGMadden/",
                  "intro_to_ml_for_id_emory/main/data/england_and_wales_measles/measles.csv")
measles <- read_csv(url_loc)

head(measles)
str(measles)
summary(measles)

measles |>
    ggplot(aes(x = time, y = cases, color = city)) +
    geom_line()

measles |>
    mutate(city_group = cut(pop, breaks = quantile(pop), include.lowest = T)) |>
    ggplot(aes(x = time, y = cases)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~city_group, scale = "free_y")


measles |>
    mutate(cases_normalized = cases / pop) |>
    ggplot(aes(x = time, y = cases_normalized, color = city)) +
    geom_line(alpha = 0.2)


london_measles <- measles |>
    filter(city == "London")

london_measles |>
    ggplot(aes(x = time, y = cases)) +
    geom_line()

# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528
# have students investigate arguments/other parameter options

london_tsir <- runtsir(
    data = london_measles, 
    IP = 2,
    xreg = 'cumcases',
    regtype = 'gaussian',
    alpha = NULL,
    sbar = NULL,
    family = 'gaussian',
    link = 'identity',
    method = 'deterministic',
    pred = 'step-ahead')

plotres(LondonRes)

london_tsir

# one-step-ahead predictions



# will use none-seasonality, but students can explore adding seasonality
london_tsir_est <- runtsir(
    data = london_measles, 
    IP = 2,
    xreg = 'cumcases',
    regtype = 'gaussian',
    alpha = NULL,
    sbar = NULL,
    family = 'gaussian',
    seasonality = "none",
    link = 'identity',
    method = 'deterministic',
    pred = 'step-ahead',
    nsim = 1)

london_tsir_est



beta_est <- unique(london_tsir_est$beta)
alpha_est <- london_tsir_est$alpha
rho_est <- london_tsir_est$rho
S_est <- london_tsir_est$simS[, "mean"]
cases_est <- london_tsir_est$res$mean
I_est <- cases_est * rho_est

S_one_step_ahead <- rep(NA, nrow(london_measles))
I_one_step_ahead <- rep(NA, nrow(london_measles))
cases_one_step_ahead <- rep(NA, nrow(london_measles))

#births from one year ago
births_for_pred <- lag(london_measles$births, 26)

#reference forward equations here
for (i in 2:nrow(london_measles)) {
    I_one_step_ahead[i] <- beta_est * S_est[i - 1] * (I_est[i - 1])^alpha_est
    cases_one_step_ahead[i] <- I_one_step_ahead[i] / rho_est[i - 1]
    S_one_step_ahead[i] <- births_for_pred[i] + S_est[i - 1] - I_one_step_ahead[i]
}

pred_dat <- london_measles
pred_dat$cases_pred <- cases_one_step_ahead

pred_dat |>
    pivot_longer(c("cases", "cases_pred"), names_to = "true_pred", values_to = "cases") |>
    ggplot(aes(x = time, y = cases, color = true_pred, linetype = true_pred)) +
    geom_line()






########LASSO########
lasso_data <- london_measles
T_lag <- 130

lasso_data$log1_cases <- log(lasso_data$cases + 1)

# get case lags
for (i in 1:T_lag) {
    lasso_data[, paste0("log1_cases_lag", i)] <- lag(lasso_data$log1_cases, i)
}

# get mean births
lasso_data$log1_mean_births_lag <- rep(NA, nrow(lasso_data))
for (i in (T_lag + 1):nrow(lasso_data)) {
    lasso_data$log1_mean_births_lag[i] <- log(mean(lasso_data$births[(i - T_lag):(i - 1)]) + 1)
}

lasso_data_full <- lasso_data[!is.na(lasso_data$log1_mean_births_lag), ]

train_set_proportion <- 0.7
train_set_size <- round(nrow(lasso_data_full) * train_set_proportion)
lasso_data_train <- lasso_data_full[1:train_set_size, ]
lasso_data_test <- lasso_data_full[(train_set_size + 1):nrow(lasso_data_full), ]

Y_train <- lasso_data_train$cases
X_col_names <- c(grep("log1_cases_lag", names(lasso_data), value = T),
                 "log1_mean_births_lag")
X_train <- as.matrix(lasso_data_train[, X_col_names])


#estimate lambda
cv.lasso.oneahead <- cv.glmnet(
    X_train, 
    Y_train, 
    alpha = 1, 
    lower.limits = -Inf, 
    family = "poisson", 
    intercept = T)

plot(cv.lasso.oneahead)
model.oneahead <- glmnet(
    X_train, 
    Y_train, 
    alpha = 1, 
    lower.limits = lower_limits,  
    family = "poisson",
    lambda = cv.lasso.oneahead$lambda.1se, 
    intercept = intercept)


train_pred <- predict(
    model.oneahead,
    newx = X_train
    )

plot(exp(train_pred), Y_train)
mean((exp(train_pred) - Y_train)^2)

lasso_data_train$pred <- train_pred

lasso_data_train |>
    ggplot(aes(x = time)) +
    geom_line(aes(y = cases)) +
    geom_line(aes(y = exp(pred)), colour = "red")

# predict on test set
X_test <- as.matrix(lasso_data_test[, X_col_names])
test_pred <- predict(
    model.oneahead,
    newx = X_test
    )

lasso_data_test$lasso_pred <- exp(test_pred)

lasso_data_test |>
    ggplot(aes(x = time)) +
    geom_line(aes(y = cases)) +
    geom_line(aes(y = exp(lasso_pred)), colour = "red")








train_set_proportion <- 0.7
train_set_size <- round(nrow(london_measles) * train_set_proportion)
train_london <- london_measles[1:train_set_size, ]
test_london <- london_measles[(train_set_size + 1):nrow(london_measles), ]

tsir_pred <- pred_dat[pred_dat$time %in% lasso_data_test$time, ]

lasso_data_test$tsir_pred <- tsir_pred$cases_pred

lasso_data_test |>
    pivot_longer(c("cases", "tsir_pred", "lasso_pred"), 
                 names_to = "type", 
                 values_to = "value") |>
    ggplot(aes(x = time, y = value, color = type, linetype = type)) +
    geom_line() +
    labs(title = "London Measles Cases: True vs. Predicted",
         x = "Time",
         y = "Cases")

lasso_data_test$cases - lasso_data_test$tsir_pred
mean((lasso_data_test$cases - lasso_data_test$tsir_pred)^2)
mean((lasso_data_test$cases - lasso_data_test$lasso_pred)^2)
mean(abs(lasso_data_test$cases - lasso_data_test$tsir_pred))
mean(abs(lasso_data_test$cases - lasso_data_test$lasso_pred))
