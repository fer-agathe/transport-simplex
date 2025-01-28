# Agathe Fernandes Machado
# Arthur Charpentier
# Ewen Gallic
# 2025
# https://arxiv.org/abs/2501.15549

# Objectives: illustrate how to build counterfactual for a categorical feature
# using our two methods.

# Dataset : Adult (https://archive.ics.uci.edu/dataset/2/adult)

library(tidyverse)
library(devtools)
library(ggtern)
# load the functions from our package to perform optimal transport on
# compositional data
load_all("../")

#' Theme for ggplot2
#'
#' @param ... arguments passed to the theme function
#' @export
#' @importFrom ggplot2 element_rect element_text element_blank element_line unit
#'   rel
theme_paper <- function(...) {
  theme(
    text = element_text(family = "Times New Roman"),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.text = element_text(size = rel(1.1)),
    legend.title = element_text(size = rel(1.1)),
    legend.background = element_rect(
      fill = "transparent", linetype="solid", colour ="black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.key = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold"),
    plot.title.position = "plot",
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = rel(1.1))
  )
}

# 1. Data----

data(adult, package = "fairml")

adult |> pull(marital_status) |> table()

# Variable of interest here: marital status
# Too many categories if we want to use ternary plot
# Regroup values so as to get 3 categories only.
adult <-
  adult |>
  mutate(
    marital_status = case_when(
      marital_status %in% c(
        "Married-AF-spouse", "Married-civ-spouse"
      ) ~ "Married",
      marital_status %in% c(
        "Divorced", "Separated", "Widowed", "Married-spouse-absent"
      ) ~ "Separated",
      marital_status %in% c("Never-married") ~ "Never-married",
      TRUE ~ "error"
    ),
    marital_status = factor(marital_status)
  )

prop.table(adult |> pull(marital_status) |> table())

prop_marital <-
  adult |>
  count(sex, marital_status) |>
  group_by(sex) |>
  mutate(prop = n / sum(n)) |>
  select(-n) |>
  pivot_wider(names_from = marital_status, values_from = prop)

prop_marital |>
  kableExtra::kbl(
    booktabs = TRUE, digits = 4,
  ) |>
  kableExtra::kable_paper() |>
  kableExtra::add_header_above(c(" " = 1, "Proportions" = 3))

# 2. Estimation of Scores----

# We first train four multiclass classifiers to predict `marital_status` using
# other variables.
# The four models:
# 1. GAM-MLR (1): A multinomial model with splines for three continuous variables.
# 2. GAM-MLR (2): A multinomial model with splines for three continuous
#                variables and seven additional predictors.
# 3. Random Forest: A classifier using all available variables.
# 4. Gradient Boosting Model: A GBM trained on all available variables.

library(splines)
require(nnet)

set.seed(123)
model_glm_1 <- multinom(
  marital_status ~ bs(age) + bs(hours_per_week) + occupation,
  data = adult |> select(-sex, -income)
)

model_glm_2 <- multinom(
  marital_status ~ bs(age) + bs(hours_per_week) + occupation + relationship +
    workclass + bs(education_num) + education + bs(capital_gain),
  data = adult |> select(-sex, -income)
)
library(randomForest)
model_rf <- randomForest(
  marital_status ~ ., data = adult |> select(-sex, -income)
)

library(gbm)
library(caret)

model_gbm <- gbm(
  marital_status ~.,
  data = adult |> select(-sex, -income),
  distribution = "multinomial",
  cv.folds = 10,
  shrinkage = .01,
  n.minobsinnode = 10,
  n.trees = 2000
)

# Estimated scores (representation in the simplex of the categorical
# `marital_status` variable)

scores_glm_1 <- predict(model_glm_1, type = "probs")
scores_glm_2 <- predict(model_glm_2, type = "probs")
scores_rf <- predict(model_rf, type = "prob")
scores_gbm <- predict.gbm(
  object = model_gbm,
  newdata = adult |> select(-sex, -income),
  n.trees = 2000,
  type = "response")
scores_gbm <- scores_gbm[ , , 1]

# Looking at the values
print_highlighted_obs <- function(scores) {
  ind_highlight <- c(1, 2, 5, 6)
  tb <- adult |>
    select(marital_status, sex) |>
    bind_cols(scores) |>
    slice(ind_highlight)
  row.names(tb) <- NULL
  tb
}

tb_four_indiv <-
  print_highlighted_obs(scores_glm_1) |> mutate(model = "glm_1") |>
  bind_rows(
    print_highlighted_obs(scores_glm_2) |> mutate(model = "glm_2")
  ) |>
  bind_rows(
    print_highlighted_obs(scores_rf) |> mutate(model = "rf")
  ) |>
  bind_rows(
    print_highlighted_obs(scores_gbm) |> mutate(model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("glm_1", "glm_2", "rf", "gbm"),
      labels = c("GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
                 "Gradient Boosting Model")
    )
  ) |>
  relocate(model, .before = marital_status)

tb_four_indiv |> select(-model) |>
  kableExtra::kbl(
    booktabs = TRUE, digits = 4,
  ) |>
  kableExtra::kable_paper() |>
  kableExtra::add_header_above(c(" " = 2, "Predicted Scores" = 3)) |>
  kableExtra::pack_rows(index = table(tb_four_indiv$model))

# 3. First Method (Optimal Transport in the Euclidean Representation)----

# We will work on a subset of the data here for the transport.
# Also, if the number is too large, the returned weights tend to be all equal
# to 1...

set.seed(1234)
idx <- sample(1:nrow(adult),size = 400)
adult_subset <- adult[idx, ]

ind_0 <- which(adult_subset$sex == "Female")
ind_1 <- which(adult_subset$sex == "Male")

# Representation in the simplex of the selected individuals
# GAM-MLR(1)
X0_glm_1 <- scores_glm_1[idx, ][ind_0, ]
X1_glm_1 <- scores_glm_1[idx, ][ind_1, ]
# GAM-MLR(2)
X0_glm_2 <- scores_glm_2[idx, ][ind_0,]
X1_glm_2 <- scores_glm_2[idx, ][ind_1,]
# RF
X0_rf <- scores_rf[idx, ][ind_0, ]
X1_rf <- scores_rf[idx, ][ind_1, ]
# GBM
X0_gbm <- scores_gbm[idx, ][ind_0, ]
X1_gbm <- scores_gbm[idx, ][ind_1, ]

# Little tweak for RF and GBM: add (substract) a small value to predictions
# exactly equal to 0 (1).

# RF
for (i in 1:3) {
  X0_rf[which(X0_rf[, i] == 0), i] = .0000001
  X0_rf[which(X0_rf[, i] == 1), i] = 1-.0000001
  X1_rf[which(X1_rf[, i] == 0), i] = .0000001
  X1_rf[which(X1_rf[, i] == 1), i] = 1-.0000001
}

# GBM
for (i in 1:3) {
  X0_gbm[which(X0_gbm[, i] == 0), i] = .0000001
  X0_gbm[which(X0_gbm[, i] == 1), i] = 1-.0000001
  X1_gbm[which(X1_gbm[, i] == 0), i] = .0000001
  X1_gbm[which(X1_gbm[, i] == 1), i] = 1-.0000001
}

# We apply our algorithm to transport the composition data,
# first representing them in the simplex (using clr transform)
# then applying Gaussian OT
# then aplying clr inverse to go back to the Euclidean space.
?transport_simplex
transp_glm_1 <- transport_simplex(X0 = X0_glm_1, X1 = X1_glm_1, n_interp = 31)
transp_glm_2 <- transport_simplex(X0 = X0_glm_2, X1 = X1_glm_2, n_interp = 31)
transp_rf <- transport_simplex(X0 = X0_rf, X1 = X1_rf, n_interp = 31)
transp_gbm <- transport_simplex(X0 = X0_rf, X1 = X1_gbm, n_interp = 31)

# We can then have a look at the percentage of each purpose category in the
# initial dataset, compare it with the average predicted score of each category
# for each model, and with the average of the transported predicted score for
# women.

# Proportions of each purpose level by gender in the dataset
prop_marital <-
  adult |>
  count(marital_status, sex) |>
  group_by(sex) |>
  mutate(prop = n / sum(n)) |>
  select(-n) |>
  pivot_wider(names_from = marital_status, values_from = prop) |>
  mutate(type = "Categorical")

get_table_pred_transp <- function(scores, transp_scores) {
  # Average predicted scores for each purpose level by gender
  mean_scores_by_gender <-
    adult_subset |>
    select(marital_status, sex) |>
    bind_cols(scores) |>
    group_by(sex) |>
    summarise(across(colnames(!!scores), ~mean(.x))) |>
    mutate(type = "Composition")

  # Average predicted transported score of women for each purpose level
  mean_transp_scores_women <- colMeans(transp_scores) |>
    as_tibble_row() |>
    mutate(type = "Transported", sex = "Female -> Male")

  mean_scores_by_gender |>
    bind_rows(mean_transp_scores_women)
}

tb_pred_transp_mean <-
  prop_marital |> mutate(model = "obs") |>
  bind_rows(
    get_table_pred_transp(scores_glm_1[idx, ], transp_glm_1) |>
      mutate(model = "glm_1")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_glm_2[idx, ], transp_glm_2) |> mutate(model = "glm_2")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_rf[idx, ], transp_rf) |> mutate(model = "rf")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_gbm[idx, ], transp_gbm) |> mutate(model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  ) |>
  relocate(model, .before = sex) |>
  relocate(type, .after = model)

tb_pred_transp_mean |> select(-model) |>
  kableExtra::kbl(
    booktabs = TRUE, digits = 4,
  ) |>
  kableExtra::kable_paper() |>
  kableExtra::add_header_above(c(" " = 2, "Marital Status" = 3)) |>
  kableExtra::pack_rows(index = table(tb_pred_transp_mean$model))

## Visualization----

# Interpolation displacement
# using our function
?interpolated

transp_glm_1_path <- interpolated(transp_glm_1) |>
  list_rbind(names_to = "id_obs")
transp_glm_2_path <- interpolated(transp_glm_2) |>
  list_rbind(names_to = "id_obs")
transp_rf_path <- interpolated(transp_rf) |>
  list_rbind(names_to = "id_obs")
transp_gbm_path <- interpolated(transp_gbm) |>
  list_rbind(names_to = "id_obs")

# Format scores (composition data)
scores_all <- as_tibble(scores_glm_1[idx, ]) |>
  mutate(sex = adult_subset$sex, model = "glm_1") |>
  bind_rows(
    as_tibble(scores_glm_2[idx, ]) |>
      mutate(sex = adult_subset$sex, model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(as.data.frame(scores_rf[idx, ])) |>
      mutate(sex = adult_subset$sex, model = "rf")
  ) |>
  bind_rows(
    as_tibble(scores_gbm[idx, ]) |>
      mutate(sex = adult_subset$sex, model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

# Format interpolation displacement values
transp_all_path <-
  transp_glm_1_path |>
  mutate(
    sex = factor("Female", levels = levels(adult_subset$sex)),
    model = "glm_1"
  ) |>
  bind_rows(
    transp_glm_2_path |>
      mutate(
        sex = factor("Female", levels = levels(adult_subset$sex)),
        model = "glm_2"
      )
  ) |>
  bind_rows(
    transp_rf_path |>
      mutate(
        sex = factor("Female", levels = levels(adult_subset$sex)),
        model = "rf"
      )
  ) |>
  bind_rows(
    transp_gbm_path |>
      mutate(
        sex = factor("Female", levels = levels(adult_subset$sex)),
        model = "gbm"
      )
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

ggtern(
  data = scores_all,
  mapping = aes(x = Married, y = Separated, z = `Never-married`, colour = sex)
) +
  geom_point(size = .1) +
  geom_line(
    data = transp_all_path,
    linewidth = .2, alpha = .8,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Married", y = "Never-married", z = "Separated") +
  theme_paper() +
  theme(
    tern.axis.title = element_text(size = rel(.8))
  )

# 4. Second Method (Optimal Transport in the Simplex)----

# We use our algorithm
?wasserstein_simplex

mapping_glm_1 <- wasserstein_simplex(X0_glm_1, X1_glm_1)
mapping_glm_2 <- wasserstein_simplex(X0_glm_2, X1_glm_2)
mapping_rf <- wasserstein_simplex(X0_rf, X1_rf)
mapping_gbm <- wasserstein_simplex(X0_gbm, X1_gbm)


# Scaling the transport plan to the original number of observations:
M0_glm_1 <- mapping_glm_1$plan * nrow(X0_glm_1)
M0_glm_2 <- mapping_glm_2$plan * nrow(X0_glm_2)
M0_rf <- mapping_rf$plan * nrow(X0_rf)
M0_gbm <- mapping_gbm$plan * nrow(X0_gbm)

# Let us focus on the third individual
i <- 3

# Its representation in the unit simplex
indiv_i_glm_1 <- X0_glm_1[i, ]
indiv_i_glm_2 <- X0_glm_2[i, ]
indiv_i_rf <- X0_rf[i, ]
indiv_i_gbm <- X0_gbm[i, ]

# Its estimated weights
weights_i_glm_1 <- M0_glm_1[i, ]
weights_i_glm_2 <- M0_glm_2[i, ]
weights_i_rf <- M0_rf[i, ]
weights_i_gbm <- M0_gbm[i, ]

# Its counterfactuals (transported values -> weighted average of
# characteristics in the other group (coupling))
cfact_i_glm_1 <- weights_i_glm_1 %*% X1_glm_1
cfact_i_glm_2 <- weights_i_glm_2 %*% X1_glm_2
cfact_i_rf <- weights_i_rf %*% X1_rf
cfact_i_gbm <- weights_i_gbm %*% X1_gbm

## Visualization----

# Women
tb_plot_females <-
  as_tibble(X0_glm_1) |>
  mutate(sex = "Female", model = "glm_1") |>
  bind_rows(
    as_tibble(X0_glm_2) |>
      mutate(sex = "Female", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(X0_rf) |>
      mutate(sex = "Female", model = "rf")
  ) |>
  bind_rows(
    as_tibble(X0_gbm) |>
      mutate(sex = "Female", model = "gbm",)
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

# Males individuals, with a column weights_i giving their weight used to
# construct the counterfactual for indiv i
tb_plot_males <-
  as_tibble(X1_glm_1) |>
  mutate(
    sex = "Male", model = "glm_1", weights_i = weights_i_glm_1
  ) |>
  bind_rows(
    as_tibble(X1_glm_2) |>
      mutate(
        sex = "Male", model = "glm_2", weights_i = weights_i_glm_2
      )
  ) |>
  bind_rows(
    as_tibble(X1_rf) |>
      mutate(
        sex = "Male", model = "rf", weights_i = weights_i_rf
      )
  ) |>
  bind_rows(
    as_tibble(X1_gbm) |>
      mutate(
        sex = "Male", model = "gbm", weights_i = weights_i_rf
      )
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

indiv_i <-
  as_tibble_row(indiv_i_glm_1) |> mutate(sex = "Female", model = "glm_1") |>
  bind_rows(
    as_tibble_row(indiv_i_glm_2) |> mutate(sex = "Female", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble_row(indiv_i_rf) |> mutate(sex = "Female", model = "rf")
  ) |>
  bind_rows(
    as_tibble_row(indiv_i_gbm) |> mutate(sex = "Female", model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

cfact_indiv_i <-
  as_tibble(cfact_i_glm_1) |> mutate(sex = "Male", model = "glm_1") |>
  bind_rows(
    as_tibble(cfact_i_glm_2) |> mutate(sex = "Male", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(cfact_i_rf) |> mutate(sex = "Male", model = "rf")
  ) |>
  bind_rows(
    as_tibble(cfact_i_gbm) |> mutate(sex = "Male", model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)", "Random Forest",
        "Gradient Boosting Model"
      )
    )
  )

ggtern(
  mapping = aes(x = Married, y = Separated, z = `Never-married`, colour = sex)
) +
  geom_point(
    data = tb_plot_females,
    size = .1,
    alpha = .6
  ) +
  geom_point(
    data = tb_plot_males,
    mapping = aes(size = weights_i),
    alpha = .5
  ) +
  geom_point(data = indiv_i, size = 3, colour = "white") +
  geom_point(data = indiv_i, size = 2) +
  geom_point(data = cfact_indiv_i, size = 3, colour = "white", shape = 15) +
  geom_point(data = cfact_indiv_i, size = 3, shape = 15) +
  facet_wrap(~ model) +
  labs(x = "Married", y = "Separated", z = "Never Married") +
  scale_colour_manual(
    values = c("Female" = "red", "Male" = "blue"), guide = "none"
  ) +
  scale_size_continuous(range = c(0, 1), guide = "none") +
  theme_paper() +
  theme(
    tern.axis.title = element_text(size = rel(.8))
  )

## Closer look at the individual----

adult_subset[ind_0[i], "marital_status"]

X0_glm_1[i, ]

# Closest identified points:
ind_close_points <- tail(order(weights_i_glm_1), 10)
X1_glm_1[ind_close_points, ] |>
  as_tibble() |>
  mutate(
    Purpose = adult_subset[ind_1[ind_close_points], ] |> select(marital_status),
    index = ind_close_points,
    weights_i = weights_i_glm_1[ind_close_points]) |>
  arrange(desc(weights_i)) |>
  kableExtra::kbl(booktabs = TRUE) |>
  kableExtra::kable_paper()

# Counterfactual of its composition with GAM-MLR(1):
cfact_i_glm_1

# Transported category:
colnames(cfact_i_glm_1)[which.max(cfact_i_glm_1)]

# For comparison: with Gaussian OT (first method):
transp_glm_1 |> slice(i)
