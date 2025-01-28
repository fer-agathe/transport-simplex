# Agathe Fernandes Machado
# Arthur Charpentier
# Ewen Gallic
# 2025
# https://arxiv.org/abs/2501.15549

# Objectives: illustrate how to build counterfactual for a categorical feature
# using our two methods.

# Dataset: German Credit
# Hofmann, H. 1994. "German Credit Data."
# UCI Machine Learning Repository.
# https://doi.org/10.24432/C5NC77.

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

# The dataset is available in the {fairml} package
# https://cran.r-project.org/web/packages/fairml/index.html

# install.packages("fairml")
data(german.credit, package = "fairml")
str(german.credit)

# Variable of interest here: `Purpose` (purpose of the credit)
# 10 categories: we regroup some categories to have only 3
# (because we can visualize what happens with 3 categories)

german.credit <-
  german.credit |>
  mutate(
    Purpose = case_when(
      Purpose %in% c("car (new)","car (used)") ~ "cars",
      Purpose %in% c(
        "domestic appliances", "furniture / equipment", "radio / television"
      ) ~ "equipment",
      TRUE ~ "other"
    ) |> as.factor()
  )

# 2. Estimation of scores----

# We first train four multiclass classifiers to predict `Purpose` using
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
  Purpose ~ bs(Credit_amount) + bs(Age) + bs(Duration),
  data = german.credit
)
model_glm_2 <- multinom(
  Purpose ~ bs(Credit_amount) + bs(Age) + bs(Duration) +
    Present_employment_since + Savings_bonds + Property + Account_status +
    Credit_history + Resident_since + Job + Housing,
  data = german.credit,
)
library(randomForest)
model_rf <- randomForest(Purpose ~ ., data = german.credit[, -c(20,21)])
library(gbm)
library(caret)
model_gbm <- gbm(
  Purpose ~ .,
  data = german.credit[, -c(20,21)],
  distribution = "multinomial",
  cv.folds = 10,
  shrinkage = .01,
  n.minobsinnode = 10,
  n.trees = 2000
)

# The predicted scores:
scores_glm_1 <- predict(model_glm_1, type = "probs")
scores_glm_2 <- predict(model_glm_2, type = "probs")
scores_rf <- predict(model_rf, type = "prob")
scores_gbm <- predict.gbm(
  object = model_gbm,
  newdata = german.credit[, -c(20,21)],
  n.trees = 2000,
  type = "response"
)
scores_gbm <- scores_gbm[ , , 1]

# Looking at predicted values for individuals 2, 5, 13 and 8

print_highlighted_obs <- function(scores) {
  ind_highlight <- c(2, 5, 13, 8)
  tb <- german.credit |>
    select(Purpose, Gender) |>
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
  relocate(model, .before = Purpose)

tb_four_indiv |> select(-model) |>
  kableExtra::kbl(
    booktabs = TRUE, digits = 4,
  ) |>
  kableExtra::kable_paper() |>
  kableExtra::add_header_above(c(" " = 2, "Predicted Scores" = 3)) |>
  kableExtra::pack_rows(index = table(tb_four_indiv$model))

# 3. First Method (Gaussian OT in the Euclidean Representation)----
ind_0 <- which(german.credit$Gender == "Female")
ind_1 <- which(german.credit$Gender == "Male")

# GAM-MLR(1)
X0_glm_1 <- scores_glm_1[ind_0,]
X1_glm_1 <- scores_glm_1[ind_1,]
# GAM-MLR(2)
X0_glm_2 <- scores_glm_2[ind_0,]
X1_glm_2 <- scores_glm_2[ind_1,]
# RF
X0_rf <- scores_rf[ind_0,]
X1_rf <- scores_rf[ind_1,]
# GBM
X0_gbm <- scores_gbm[ind_0,]
X1_gbm <- scores_gbm[ind_1,]

# Transport using our algorithm
?transport_simplex
transp_glm_1 <- transport_simplex(X0 = X0_glm_1, X1 = X1_glm_1, n_interp = 31)
transp_glm_2 <- transport_simplex(X0 = X0_glm_2, X1 = X1_glm_2, n_interp = 31)
transp_rf <- transport_simplex(X0 = X0_rf, X1 = X1_rf, n_interp = 31)
transp_gbm <- transport_simplex(X0 = X0_rf, X1 = X1_gbm, n_interp = 31)

# Percentage of each purpose category in the initial dataset,
# Comparison with the average predicted score of each category for each model,
# and with the average of the transported predicted score for women.
# Proportions of each purpose level by gender in the dataset
prop_purposes <-
  german.credit |>
  count(Purpose, Gender) |>
  group_by(Gender) |>
  mutate(prop = n / sum(n)) |>
  select(-n) |>
  pivot_wider(names_from = Purpose, values_from = prop) |>
  mutate(type = "Categorical")

get_table_pred_transp <- function(scores, transp_scores) {
  # Average predicted scores for each purpose level by gender
  mean_scores_by_gender <-
    german.credit |>
    select(Purpose, Gender) |>
    bind_cols(scores) |>
    group_by(Gender) |>
    summarise(across(colnames(!!scores), ~mean(.x))) |>
    mutate(type = "Composition")

  # Average predicted transported score of women for each purpose level
  mean_transp_scores_women <- colMeans(transp_scores) |>
    as_tibble_row() |>
    mutate(type = "Transported", Gender = "Female -> Male")

  mean_scores_by_gender |>
    bind_rows(mean_transp_scores_women)
}

tb_pred_transp_mean <-
  prop_purposes |> mutate(model = "obs") |>
  bind_rows(
    get_table_pred_transp(scores_glm_1, transp_glm_1) |> mutate(model = "glm_1")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_glm_2, transp_glm_2) |> mutate(model = "glm_2")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_rf, transp_rf) |> mutate(model = "rf")
  ) |>
  bind_rows(
    get_table_pred_transp(scores_gbm, transp_gbm) |> mutate(model = "gbm")
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
  relocate(model, .before = Gender) |>
  relocate(type, .after = model)

tb_pred_transp_mean |> select(-model) |>
  kableExtra::kbl(
    booktabs = TRUE, digits = 4,
  ) |>
  kableExtra::kable_paper() |>
  kableExtra::add_header_above(c(" " = 2, "Purposes" = 3)) |>
  kableExtra::pack_rows(index = table(tb_pred_transp_mean$model))


## Visualization of Transported Categories----

# Get interpolated displacement
?interpolated

transp_glm_1_path <- interpolated(transp_glm_1) |>
  list_rbind(names_to = "id_obs")
transp_glm_2_path <- interpolated(transp_glm_2) |>
  list_rbind(names_to = "id_obs")
transp_rf_path <- interpolated(transp_rf) |>
  list_rbind(names_to = "id_obs")
transp_gbm_path <- interpolated(transp_gbm) |>
  list_rbind(names_to = "id_obs")

# Format scores
scores_all <- as_tibble(scores_glm_1) |>
  mutate(Gender = german.credit$Gender, model = "glm_1") |>
  bind_rows(
    as_tibble(scores_glm_2) |>
      mutate(Gender = german.credit$Gender, model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(as.data.frame(scores_rf)) |>
      mutate(Gender = german.credit$Gender, model = "rf")
  ) |>
  bind_rows(
    as_tibble(scores_gbm) |>
      mutate(Gender = german.credit$Gender, model = "gbm")
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

# Format interpolated displacement
transp_all_path <-
  transp_glm_1_path |>
  mutate(
    Gender = factor("Female", levels = levels(german.credit$Gender)),
    model = "glm_1"
  ) |>
  bind_rows(
    transp_glm_2_path |>
      mutate(
        Gender = factor("Female", levels = levels(german.credit$Gender)),
        model = "glm_2"
      )
  ) |>
  bind_rows(
    transp_rf_path |>
      mutate(
        Gender = factor("Female", levels = levels(german.credit$Gender)),
        model = "rf"
      )
  ) |>
  bind_rows(
    transp_gbm_path |>
      mutate(
        Gender = factor("Female", levels = levels(german.credit$Gender)),
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

# Optimal Transport using clr transform. Points in red are compositions for
# women, whereas points in blue are compositions for men. The lines indicate
# the displacement interpolation when generating counterfactuals.
ggtern(
  data = scores_all,
  mapping = aes(x = cars, y = other, z = equipment, colour = Gender)
) +
  geom_point(size = .1) +
  geom_line(
    data = transp_all_path,
    linewidth = .2, alpha = .8,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Cars", y = "Other", z = "Equip.") +
  theme_paper() +
  theme(
    tern.axis.title = element_text(size = rel(.7))
  )

# 4. Second Method (Optimal Transport in the Simplex)----

# We use our function (this takes about 2min):
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

# Let us focus on the 6th individual
i <- 6

# Composition for this individual (representation of the categorical data in
# the unit simplex)
indiv_i_glm_1 <- X0_glm_1[i, ]
indiv_i_glm_2 <- X0_glm_2[i, ]
indiv_i_rf <- X0_rf[i, ]
indiv_i_gbm <- X0_gbm[i, ]

# The estimated weights
weights_i_glm_1 <- M0_glm_1[i, ]
weights_i_glm_2 <- M0_glm_2[i, ]
weights_i_rf <- M0_rf[i, ]
weights_i_gbm <- M0_gbm[i, ]

# Transported values in the simplex:
# Weighted average of the characteristics
# i.e., counterfactuals
cfact_i_glm_1 <- weights_i_glm_1 %*% X1_glm_1
cfact_i_glm_2 <- weights_i_glm_2 %*% X1_glm_2
cfact_i_rf <- weights_i_rf %*% X1_rf
cfact_i_gbm <- weights_i_gbm %*% X1_gbm

## Visualiation----

# Women
tb_plot_females <-
  as_tibble(X0_glm_1) |>
  mutate(Gender = "Female", model = "glm_1") |>
  bind_rows(
    as_tibble(X0_glm_2) |>
      mutate(Gender = "Female", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(X0_rf) |>
      mutate(Gender = "Female", model = "rf")
  ) |>
  bind_rows(
    as_tibble(X0_gbm) |>
      mutate(Gender = "Female", model = "gbm",)
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
    Gender = "Male", model = "glm_1", weights_i = weights_i_glm_1
  ) |>
  bind_rows(
    as_tibble(X1_glm_2) |>
      mutate(
        Gender = "Male", model = "glm_2", weights_i = weights_i_glm_2
      )
  ) |>
  bind_rows(
    as_tibble(X1_rf) |>
      mutate(
        Gender = "Male", model = "rf", weights_i = weights_i_rf
      )
  ) |>
  bind_rows(
    as_tibble(X1_gbm) |>
      mutate(
        Gender = "Male", model = "gbm", weights_i = weights_i_rf
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
  as_tibble_row(indiv_i_glm_1) |> mutate(Gender = "Female", model = "glm_1") |>
  bind_rows(
    as_tibble_row(indiv_i_glm_2) |> mutate(Gender = "Female", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble_row(indiv_i_rf) |> mutate(Gender = "Female", model = "rf")
  ) |>
  bind_rows(
    as_tibble_row(indiv_i_gbm) |> mutate(Gender = "Female", model = "gbm")
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
  as_tibble(cfact_i_glm_1) |> mutate(Gender = "Male", model = "glm_1") |>
  bind_rows(
    as_tibble(cfact_i_glm_2) |> mutate(Gender = "Male", model = "glm_2")
  ) |>
  bind_rows(
    as_tibble(cfact_i_rf) |> mutate(Gender = "Male", model = "rf")
  ) |>
  bind_rows(
    as_tibble(cfact_i_gbm) |> mutate(Gender = "Male", model = "gbm")
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

# Empirical matching of a woman (big red dot) with men (blue dots).
# The Size of blue dots are proportional to the weights.
# The counterfactual obtained with matching is shown as a blue square.
ggtern(
  mapping = aes(x = cars, y = other, z = equipment, colour = Gender)
) +
  geom_point(
    data = tb_plot_females,
    size = .1,
    alpha = .6
  ) +
  geom_point(
    data = tb_plot_males |>
      group_by(model) |>
      mutate(high = weights_i > quantile(weights_i, probs = .995)),
    mapping = aes(size = weights_i, alpha = high),
  ) +
  geom_point(data = indiv_i, size = 3, colour = "white") +
  geom_point(data = indiv_i, size = 2) +
  geom_point(data = cfact_indiv_i, size = 3, colour = "white", shape = 15) +
  geom_point(data = cfact_indiv_i, size = 2, shape = 15) +
  facet_wrap(~model) +
  labs(x = "Cars", y = "Other", z = "Equip.") +
  scale_colour_manual(
    values = c("Female" = "red", "Male" = "blue"), guide = "none"
  ) +
  scale_size_continuous(range = c(0, 2), guide = "none") +
  scale_alpha_discrete(guide = "none") +
  theme_paper()

## Closer look at the individual----
german.credit[ind_0[i], "Purpose"]
X0_glm_1[i, ]

# Closest points:
ind_close_points <- tail(order(weights_i_glm_1), 3)
X1_glm_1[ind_close_points, ] |>
  as_tibble() |>
  mutate(
    Purpose = german.credit[ind_1[ind_close_points], ] |> select(Purpose),
    index = ind_close_points,
    weights_i = weights_i_glm_1[ind_close_points]) |>
  arrange(desc(weights_i)) |>
  kableExtra::kbl(booktabs = TRUE) |>
  kableExtra::kable_paper()

# Counterfactual using GAM-MLR(1):
cfact_i_glm_1
# The counterfactual category would be:
colnames(cfact_i_glm_1)[which.max(cfact_i_glm_1)]

# Comparison with Gaussian OT (first method):
transp_glm_1 |> slice(i)
