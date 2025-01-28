# Agathe Fernandes Machado
# Arthur Charpentier
# Ewen Gallic
# 2025
# https://arxiv.org/abs/2501.15549

# Objective: provide a methodology to build counterfactuals for categorical data.

# First method:
# Gaussian optimal transport based on an alternative representation of the
# probability vector (in the Euclidean space R^{d-1}).

# Second method:
# Transport and matching directly within the simplex using an appropriate
# cost function.

library(ggtern)
library(tidyverse)
library(compositions)
library(expm)
library(devtools)
load_all("../")

# Load data
load("../data/toydataset.RData")
str(toydataset)

# Two groups: 0 (red) and 1 (blue)
col_groups <- c("0" = "red", "1" = "blue")
toydataset <- as_tibble(toydataset) |>
  mutate(
    group = factor(group),
    colour = factor(group, levels = c(1, 0), labels = c("blue", "red"))
  )
toydataset

# Ternary plot
ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point() +
  scale_colour_manual(values = col_groups)

# The average values in each group for the components:
toydataset |>
  group_by(group, colour) |>
  summarise(across(c("A", "B", "C"), ~100*mean(.x))) |>
  arrange(colour)

# 1. First Method (Gaussian Mapping in the Euclidean Representation)----

X0 <- toydataset |> filter(group == 0) |> select(A, B, C)
X1 <- toydataset |> filter(group == 1) |> select(A, B, C)

# Centered log ratio (clr) transform of each group (h(x_{0,i}))
Z0 <- matrix(compositions::clr(X0), ncol = 3)
Z1 <- matrix(compositions::clr(X1), ncol = 3)
# Keeping first two characteristics
Z0 <- Z0[, 1:2]
Z1 <- Z1[, 1:2]

# Gaussian Optimal Transport:
# We need to compute the average and the variance in each group
m0 <- apply(Z0, 2, mean)
m1 <- apply(Z1, 2, mean)
S0 <- var(Z0)
S1 <- var(Z1)
A <- solve(sqrtm(S0)) %*% sqrtm(sqrtm(S0) %*% S1 %*% (sqrtm(S0))) %*%
  solve(sqrtm(S0))
# Transported values: h^{-1}(m_1 + A(h(x_0) - m_0))
# for the first observation, i.e. X0[1,]
X0[1,]
(z <- Z0[1, ]) # clr
transp_z <- as.numeric(m1 + A %*% (z - m0)) # transported value
transp_z <- clrInv(c(transp_z, -sum(transp_z))) # back in the unit simplex

## McCann's displacement interpolation----
nb <- 31 # no desired points for the interpolation
vec_t <- seq(0, 1, lengt = nb)
vec_t

transp_x <- matrix(NA, nb, 3)
for(i in 1:nb) {
  t <- vec_t[i]
  transp_z <- (1 - t) * z + t * (m1 + A %*% (z - m0))
  transp_z = as.numeric(transp_z)
  transp_z = c(transp_z, -sum(transp_z))
  transp_x[i,] = clrInv(transp_z)
}
head(transp_x)

?transport_x_clr
transport_x_clr(x = X0[1,], n_interp = 5, A = A, m0 = m0, m1 = m1)

# Counterfactual using the clr transformation:
?transport_simplex
# From group 0 to group 1:
transp_val_clr_0_1 <- transport_simplex(X0 = X0, X1 = X1, n_interp = 31)
# From group 1 to group 0:
transp_val_clr_1_0 <- transport_simplex(X0 = X1, X1 = X0, n_interp = 31)

## clr Transform----
# Add group
transp_val_clr_inter_0_1 <-
  interpolated(transp_val_clr_0_1) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 0) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_clr_inter_1_0 <-
  interpolated(transp_val_clr_1_0) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 1) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_clr_inter_both <-
  transp_val_clr_inter_0_1 |> mutate(type = "from 0 to 1") |>
  bind_rows(
    transp_val_clr_inter_1_0 |> mutate(type = "from 1 to 0")
  )

ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point() +
  geom_line(
    data = transp_val_clr_inter_both, linewidth = .1,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = col_groups) +
  facet_wrap(~type) +
  labs(
    title = str_c(
      "Counterfactuals using the clr transformation and\n Gaussian ",
      "optimal transports"
    )
  )

## alr Transform----
transp_val_alr_0_1 <- transport_simplex(X0 = X0, X1 = X1, n_interp = 31, isomorphism = "alr")
transp_val_alr_1_0 <- transport_simplex(X0 = X1, X1 = X0, n_interp = 31, isomorphism = "alr")

# Add group
transp_val_alr_inter_0_1 <-
  interpolated(transp_val_alr_0_1) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 0) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_alr_inter_1_0 <-
  interpolated(transp_val_alr_1_0) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 1) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_alr_inter_both <-
  transp_val_alr_inter_0_1 |> mutate(type = "from 0 to 1") |>
  bind_rows(
    transp_val_alr_inter_1_0 |> mutate(type = "from 1 to 0")
  )

ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point() +
  geom_line(
    data = transp_val_alr_inter_both, linewidth = .1,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = col_groups) +
  facet_wrap(~type) +
  labs(
    title = str_c(
      "Counterfactuals using the alr transformation and\n Gaussian ",
      "optimal transports"
    )
  )

## ilr Transform----
transp_val_ilr_0_1 <- transport_simplex(X0 = X0, X1 = X1, n_interp = 31, isomorphism = "ilr")
transp_val_ilr_0_1 <- transport_simplex(X0 = X1, X1 = X0, n_interp = 31, isomorphism = "ilr")

# Add group
transp_val_ilr_inter_0_1 <-
  interpolated(transp_val_ilr_0_1) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 0) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_ilr_inter_1_0 <-
  interpolated(transp_val_ilr_0_1) |> list_rbind(names_to = "id_obs") |>
  left_join(
    toydataset |> filter(group == 1) |> mutate(id_obs = row_number()) |>
      select(id_obs, group, colour),
    by = "id_obs"
  )

transp_val_ilr_inter_both <-
  transp_val_ilr_inter_0_1 |> mutate(type = "from 0 to 1") |>
  bind_rows(
    transp_val_ilr_inter_1_0 |> mutate(type = "from 1 to 0")
  )

ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point() +
  geom_line(
    data = transp_val_ilr_inter_both, linewidth = .1,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = col_groups) +
  facet_wrap(~type) +
  labs(
    title = str_c(
      "Counterfactuals using the ilr transformation and\n Gaussian ",
      "optimal transports"
    )
  )

# average values of the three components of x and T^*(x)
toydataset |>
  mutate(type = "initial") |>
  rename(init_group = group, init_colour = colour) |>
  bind_rows(
    transp_val_clr_0_1 |>
      mutate(
        init_group = factor(0, levels = c(0,1)),
        init_colour = "red", type = "Transported: clr_0_1")
  ) |>
  bind_rows(
    transp_val_clr_1_0 |>
      mutate(
        init_group = factor(1, levels = c(0,1)),
        init_colour = "blue", type = "Transported: clr_1_0")
  ) |>
  mutate(
    type = factor(
      type, levels = c("initial", "Transported: clr_0_1",
                       "Transported: clr_1_0")
    )
  ) |>
  group_by(type, init_group, init_colour) |>
  summarise(across(c("A", "B", "C"), ~100*mean(.x))) |>
  arrange(type)

## OT in R^2: viz----
transported_z_0 <-
  apply(Z0, MARGIN = 1, function(z) as.numeric(m1 + A %*% (z - m0))) |>
  t() |>
  as_tibble()

colnames(transported_z_0) <- c("V1_t", "V2_t")

transported_z_1 <-
  apply(Z1, MARGIN = 1, function(z) as.numeric(m0 + A %*% (z - m1))) |>
  t() |>
  as_tibble()
colnames(transported_z_1) <- c("V1_t", "V2_t")

map_both <- bind_cols(transported_z_0, as_tibble(Z0)) |>
  mutate(
    group = factor(0, levels = c(0, 1)),
    type = "group 0 to 1"
  ) |>
  bind_rows(
    bind_cols(transported_z_1, as_tibble(Z1)) |>
      mutate(
        group = factor(1, levels = c(0, 1)),
        type = "group 1 to 0")
  )


ggplot(
  data = as_tibble(Z0) |> mutate(group = 0) |>
    bind_rows(
      as_tibble(Z1) |> mutate(group = 1)
    ) |>
    mutate(group = factor(group)),
  mapping = aes(x = V1, y = V2, colour = group)
) +
  geom_point() +
  geom_segment(
    data = map_both,
    mapping = aes(xend = V1_t, yend = V2_t)
  ) +
  scale_colour_manual(values = col_groups) +
  coord_equal(xlim = c(-1.3, 1.3)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~type) +
  stat_ellipse()

# 2. Second Method (OT directly in the unit simplex)----

?wasserstein_simplex

tP0 <- as.matrix(X0)
tP1 <- as.matrix(X1)
# No obs and characteristics in each group:
rbind(dim(tP0), dim(tP1))

# Wasserstein distance and transport plan from group 0 to group 1:
W_xy <- wasserstein_simplex(tP0, tP1)
# Transport plan:
dim(W_xy$plan)
apply(W_xy$plan, 1, sum) # uniform weights
1/nrow(tP0)

apply(W_xy$plan, 2, sum)
1/nrow(tP1)

# Scaling the transport plan to the original number of observations:
M0 <- W_xy$plan * nrow(X0)
# Weights P_i^\star
# for the first individual:
i <- 1
M0[i, ]
# 17th individual with a large weight:
which(M0[i, ]>.1)

# Counterfactual obtained with a weighted average:
weights_i <- M0[i,]
counterfactual_i <- weights_i %*% tP1
counterfactual_i

# Comparison with the counterfactual obtained with Gaussian OT:
gaussian_t_obs_i <- transp_val_clr_0_1[i, ]
gaussian_t_obs_i

# Let us visualize this on a ternary plot

# We can use the interpolated displacement obtained with Gaussian OT
gaussian_t_obs_i_interp <- interpolated(transp_val_clr_0_1)[[i]]

p_1 <- ggtern(
  data = toydataset |> filter(group == 0),
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point(size = .2) +
  # Focus on the first obs in group 0
  geom_point(
    data = toydataset |> filter(group == 0) |> slice(!!i),
    size = 3
  ) +
  # Display observations in group 1 where size is function of the
  # obtained weight
  geom_point(
    data = toydataset |> filter(group == 1) |>
      mutate(weight = M0[i,]),
    mapping = aes(size = weight),
    alpha = .5
  ) +
  # Display the counterfactual
  geom_point(
    data = gaussian_t_obs_i |>
      mutate(
        group = factor(1, levels = c(0, 1)),
        colour = factor(col_groups[["1"]], levels = levels(toydataset$colour))
      ),
    size = 2,
    shape = 15
  ) +
  # Display the interpolation path obtained with Gaussian OT
  geom_line(
    data = gaussian_t_obs_i_interp |>
      mutate(
        group = factor(0, levels = c(0, 1)),
        colour = factor(col_groups[["0"]], levels = levels(toydataset$colour))
      ),
    linewidth = 1
  ) +
  scale_colour_manual(values = col_groups, guide = "none") +
  scale_size_continuous(range = c(0, 3), guide = "none")

p_1

# Counterfactuals for all observations from group 0:
?counterfactual_w
counterfactuals <- counterfactual_w(mapping = W_xy, X0 = X0, X1 = X1)
head(counterfactuals)
