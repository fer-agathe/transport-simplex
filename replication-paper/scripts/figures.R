# remotes::install_github(repo = "fer-agathe/transport-simplex")
set.seed(123)

library(transportsimplex)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(glue)
library(compositions)
library(expm)
library(ggplot2)
library(ggtern)
library(gridExtra)
library(grid)
library(gtable)

library(extrafont)
# font_import()        # only once; may take a while
# https://ctan.org/topic/font-cm
# or, with macos, we used:
# brew install --cask font-computer-modern
# brew install --cask font-new-computer-modern
loadfonts(device = "pdf")
font_size <- 20
font_factor_table <- .8
font_family <- "CMU Serif"

path <- "../figs/"
if (!dir.exists("figs/")) dir.create(path)

# First three columns: probabilities of being of class A, B, or C.
# Last column: group (0 or 1)
data(toydataset)
X0 <- toydataset[toydataset$group == 0, c("A", "B", "C")]
X1 <- toydataset[toydataset$group == 1, c("A", "B", "C")]

# Figure 1----

ternary_1 <- ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = factor(group))
) +
  geom_point(size = .5) +
  scale_colour_manual(values = c("0" = "red", "1" = "blue")) +
  guides(colour = "none") +
  theme_light() +
  theme(
    tern.axis.arrow.show = TRUE
  ) +
  theme_hidetitles()

table_values <- toydataset |>
  group_by(group) |>
  summarise(
    across(
      c("A", "B", "C"),
      ~ str_c(scales::number(100 * mean(.x), accuracy = .01), "%")
    )
  ) |>
  arrange(desc(group))

library(gridExtra)
library(grid)
library(gtable)

table_grob <- tableGrob(
  table_values |> select(-group),
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(
        hjust = 0.5, fontsize = font_size*font_factor_table,
        fontfamily = font_family
      ),
      padding = unit(c(2, 2), "mm")  # vertical, horizontal padding
    ),
    colhead = list(
      fg_params = list(
        fontface = "plain", fontsize = font_size*font_factor_table,
        fontfamily = font_family
      ),
      padding = unit(c(2, 2), "mm")
    )
  )
)

# Add colored bullets manually
blue_bullet <- pointsGrob(
  x = unit(0.5, "npc"), y = unit(0.5, "npc"),
  pch = 16, size = unit(2, "mm"),
  gp = gpar(col = "blue")
)

red_bullet  <- pointsGrob(
  x = unit(0.5, "npc"), y = unit(0.5, "npc"),
  pch = 16, size = unit(2, "mm"),
  gp = gpar(col = "red")
)


table_grob <- gtable::gtable_add_cols(table_grob, unit(1.5, "lines"), pos = 0)
table_grob <- gtable::gtable_add_grob(table_grob, list(blue_bullet, red_bullet), t = 2:3, l = 1)

nr <- nrow(table_grob)
nc <- ncol(table_grob)

# Horizontal lines
hline_header <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
hline_top <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 1))
hline_bottom <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
# Vertical lines
vline_left <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(0, "npc"), gp = gpar(lwd = 1))
vline_right <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_bullet_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))

table_grob <- table_grob |>
  gtable_add_grob(hline_header, t = 1, b = 1, l = 1, r = nc, name = "hline_header") |>
  gtable_add_grob(hline_top, t = 1, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(hline_bottom, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 1, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 1, b = nr, l = nc, r = nc, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 1, b = nr, l = 1, r = 1, name = "vline_bullet_sep")

# Shift upward the table
table_panel <- cowplot::ggdraw() + cowplot::draw_grob(table_grob, y = 0.5)

ternary_plot <- cowplot::plot_grid(
  ggplotGrob(
    ternary_1 +
      # Remove top/bottom margin
      theme(
        plot.margin = ggplot2::margin(t = -5, r = 2, b = 0, l = 2),
        text = element_text(family = "CMU Serif", size = unit(font_size, "pt")),
        axis.title = element_text(size = rel(.8))
      )
  ),
  # table_grob,
  table_panel,
  ncol = 1,
  rel_heights = c(2, .25),
  axis = "tblr",
  align = "v"
)
ternary_plot

filename <- "ternary1"
ggsave(ternary_plot, file = str_c(path, filename, ".pdf"), height = 2.8*1.75, width = 2.4*1.75)
# Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))


# Figure 2----

## Top panel----

col_groups <- c("0" = "red", "1" = "blue")
toydataset <- as_tibble(toydataset) |>
  mutate(
    group = factor(group),
    colour = factor(group, levels = c(1, 0), labels = c("blue", "red"))
  )
toydataset

# Counterfactuals using the clr transformation and Gaussian optimal transports
transp_val_clr_0_1 <- transport_simplex(X0 = X0, X1 = X1, n_interp = 31)
transp_val_clr_1_0 <- transport_simplex(X0 = X1, X1 = X0, n_interp = 31)

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
  transp_val_clr_inter_0_1 |>
  mutate(
    type = glue(
      "from <span style='color:{col_groups[['0']]};'>**0**</span> to ",
      "<span style='color:{col_groups[['1']]};'>**1**</span>"
    )
  ) |>
  bind_rows(
    transp_val_clr_inter_1_0 |>
      mutate(
        type = glue(
          "from <span style='color:{col_groups[['1']]};'>**1**</span> to ",
          "<span style='color:{col_groups[['0']]};'>**0**</span>"
        )
      )
  ) |>
  mutate(
    type = factor(type, levels = unique(type)[order(str_detect(type, "from.*0.*to.*1"))])
  )

library(ggtext)

ternary_2 <- ggtern(
  data = toydataset,
  mapping = aes(x = A, y = C, z = B, colour = group)
) +
  geom_point(size = .5) +
  geom_line(
    data = transp_val_clr_inter_both, linewidth = .05,
    mapping = aes(group = id_obs),
    alpha = .7
  ) +
  scale_colour_manual(values = col_groups) +
  facet_wrap(~type) +
  guides(colour = "none") +
  theme_light() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    strip.text.x = element_text(colour = "black"),
    strip.text = element_markdown(),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    axis.title = element_text(size = rel(.8)),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05
  ) +
  theme_hidetitles()
# ternary_2

## Middle panel----
#
# table_fig <-
#   toydataset |>
#   mutate(type = "initial") |>
#   rename(init_group = group, init_colour = colour) |>
#   bind_rows(
#     transp_val_clr_0_1 |>
#       mutate(
#         init_group = factor(0, levels = c(0,1)),
#         init_colour = "red", type = "Transported: clr_0_1")
#   ) |>
#   bind_rows(
#     transp_val_clr_1_0 |>
#       mutate(
#         init_group = factor(1, levels = c(0,1)),
#         init_colour = "blue", type = "Transported: clr_1_0")
#   ) |>
#   mutate(
#     type = factor(
#       type, levels = c("initial", "Transported: clr_0_1",
#                        "Transported: clr_1_0")
#     )
#   ) |>
#   group_by(type, init_group, init_colour) |>
#   summarise(
#     across(
#       c("A", "B", "C"),
#       ~ str_c(scales::number(100 * mean(.x), accuracy = .01), "%")
#     )
#   ) |>
#   arrange(type) |>
#   ungroup()
#
# table_01 <-
#   table_fig |>
#   filter(type %in% c("initial", "Transported: clr_0_1")) |>
#   select(A, B, C)
#
# table_10 <-
#   table_fig |>
#   filter(type %in% c("initial", "Transported: clr_1_0")) |>
#   select(A, B, C)
#
# # Label helper
# label_grob <- function(bullet_col, text_parts) {
#   grobTree(
#     pointsGrob(
#       x = unit(0.35, "npc"), y = unit(0.5, "npc"),
#       pch = 16, size = unit(2, "mm"), gp = gpar(col = bullet_col)
#     ),
#     do.call(textGrob, c(list(label = text_parts[1], x = unit(0.45, "npc")), list(gp = gpar(fontsize = 9)))),
#     do.call(textGrob, c(list(label = text_parts[2], x = unit(0.55, "npc")), list(gp = gpar(col = bullet_col, fontsize = 9)))),
#     do.call(textGrob, c(list(label = text_parts[3], x = unit(0.65, "npc")), list(gp = gpar(fontsize = 9))))
#   )
# }
#
# # Labels
# labels_left <- list(
#   label_grob("red",  c("(", "0", ")")),
#   label_grob("blue", c("(", "1", ")")),
#   grobTree( # T(red bullet)
#     textGrob("T(", x = unit(0.3, "npc"), y = unit(0.5, "npc"),
#              just = "left", gp = gpar(fontsize = 9)),
#     pointsGrob(x = unit(0.56, "npc"), y = unit(0.4, "npc"),
#                pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
#     textGrob(")", x = unit(0.62, "npc"), y = unit(0.5, "npc"),
#              just = "left", gp = gpar(fontsize = 9))
#   )
# )
#
# labels_right <- list(
#   label_grob("red",  c("(", "0", ")")),
#   label_grob("blue", c("(", "1", ")")),
#   grobTree( # T(blue bullet)
#     textGrob("T(", x = unit(0.3, "npc"), y = unit(0.5, "npc"),
#              just = "left", gp = gpar(fontsize = 9)),
#     pointsGrob(x = unit(0.56, "npc"), y = unit(0.4, "npc"),
#                pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
#     textGrob(")", x = unit(0.62, "npc"), y = unit(0.5, "npc"),
#              just = "left", gp = gpar(fontsize = 9))
#   )
# )
#
# # Creation of a single table
# build_table <- function(df, labels) {
#   tg <- tableGrob(
#     df, rows = NULL,
#     theme = ttheme_minimal(
#       core = list(fg_params = list(hjust = 0.5, fontsize = 9),
#                   padding = unit(c(2, 2), "mm")),
#       colhead = list(fg_params = list(fontsize = 9, fontface = "plain"))
#     )
#   )
#
#   # Label column on the left
#   tg <- gtable_add_cols(tg, unit(15, "mm"), pos = 0)
#   for (i in seq_along(labels)) {
#     tg <- gtable_add_grob(tg, labels[[i]], t = i + 1, l = 1)
#   }
#   nr <- nrow(tg); nc <- ncol(tg)
#   # horizontal lines
#   top_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 1))
#   header_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
#   bottom_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
#   # vertical lines
#   vline_left <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(0, "npc"), x1 = unit(0, "npc"), gp = gpar(lwd = 1))
#   vline_right <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
#   vline_bullet_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
#
#   gtable_add_grob(
#     tg, grobs = top_line, t = 1, b = 1, l = 1, r = nc, name = "hline_header"
#   ) |>
#     gtable_add_grob(header_line, t = 1, b = 1, l = 1, r = nc, name = "hline_top") |>
#     gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
#     gtable_add_grob(vline_left, t = 1, b = nr, l = 1, r = 1, name = "vline_left") |>
#     gtable_add_grob(vline_right, t = 1, b = nr, l = nc, r = nc,, name = "vline_right") |>
#     gtable_add_grob(vline_bullet_sep, t = 1, b = nr, l = 1, r = 1, name = "vline_bullet_sep")
#
# }
#
# ## Build both tables
# left_table  <- build_table(table_01, labels_left)
# right_table <- build_table(table_10, labels_right)

## Middle panel (bis)----

table_fig <-
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
  summarise(
    across(
      c("A", "B", "C"),
      ~ str_c(scales::number(100 * mean(.x), accuracy = .01), "%")
    )
  ) |>
  arrange(type) |>
  ungroup()


# Label helper
label_grob <- function(bullet_col, text_parts) {
  grobTree(
    pointsGrob(
      x = unit(0.25, "npc"), y = unit(0.45, "npc"),
      pch = 16, size = unit(2, "mm"), gp = gpar(col = bullet_col)
    ),
    do.call(textGrob, c(list(label = text_parts[1], x = unit(0.45, "npc")), list(gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family)))),
    do.call(textGrob, c(list(label = text_parts[2], x = unit(0.55, "npc")), list(gp = gpar(col = bullet_col, fontsize = font_size*font_factor_table, fontfamily = font_family)))),
    do.call(textGrob, c(list(label = text_parts[3], x = unit(0.65, "npc")), list(gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family))))
  )
}

# Labels
labels_left <- list(
  label_grob("red",  c("( ", "0", " )")),
  label_grob("blue", c("( ", "1", " )")),
  grobTree( # T(red bullet)
    textGrob("T(", x = unit(0.225, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.7, "npc"), y = unit(0.4, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob(")", x = unit(0.75, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # T(blue bullet)
    textGrob("T(", x = unit(0.225, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.7, "npc"), y = unit(0.4, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob(")", x = unit(0.75, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)

tg <- table_fig |>
  select(A, B, C) |>
  tableGrob(
    rows = NULL,
    theme = ttheme_minimal(
      core = list(
        fg_params = list(hjust = 0.5, fontsize = font_size*font_factor_table, fontfamily = font_family),
        padding = unit(c(3, 1.5), "mm")),
      colhead = list(
        fg_params = list(
          fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family
        )
      )
    )
  )

# Label column on the left
tg <- gtable_add_cols(tg, unit(15, "mm"), pos = 0)
for (i in seq_along(labels_left)) {
  tg <- gtable_add_grob(tg, labels_left[[i]], t = i + 1, l = 1)
}
nr <- nrow(tg); nc <- ncol(tg)
# horizontal lines
top_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 1))
header_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
bottom_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
# vertical lines
vline_left <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(0, "npc"), x1 = unit(0, "npc"), gp = gpar(lwd = 1))
vline_right <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_bullet_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))

middle_table <- gtable_add_grob(
  tg, grobs = top_line, t = 1, b = 1, l = 1, r = nc, name = "hline_header"
) |>
  gtable_add_grob(header_line, t = 1, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 1, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 1, b = nr, l = nc, r = nc,, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 1, b = nr, l = 1, r = 1, name = "vline_bullet_sep")

plot(middle_table)


## Bottom Panel----

Z0 <- matrix(clr(X0), ncol = 3)
Z1 <- matrix(clr(X1), ncol = 3)
Z0 <- Z0[, 1:2]
Z1 <- Z1[, 1:2]
m0 <- apply(Z0, 2, mean)
m1 <- apply(Z1, 2, mean)
S0 <- var(Z0)
S1 <- var(Z1)
A <- solve(sqrtm(S0)) %*%
  sqrtm(sqrtm(S0) %*% S1 %*% (sqrtm(S0))) %*%
  solve(sqrtm(S0))

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


p_euclid <- ggplot(
  data = as_tibble(Z0) |> mutate(group = 0) |>
    bind_rows(
      as_tibble(Z1) |> mutate(group = 1)
    ) |>
    mutate(group = factor(group)),
  mapping = aes(x = V1, y = V2, colour = group)
) +
  geom_point(size = .5) +
  geom_segment(
    data = map_both,
    mapping = aes(xend = V1_t, yend = V2_t),
    alpha = .7, linewidth = 0.05
  ) +
  scale_colour_manual(values = col_groups) +
  coord_equal(xlim = c(-1.2, 1.2)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~type) +
  stat_ellipse(linewidth = 0.5) +
  guides(colour = "none") +
  theme_bw() +
  theme(
    # strip.background = element_rect(colour = "black", fill = NA),
    # strip.text = element_markdown(),
    # strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(family = font_family, size = unit(font_size, "pt"))
  )


## Combine and export----

figure_2 <- cowplot::plot_grid(
  ggplotGrob(
    ternary_2 +
      # Remove top/bottom margin
      theme(plot.margin = margin(t = 0, r = 0, b = -50, l = 0))
  ),
  # cowplot::plot_grid(
  #   left_table, right_table, ncol = 2
  # ),
  middle_table,
  ggplotGrob(
    p_euclid +
      theme(
        plot.margin = margin(t = 5, r = 0, b = 0, l = 0)
        # text = element_text(size = unit(11.5, "pt"))
      )
  ),
  ncol = 1,
  rel_heights = c(2, .75, 1.75),
  axis = "tblr",
  align = "v"
)

plot(figure_2)

filename <- "ternary2"
ggsave(figure_2, file = str_c(path, filename, ".pdf"),
       height = 4.65*1.75, width = 3.5*1.75)
# Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))


# Figure 3----

library(VGAM)
library(gtools)
dir_0 <- vglm(
  cbind(A,B,C)  ~ 1,
  dirichlet,
  data = toydataset |> filter(group == 0),
  trace = TRUE, crit = "coef"
)
dir_1 <- vglm(
  cbind(A,B,C)  ~ 1,
  dirichlet,
  data = toydataset |> filter(group == 1),
  trace = TRUE, crit = "coef"
)
Coef(dir_0)
Coef(dir_1)

u <- (0:100) / 100
u <- u[-c(1, length(u))]
x <- expand.grid(u, u)
x <- cbind(x, w = 1 - rowSums(x))
# keep only valid points inside the simplex
x <- x[rowSums(x >= 0) == 3, ]
# Density for the Dirichlet distribution
z_0 <- ddirichlet(x, alpha = Coef(dir_0))
z_1 <- ddirichlet(x, alpha = Coef(dir_1))
z_0[is.infinite(z_0)] <- NA
z_1[is.infinite(z_1)] <- NA

# Matrix for contouring
mat_0 <- mat_1 <- matrix(NA, length(u), length(u))
for (i in seq_len(nrow(x))) {
  iu <- which.min(abs(u - x[i, 1]))
  iv <- which.min(abs(u - x[i, 2]))
  mat_0[iu, iv] <- z_0[i]
  mat_1[iu, iv] <- z_1[i]
}

# Extract contour lines
CL_0 <- contourLines(u, u, mat_0)
CL_1 <- contourLines(u, u, mat_1)

# Format in a data frame with ternary coordinates
contour_df_0 <- do.call(
  rbind,
  lapply(
    seq_along(CL_0), function(i) {
      L <- CL_0[[i]]
      tibble(
        x = L$x,
        y = L$y,
        z = 1 - L$x - L$y,
        group_obs = i,
        level = L$level
      )
    }
  )
)
contour_df_1 <- do.call(
  rbind,
  lapply(
    seq_along(CL_1), function(i) {
      L <- CL_1[[i]]
      tibble(
        x = L$x,
        y = L$y,
        z = 1 - L$x - L$y,
        group_obs = i,
        level = L$level
      )
    }
  )
)

contour_df <-
  contour_df_0 |> mutate(group = "0") |>
  bind_rows(contour_df_1 |> mutate(group = "1")) |>
  rename(A = x, B = y, C = z)


ternary_dir <- ggtern() +
  # Add points
  geom_point(
    data = toydataset,
    mapping = aes(x = A, y = C, z = B, colour = group),
    size = 0.5
  ) +
  # Add contour lines
  geom_path(
    data = contour_df,
    mapping = aes(x = A, y = C, z = B, group = group_obs, colour = group),
    linewidth = .1
  ) +
  facet_wrap(~ group) +
  scale_colour_manual(values = col_groups) +
  guides(colour = "none") +
  theme_light() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    axis.title = element_text(size = rel(.8)),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

filename <- "ternary_dir"
ggsave(ternary_dir, file = str_c(path, filename, ".pdf"), height = 2*1.75, width = 3.75*1.75)
# Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))

# Figure 4----

tP0 <- as.matrix(X0)
tP1 <- as.matrix(X1)
W_xy <- wasserstein_simplex(tP0, tP1)
M0 <- W_xy$plan * nrow(X0)

## Left----
i <- 1
weights_i <- M0[i,]
counterfactual_i <- weights_i %*% tP1
gaussian_t_obs_i <- transp_val_clr_0_1[i, ]
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
  scale_size_continuous(range = c(0, 3), guide = "none") +
  theme_light() +
  theme(
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

## Right----
i <- 3
weights_i <- M0[i,]
counterfactual_i <- weights_i %*% tP1
gaussian_t_obs_i <- transp_val_clr_0_1[i, ]
gaussian_t_obs_i_interp <- interpolated(transp_val_clr_0_1)[[i]]

p_2 <- ggtern(
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
  scale_size_continuous(range = c(0, 3), guide = "none") +
  theme_light() +
  theme(
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

p_2

## Export----

ternary_coupling <- cowplot::plot_grid(
  ggplotGrob(
    p_1 +
      # Remove top/bottom margin
      theme(
        plot.margin = margin(t = -5, r = 2, b = 0, l = 2),
        text = element_text(family = font_family, size = unit(font_size, "pt")),
        axis.title = element_text(size = rel(.8))
      )
  ),
  ggplotGrob(
    p_2 +
      # Remove top/bottom margin
      theme(
        plot.margin = margin(t = -5, r = 2, b = 0, l = 2),
        text = element_text(family = font_family, size = unit(font_size, "pt")),
        axis.title = element_text(size = rel(.8))
      )
  ),
  ncol = 2
)

filename <- "ternary_coupling"
ggsave(ternary_coupling, file = str_c(path, filename, ".pdf"),
       height = 2*1.75, width = 3.65*1.75)
# Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))


# Figure 5----

# install.packages("fairml")
data(german.credit, package = "fairml")
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

## Estimation of Scores
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
#
# tb_four_indiv |> select(-model) |>
#   kableExtra::kbl(
#     booktabs = TRUE, digits = 4,
#   ) |>
#   kableExtra::kable_paper() |>
#   kableExtra::add_header_above(c(" " = 2, "Predicted Scores" = 3)) |>
#   kableExtra::pack_rows(index = table(tb_four_indiv$model))


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


transp_glm_1 <- transport_simplex(X0 = X0_glm_1, X1 = X1_glm_1, n_interp = 31)
transp_glm_2 <- transport_simplex(X0 = X0_glm_2, X1 = X1_glm_2, n_interp = 31)
transp_rf <- transport_simplex(X0 = X0_rf, X1 = X1_rf, n_interp = 31)
transp_gbm <- transport_simplex(X0 = X0_gbm, X1 = X1_gbm, n_interp = 31)

transp_glm_1_path <- interpolated(transp_glm_1) |>
  list_rbind(names_to = "id_obs")
transp_glm_2_path <- interpolated(transp_glm_2) |>
  list_rbind(names_to = "id_obs")
transp_rf_path <- interpolated(transp_rf) |>
  list_rbind(names_to = "id_obs")
transp_gbm_path <- interpolated(transp_gbm) |>
  list_rbind(names_to = "id_obs")

## Top panel----

### Plots----

scores_top <- as_tibble(scores_glm_1) |>
  mutate(Gender = german.credit$Gender, model = "glm_1") |>
  bind_rows(
    as_tibble(scores_glm_2) |>
      mutate(Gender = german.credit$Gender, model = "glm_2")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)"
      )
    )
  )

transp_top_path <-
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
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)"
      )
    )
  )


ternary_german_top <- ggtern(
  data = scores_top,
  mapping = aes(x = cars, y = other, z = equipment, colour = Gender)
) +
  geom_point(size = .05, alpha = .5) +
  geom_line(
    data = transp_top_path,
    linewidth = .05, alpha = .5,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Cars", y = "Other", z = "Equipment") +
  theme_light() +
  theme(
    strip.background = element_rect(fill = NA, colour = "black"),
    tern.axis.title = element_text(size = rel(.8)),
    strip.text.x = element_text(colour = "black"),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

### Table----

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
    mutate(type = "T", Gender = "Female -> Male")

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
  relocate(type, .after = model) |>
  mutate(
    label = str_c(
      type, " (",
      # ifelse(
      #   Gender == "Female",
      #   yes = "<span style='color:{col_groups[['0']]};'>F</span>",
      #   no  = "<span style='color:{col_groups[['1']]};'>M</span>"
      # ),
      str_sub(Gender, 1),
      ")"
    ),
    type = factor(type, levels = c("Categorical", "Composition", "T"))
  ) |>
  relocate(
    label, .before = model
  )

table_glm1 <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(Gender)) |>
  filter(model %in% c("Observed Values", "GAM-MLR(1)")) |>
  mutate(
    across(
      c("cars", "equipment", "other"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, cars, equipment, other)

table_glm2 <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(Gender)) |>
  filter(model %in% c("Observed Values", "GAM-MLR(2)")) |>
  mutate(
    across(
      c("cars", "equipment", "other"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, cars, equipment, other)


label_grobs <- list(
  grobTree( # categorical, Male
    textGrob("categ.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # categorical, Female
    textGrob("categ.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Male
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Female
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # T(red bullet)
    textGrob("T(", x = unit(0.4, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob(")", x = unit(0.65, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)

# Create the table grob
tg <- tableGrob(
  table_glm1 |> select(-label) |> rename(`eq.` = equipment) |>
    cbind(table_glm2 |> select(-label) |> rename(`eq.` = equipment)),
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(
        hjust = 0.5, fontsize = font_size*font_factor_table, fontfamily = font_family
      ),
      padding = unit(c(3, 2), "mm")),
    colhead = list(
      fg_params = list(
        fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family
      )
    )
  )
)

# Add bullet column
tg <- gtable_add_cols(tg, unit(35, "mm"), pos = 0)
for (i in seq_along(label_grobs)) {
  tg <- gtable_add_grob(tg, label_grobs[[i]], t = i + 1, l = 1)
}
tg <- gtable_add_grob(tg, textGrob(""), t = 1, l = 1)  # header cell

# Header
# Add empty header cell in top-left
tg <- gtable_add_grob(tg, textGrob(""), t = 1, l = 1)

# Add title row that spans columns 2 to end of first model
nc <- ncol(tg)
tg <- gtable_add_rows(tg, heights = unit(5, "mm"), pos = 0)

title_grob_1 <- textGrob(
  "GAM-MLR(1)", gp = gpar(font_size*font_factor_table, fontface = "plain", fontfamily = font_family), just = "center"
)
title_grob_2 <- textGrob(
  "GAM-MLR(2)", gp = gpar(font_size*font_factor_table, fontface = "plain", fontfamily = font_family), just = "center"
)

tg <- gtable_add_grob(
  tg, grobs = title_grob_1, t = 1, b = 1, l = 2, r = 2+3, name = "model_title"
) |>  gtable_add_grob(
  grobs = title_grob_2, t = 1, b = 1, l = 5, r = nc, name = "model_title_2"
)

plot(tg)


nr <- nrow(tg); nc <- ncol(tg)
# horizontal lines
top_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 1))
header_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
delim_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2, lty = "dotted"))
bottom_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
# vertical lines
vline_left <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(0, "npc"), x1 = unit(0, "npc"), gp = gpar(lwd = 1))
vline_right <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_bullet_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_model_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))

table_top <- tg |>
  #   gtable_add_grob(
  #   tg, grobs = top_line, t = 1, b = 1, l = 2, r = nc, name = "hline_header"
  # ) |>
  gtable_add_grob(header_line, t = 1, b = 1, l = 2, r = nc, name = "hline_top") |>
  gtable_add_grob(header_line, t = 2, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(delim_line, t = 4, b = 1, l = 1, r = nc, name = "hline_delim") |>
  gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 3, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 2, b = nr, l = nc, r = nc,, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 2, b = nr, l = 1, r = 1, name = "vline_bullet_sep") |>
  gtable_add_grob(vline_model_sep, t = 2, b = nr, l = 4, r = 1, name = "vline_bullet_sep")


## Bottom panel----

### Plots----

scores_bottom <- as_tibble(as.data.frame(scores_rf)) |>
  mutate(Gender = german.credit$Gender, model = "rf") |>
  bind_rows(
    as_tibble(scores_gbm) |>
      mutate(Gender = german.credit$Gender, model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "Random Forest", "Gradient Boosting Model"
      )
    )
  )

transp_path_bottom <-
  transp_rf_path |>
  mutate(
    Gender = factor("Female", levels = levels(german.credit$Gender)),
    model = "rf"
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
      levels = c("obs", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "Random Forest",  "Gradient Boosting Model"
      )
    )
  )

ternary_german_bottom <- ggtern(
  data = scores_bottom,
  mapping = aes(x = cars, y = other, z = equipment, colour = Gender)
) +
  geom_point(size = .05, alpha = .5) +
  geom_line(
    data = transp_path_bottom,
    linewidth = .05, alpha = .5,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Cars", y = "Other", z = "Equipment") +
  theme_light() +
  theme(
    strip.background = element_rect(fill = NA, colour = "black"),
    tern.axis.title = element_text(size = rel(.8)),
    strip.text.x = element_text(colour = "black"),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

### Tables----
table_rf <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(Gender)) |>
  filter(model %in% c("Random Forest")) |>
  mutate(
    across(
      c("cars", "equipment", "other"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, cars, equipment, other)

table_xgb <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(Gender)) |>
  filter(model %in% c("Gradient Boosting Model")) |>
  mutate(
    across(
      c("cars", "equipment", "other"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, cars, equipment, other)

label_grobs <- list(
  grobTree( # composition, Male
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Female
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # T(red bullet)
    textGrob("T(", x = unit(0.4, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob(")", x = unit(0.65, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)


# Create the table grob
tg_2 <- tableGrob(
  table_rf |> select(-label) |> rename(`eq.` = equipment) |>
    cbind(table_xgb |> select(-label) |> rename(`eq.` = equipment)),
  rows = NULL,
  theme = ttheme_minimal(
    core = list(fg_params = list(hjust = 0.5, fontsize = font_size*font_factor_table, fontfamily = font_family),
                padding = unit(c(3, 2), "mm")),
    colhead = list(fg_params = list(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)


# Add bullet column
tg_2 <- gtable_add_cols(tg_2, unit(35, "mm"), pos = 0)
for (i in seq_along(label_grobs)) {
  tg_2 <- gtable_add_grob(tg_2, label_grobs[[i]], t = i + 1, l = 1)
}
tg_2 <- gtable_add_grob(tg_2, textGrob(""), t = 1, l = 1)  # header cell

# Header
# Add empty header cell in top-left
tg_2 <- gtable_add_grob(tg_2, textGrob(""), t = 1, l = 1)

# Add title row that spans columns 2 to end of first model
nc <- ncol(tg_2)
tg_2 <- gtable_add_rows(tg_2, heights = unit(5, "mm"), pos = 0)

title_grob_1 <- textGrob(
  "random forest", gp = gpar(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family), just = "center"
)
title_grob_2 <- textGrob(
  "boosting", gp = gpar(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family), just = "center"
)

tg_2 <- gtable_add_grob(
  tg_2, grobs = title_grob_1, t = 1, b = 1, l = 2, r = 2+3, name = "model_title"
) |>  gtable_add_grob(
  grobs = title_grob_2, t = 1, b = 1, l = 5, r = nc, name = "model_title_2"
)

plot(tg_2)


nr <- nrow(tg_2); nc <- ncol(tg_2)
table_bottom <- tg_2 |>
  #   gtable_add_grob(
  #   tg_2, grobs = top_line, t = 1, b = 1, l = 2, r = nc, name = "hline_header"
  # ) |>
  gtable_add_grob(header_line, t = 1, b = 1, l = 2, r = nc, name = "hline_top") |>
  gtable_add_grob(header_line, t = 2, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 3, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 2, b = nr, l = nc, r = nc,, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 2, b = nr, l = 1, r = 1, name = "vline_bullet_sep") |>
  gtable_add_grob(vline_model_sep, t = 2, b = nr, l = 4, r = 1, name = "vline_bullet_sep")



## Export----
table_top_trimmed <- gtable::gtable_trim(table_top)
table_bottom_trimmed <- gtable::gtable_trim(table_bottom)
aligned_table_top <- grobTree(
  gtable_trim(table_top),  # trims padding
  vp = viewport(
    y = 1, just = "top",
    height = grobHeight(gtable_trim(table_top))
  )
)


aligned_table_bottom <- grobTree(
  table_bottom_trimmed,
  vp = viewport(y = 1, just = "top", height = grobHeight(table_bottom_trimmed))
)


gg_top_trimmed <- ggplotGrob(
  ternary_german_top + theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 30, l = 0))
)
gg_bottom_trimmed <- ggplotGrob(
  ternary_german_bottom +
    theme(
      plot.margin = ggplot2::margin(t = -30, 0, 0, 0)
    )
)


ternary_german <-
  cowplot::plot_grid(
    gg_top_trimmed,
    aligned_table_top,
    gg_bottom_trimmed,
    aligned_table_bottom,
    ncol = 1,
    rel_heights = c(2.5, 1, 2.4, 1), # fine-tune
    align = "v",
    axis = "tblr"
  )


filename <- "ternary_german"
ggsave(ternary_german, file = str_c(path, filename, ".pdf"), height = 5.5*1.75, width = 3.5*1.75)
# # Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))



# Figure 6----

# mapping_glm_1 <- wasserstein_simplex(X0_glm_1, X1_glm_1)
# mapping_glm_2 <- wasserstein_simplex(X0_glm_2, X1_glm_2)
# mapping_rf <- wasserstein_simplex(X0_rf, X1_rf)
# mapping_gbm <- wasserstein_simplex(X0_gbm, X1_gbm)


load("../output/matching_german.rda")
M0_glm_1 <- mapping_glm_1$plan * nrow(X0_glm_1)
M0_glm_2 <- mapping_glm_2$plan * nrow(X0_glm_2)
M0_rf <- mapping_rf$plan * nrow(X0_rf)
M0_gbm <- mapping_gbm$plan * nrow(X0_gbm)

# Focusing on a specific individual
i_1 <- 34


indiv_i_1_glm_2 <- X0_glm_2[i_1, ]
round(100*indiv_i_1_glm_2, 2)
weights_i_1_glm_2 <- M0_glm_2[i_1, ]
cfact_i_1_glm_2 <- weights_i_1_glm_2 %*% X1_glm_2
ind_match_i_1 <- which(weights_i_1_glm_2 > 0)
# Weights of closest points
round(weights_i_1_glm_2[ind_match_i_1], 3)
# propensity closest points
X1_glm_2[ind_match_i_1,]
german.credit[ind_1[ind_match_i_1], "Purpose"]

round(100 * cfact_i_1_glm_2, 2)

str_c(
  "Woman ", i_1,
  "\n# Purpose:\n\t",
  german.credit[ind_0[i_1], "Purpose"],
  "\n# Pred with MLR(2)\n\t",
  str_c(names(indiv_i_1_glm_2), " = ", round(100*indiv_i_1_glm_2, 2), collapse = ", "),
  "\n# Weights closest points:\n\t",
  str_c(round(weights_i_1_glm_2[ind_match_i_1], 3), collapse = ", "),
  "\n# Values of the neighbors:\n\t",
  str_c(german.credit[ind_1[ind_match_i_1], "Purpose"], collapse = ", "),
  "\n# Counterfactual:\n\t",
  str_c(colnames(cfact_i_1_glm_2), " = ", round(100 * cfact_i_1_glm_2, 2), collapse = ", ")
) |>
  cat()

# And a second one
i_2 <- 83
indiv_i_2_glm_2 <- X0_glm_2[i_2, ]
weights_i_2_glm_2 <- M0_glm_2[i_2, ]
ind_match_i_2 <- which(weights_i_2_glm_2 > 0)
cfact_i_2_glm_2 <- weights_i_2_glm_2 %*% X1_glm_2

str_c(
  "Woman ", i_1,
  "\n# Purpose:\n\t",
  german.credit[ind_0[i_1], "Purpose"],
  "\n# Pred with MLR(2)\n\t",
  str_c(names(indiv_i_1_glm_2), " = ", round(100*indiv_i_1_glm_2, 2), collapse = ", "),
  "\n# Weights closest points:\n\t",
  str_c(round(weights_i_1_glm_2[ind_match_i_1], 3), collapse = ", "),
  "\n# Values of the neighbors:\n\t",
  str_c(german.credit[ind_1[ind_match_i_1], "Purpose"], collapse = ", "),
  "\n# Predicted propensities of the neighbors in group 1:\n\t",
  apply(X1_glm_2[ind_match_i_1,], 1, function(x) str_c(round(x, 3), collapse = ", ")) |>
    str_c(collapse = "\n\t"),
  "\n# Counterfactual:\n\t",
  str_c(colnames(cfact_i_1_glm_2), " = ", round(100 * cfact_i_1_glm_2, 2), collapse = ", ")
) |>
  cat()


# Women
tb_plot_females <-
  as_tibble(X0_glm_2) |>
  mutate(Gender = "Female", model = "glm_2") |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_2"),
      labels = c("Observed Values", "GAM-MLR(2)")
    )
  )

# Males individuals, with a column weights_i giving their weight used to
# construct the counterfactual for indiv i
tb_plot_males <-
  as_tibble(X1_glm_2) |>
  mutate(
    Gender = "Male", model = "glm_2", weights_i = weights_i_1_glm_2,
    indiv = as.character(i_1)
  ) |>
  bind_rows(
    as_tibble(X1_glm_2) |>
      mutate(
        Gender = "Male", model = "glm_2", weights_i = weights_i_2_glm_2,
        indiv = as.character(i_2)
      )
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_2"),
      labels = c("Observed Values", "GAM-MLR(2)")
    ),
    indiv = factor(
      indiv, levels = c(i_1, i_2), labels = c("A first woman", "A second Woman")
    )
  )

indiv_i <-
  as_tibble_row(indiv_i_1_glm_2) |>
  mutate(Gender = "Female", model = "glm_2", indiv = as.character(i_1)) |>
  bind_rows(
    as_tibble_row(indiv_i_2_glm_2) |>
      mutate(Gender = "Female", model = "glm_2", indiv = as.character(i_2))
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_2"),
      labels = c("Observed Values", "GAM-MLR(2)")
    ),
    indiv = factor(
      indiv, levels = c(i_1, i_2), labels = c("A first woman", "A second Woman")
    )
  )

cfact_indiv_i <-
  as_tibble(cfact_i_1_glm_2) |>
  mutate(Gender = "Male", model = "glm_2", indiv = as.character(i_1)) |>
  bind_rows(
    as_tibble(cfact_i_2_glm_2) |>
      mutate(Gender = "Male", model = "glm_2", indiv = as.character(i_2))
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_2"),
      labels = c("Observed Values", "GAM-MLR(2)")
    ),
    indiv = factor(
      indiv, levels = c(i_1, i_2), labels = c("A first woman", "A second Woman")
    )
  )

p_zoom <- ggtern(
  mapping = aes(x = cars, y = other, z = equipment, colour = Gender)
) +
  geom_point(
    data = tb_plot_females,
    size = .01,
    alpha = .6
  ) +
  geom_point(
    data = tb_plot_males |>
      group_by(model, indiv) |>
      mutate(high = weights_i > quantile(weights_i, probs = .995)),
    mapping = aes(size = weights_i, alpha = high),
  ) +
  geom_point(data = indiv_i, size = 3, colour = "white") +
  geom_point(data = indiv_i, size = 2) +
  geom_point(data = cfact_indiv_i, size = 3, colour = "white", shape = 15) +
  geom_point(data = cfact_indiv_i, size = 2, shape = 15) +
  facet_wrap(~indiv) +
  labs(x = "Cars", y = "Other", z = "Equipment") +
  scale_colour_manual(
    values = c("Female" = "red", "Male" = "blue"), guide = "none"
  ) +
  scale_size_continuous(range = c(0, 3), guide = "none") +
  scale_alpha_discrete(guide = "none") +
  theme_light() +
  theme(
    strip.background = element_rect(colour = "black", fill = NA),
    strip.text = element_markdown(),
    strip.text.x = element_text(colour = "black"),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    tern.axis.title = element_text(size = rel(.8)),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

# p_zoom

filename <- "zoom-pt-mlr2"
ggsave(p_zoom, file = str_c(path, filename, ".pdf"), height = 2*1.75, width = 3.55*1.75)
# Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))


# Figure 7----
data(adult, package = "fairml")
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
## Train Models----
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
# model_rf <- randomForest(
#   marital_status ~ ., data = adult |> select(-sex, -income)
# )
load( "../output/model_rf_adult.rda")
# model_gbm <- gbm(
#   marital_status ~.,
#   data = adult |> select(-sex, -income),
#   distribution = "multinomial",
#   cv.folds = 10,
#   shrinkage = .01,
#   n.minobsinnode = 10,
#   n.trees = 2000
# )
load( "../output/model_gbm_adult.rda")

# Extract scores
scores_glm_1 <- predict(model_glm_1, type = "probs")
scores_glm_2 <- predict(model_glm_2, type = "probs")
scores_rf <- predict(model_rf, type = "prob")
scores_gbm <- predict.gbm(
  object = model_gbm,
  newdata = adult |> select(-sex, -income),
  n.trees = 2000,
  type = "response")
scores_gbm <- scores_gbm[ , , 1]

set.seed(1234)
# Focus on less individuals to have figures that remain readable
idx <- sample(1:nrow(adult),size = 400)
adult_subset <- adult[idx, ]

# Isolate women/men
ind_0 <- which(adult_subset$sex == "Female")
ind_1 <- which(adult_subset$sex == "Male")

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

# Overcoming issue with predicted score  equal to 0 or 1
# RF
for(i in 1:3) X0_rf[which(X0_rf[, i] == 0), i] = .0000001
for(i in 1:3) X0_rf[which(X0_rf[, i] == 1), i] = 1-.0000001
for(i in 1:3) X1_rf[which(X1_rf[, i] == 0), i] = .0000001
for(i in 1:3) X1_rf[which(X1_rf[, i] == 1), i] = 1-.0000001
# GBM
for(i in 1:3) X0_gbm[which(X0_gbm[, i] == 0), i] = .0000001
for(i in 1:3) X0_gbm[which(X0_gbm[, i] == 1), i] = 1-.0000001
for(i in 1:3) X1_gbm[which(X1_gbm[, i] == 0), i] = .0000001
for(i in 1:3) X1_gbm[which(X1_gbm[, i] == 1), i] = 1-.0000001


transp_glm_1 <- transport_simplex(X0 = X0_glm_1, X1 = X1_glm_1, n_interp = 31)
transp_glm_2 <- transport_simplex(X0 = X0_glm_2, X1 = X1_glm_2, n_interp = 31)
transp_rf <- transport_simplex(X0 = X0_rf, X1 = X1_rf, n_interp = 31)
transp_gbm <- transport_simplex(X0 = X0_rf, X1 = X1_gbm, n_interp = 31)

transp_glm_1_path <- interpolated(transp_glm_1) |>
  list_rbind(names_to = "id_obs")
transp_glm_2_path <- interpolated(transp_glm_2) |>
  list_rbind(names_to = "id_obs")
transp_rf_path <- interpolated(transp_rf) |>
  list_rbind(names_to = "id_obs")
transp_gbm_path <- interpolated(transp_gbm) |>
  list_rbind(names_to = "id_obs")

## Top Panel----

### Plots----

scores_top <- as_tibble(scores_glm_1[idx, ]) |>
  mutate(sex = adult_subset$sex, model = "glm_1") |>
  bind_rows(
    as_tibble(scores_glm_2[idx, ]) |>
      mutate(sex = adult_subset$sex, model = "glm_2")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)"
      )
    )
  )

transp_path_top <-
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
  mutate(
    model = factor(
      model,
      levels = c("obs", "glm_1", "glm_2"),
      labels = c(
        "Observed Values",
        "GAM-MLR(1)", "GAM-MLR(2)"
      )
    )
  )

ternary_adult_top <- ggtern(
  data = scores_top,
  mapping = aes(x = Married, y = Separated, z = `Never-married`, colour = sex)
) +
  geom_point(size = .05, alpha = .5) +
  geom_line(
    data = transp_path_top,
    linewidth = .05, alpha = .5,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Married", y = "Separated", z = "Never-married") +
  theme_light() +
  theme(
    strip.background = element_rect(fill = NA, colour = "black"),
    tern.axis.title = element_text(size = rel(.8)),
    strip.text.x = element_text(colour = "black"),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

### Tables----

# Proportions of each marital status by gender in the dataset
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
    mutate(type = "T", sex = "Female -> Male")

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
  relocate(type, .after = model) |>
  mutate(
    label = str_c(
      type, " (",
      str_sub(sex, 1),
      ")"
    ),
    type = factor(type, levels = c("Categorical", "Composition", "T"))
  ) |>
  relocate(
    label, .before = model
  )

table_glm1 <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(sex)) |>
  filter(model %in% c("Observed Values", "GAM-MLR(1)")) |>
  mutate(
    across(
      c("Married", "Never-married", "Separated"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, Married, `Never-married`, Separated)

table_glm2 <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(sex)) |>
  filter(model %in% c("Observed Values", "GAM-MLR(2)")) |>
  mutate(
    across(
      c("Married", "Never-married", "Separated"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, Married, `Never-married`, Separated)


label_grobs <- list(
  grobTree( # categorical, Male
    textGrob("categ.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # categorical, Female
    textGrob("categ.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Male
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Female
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # T(red bullet)
    textGrob("T(", x = unit(0.4, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob(")", x = unit(0.65, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)

# Create the table grob
tg <- tableGrob(
  table_glm1 |> select(-label) |>
    rename(M = Married, N = `Never-married`, S = "Separated") |>
    cbind(
      table_glm2 |>
        select(-label) |>
        rename(M = Married, N = `Never-married`, S = "Separated")
    ),
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(
        hjust = 0.5, fontsize = font_size*font_factor_table, fontfamily = font_family
      ),
      padding = unit(c(3, 1.5), "mm")),
    colhead = list(
      fg_params = list(
        fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family
      )
    )
  )
)

# Add bullet column
tg <- gtable_add_cols(tg, unit(35, "mm"), pos = 0)
for (i in seq_along(label_grobs)) {
  tg <- gtable_add_grob(tg, label_grobs[[i]], t = i + 1, l = 1)
}
tg <- gtable_add_grob(tg, textGrob(""), t = 1, l = 1)  # header cell

# Header
# Add empty header cell in top-left
tg <- gtable_add_grob(tg, textGrob(""), t = 1, l = 1)

# Add title row that spans columns 2 to end of first model
nc <- ncol(tg)
tg <- gtable_add_rows(tg, heights = unit(5, "mm"), pos = 0)

title_grob_1 <- textGrob(
  "GAM-MLR(1)", gp = gpar(font_size*font_factor_table, fontface = "plain", fontfamily = font_family), just = "center"
)
title_grob_2 <- textGrob(
  "GAM-MLR(2)", gp = gpar(font_size*font_factor_table, fontface = "plain", fontfamily = font_family), just = "center"
)

tg <- gtable_add_grob(
  tg, grobs = title_grob_1, t = 1, b = 1, l = 2, r = 2+3, name = "model_title"
) |>  gtable_add_grob(
  grobs = title_grob_2, t = 1, b = 1, l = 5, r = nc, name = "model_title_2"
)

plot(tg)


nr <- nrow(tg); nc <- ncol(tg)
# horizontal lines
top_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 1))
header_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
delim_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2, lty = "dotted"))
bottom_line <- segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 1))
# vertical lines
vline_left <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(0, "npc"), x1 = unit(0, "npc"), gp = gpar(lwd = 1))
vline_right <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_bullet_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))
vline_model_sep <- segmentsGrob(y0 = unit(0, "npc"), y1 = unit(1, "npc"), x0 = unit(1, "npc"), x1 = unit(1, "npc"), gp = gpar(lwd = 1))

table_top <- tg |>
  gtable_add_grob(header_line, t = 1, b = 1, l = 2, r = nc, name = "hline_top") |>
  gtable_add_grob(header_line, t = 2, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(delim_line, t = 4, b = 1, l = 1, r = nc, name = "hline_delim") |>
  gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 3, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 2, b = nr, l = nc, r = nc,, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 2, b = nr, l = 1, r = 1, name = "vline_bullet_sep") |>
  gtable_add_grob(vline_model_sep, t = 2, b = nr, l = 4, r = 1, name = "vline_bullet_sep")


## Bottom panel----

### Plots----


scores_bottom <- as_tibble(as.data.frame(scores_rf[idx, ])) |>
  mutate(sex = adult_subset$sex, model = "rf") |>
  bind_rows(
    as_tibble(scores_gbm[idx, ]) |>
      mutate(sex = adult_subset$sex, model = "gbm")
  ) |>
  mutate(
    model = factor(
      model,
      levels = c("obs", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "Random Forest", "Gradient Boosting Model"
      )
    )
  )

transp_path_bottom <-
  transp_rf_path |>
  mutate(
    sex = factor("Female", levels = levels(adult_subset$sex)),
    model = "rf"
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
      levels = c("obs", "rf", "gbm"),
      labels = c(
        "Observed Values",
        "Random Forest",  "Gradient Boosting Model"
      )
    )
  )

ternary_adult_bottom <- ggtern(
  data = scores_bottom,
  mapping = aes(x = Married, y =  Separated, z = `Never-married`, colour = sex)
) +
  geom_point(size = .05, alpha = .5) +
  geom_line(
    data = transp_path_bottom,
    linewidth = .05, alpha = .5,
    mapping = aes(group = id_obs)
  ) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), guide = "none") +
  facet_wrap(~model) +
  labs(x = "Married", y = "Separated", z = "Never-married") +
  theme_light() +
  theme(
    strip.background = element_rect(fill = NA, colour = "black"),
    tern.axis.title = element_text(size = rel(.8)),
    strip.text.x = element_text(colour = "black"),
    text = element_text(family = font_family, size = unit(font_size, "pt")),
    axis.title = element_text(size = rel(.8)),
    tern.axis.arrow.show = TRUE,
    tern.axis.arrow.sep = .13,
    tern.axis.vshift = .05,
  ) +
  theme_hidetitles()

### Tables----
table_rf <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(sex)) |>
  filter(model %in% c("Random Forest")) |>
  mutate(
    across(
      c("Married", "Never-married", "Separated"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, Married, `Never-married`, Separated)

table_xgb <-
  tb_pred_transp_mean |>
  ungroup() |>
  arrange(model, type, desc(sex)) |>
  filter(model %in% c("Gradient Boosting Model")) |>
  mutate(
    across(
      c("Married", "Never-married", "Separated"),
      ~ str_c(scales::number(100 * .x, accuracy = 0.1), "%"))
  ) |>
  select(label, Married, `Never-married`, Separated)

label_grobs <- list(
  grobTree( # composition, Male
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "blue")),
    textGrob("(M)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # composition, Female
    textGrob("comp.", x = unit(0.1, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"), pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob("(F)", x = unit(0.7, "npc"), y = unit(0.5, "npc"), just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  ),
  grobTree( # T(red bullet)
    textGrob("T(", x = unit(0.4, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family)),
    pointsGrob(x = unit(0.625, "npc"), y = unit(0.45, "npc"),
               pch = 16, size = unit(2, "mm"), gp = gpar(col = "red")),
    textGrob(")", x = unit(0.65, "npc"), y = unit(0.5, "npc"),
             just = "left", gp = gpar(col = "black", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)


# Create the table grob
tg_2 <- tableGrob(
  table_rf |> select(-label) |>
    rename(M = Married, N = `Never-married`, S = "Separated") |>
    cbind(
      table_xgb |> select(-label) |>
        rename(M = Married, N = `Never-married`, S = "Separated")
    ),
  rows = NULL,
  theme = ttheme_minimal(
    core = list(fg_params = list(hjust = 0.5, fontsize = font_size*font_factor_table, fontfamily = font_family),
                padding = unit(c(3, 1.5), "mm")),
    colhead = list(fg_params = list(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family))
  )
)


# Add bullet column
tg_2 <- gtable_add_cols(tg_2, unit(35, "mm"), pos = 0)
for (i in seq_along(label_grobs)) {
  tg_2 <- gtable_add_grob(tg_2, label_grobs[[i]], t = i + 1, l = 1)
}
tg_2 <- gtable_add_grob(tg_2, textGrob(""), t = 1, l = 1)  # header cell

# Header
# Add empty header cell in top-left
tg_2 <- gtable_add_grob(tg_2, textGrob(""), t = 1, l = 1)

# Add title row that spans columns 2 to end of first model
nc <- ncol(tg_2)
tg_2 <- gtable_add_rows(tg_2, heights = unit(5, "mm"), pos = 0)

title_grob_1 <- textGrob(
  "random forest", gp = gpar(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family), just = "center"
)
title_grob_2 <- textGrob(
  "boosting", gp = gpar(fontface = "plain", fontsize = font_size*font_factor_table, fontfamily = font_family), just = "center"
)

tg_2 <- gtable_add_grob(
  tg_2, grobs = title_grob_1, t = 1, b = 1, l = 2, r = 2+3, name = "model_title"
) |>  gtable_add_grob(
  grobs = title_grob_2, t = 1, b = 1, l = 5, r = nc, name = "model_title_2"
)

plot(tg_2)


nr <- nrow(tg_2); nc <- ncol(tg_2)
table_bottom <- tg_2 |>
  #   gtable_add_grob(
  #   tg_2, grobs = top_line, t = 1, b = 1, l = 2, r = nc, name = "hline_header"
  # ) |>
  gtable_add_grob(header_line, t = 1, b = 1, l = 2, r = nc, name = "hline_top") |>
  gtable_add_grob(header_line, t = 2, b = 1, l = 1, r = nc, name = "hline_top") |>
  gtable_add_grob(bottom_line, t = nr, b = nr, l = 1, r = nc, name = "hline_bottom") |>
  gtable_add_grob(vline_left, t = 3, b = nr, l = 1, r = 1, name = "vline_left") |>
  gtable_add_grob(vline_right, t = 2, b = nr, l = nc, r = nc,, name = "vline_right") |>
  gtable_add_grob(vline_bullet_sep, t = 2, b = nr, l = 1, r = 1, name = "vline_bullet_sep") |>
  gtable_add_grob(vline_model_sep, t = 2, b = nr, l = 4, r = 1, name = "vline_bullet_sep")

## Export----
table_top_trimmed <- gtable::gtable_trim(table_top)
table_bottom_trimmed <- gtable::gtable_trim(table_bottom)
aligned_table_top <- grobTree(
  gtable_trim(table_top),  # trims padding
  vp = viewport(
    y = 1, just = "top",
    height = grobHeight(gtable_trim(table_top))
  )
)


aligned_table_bottom <- grobTree(
  table_bottom_trimmed,
  vp = viewport(y = 1, just = "top", height = grobHeight(table_bottom_trimmed))
)


gg_top_trimmed <- ggplotGrob(
  ternary_adult_top + theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 30, l = 0))
)
gg_bottom_trimmed <- ggplotGrob(
  ternary_adult_bottom +
    theme(
      plot.margin = ggplot2::margin(t = -30, 0, 0, 0)
    )
)


ternary_adult <-
  cowplot::plot_grid(
    gg_top_trimmed,
    aligned_table_top,
    gg_bottom_trimmed,
    aligned_table_bottom,
    ncol = 1,
    rel_heights = c(2.5, 1, 2.4, 1), # fine-tune
    align = "v",
    axis = "tblr"
  )


filename <- "ternary_adult"
ggsave(ternary_adult, file = str_c(path, filename, ".pdf"), height = 5.5*1.75, width = 3.5*1.75)
# # Crop PDF
system(paste0("pdfcrop ", path, filename, ".pdf ", path, filename, ".pdf"))

