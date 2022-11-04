library(tidyverse)
library(gghdx)
library(patchwork)
gghdx()

som_dir <- Sys.getenv("SOM_ANALYSIS_DIR")
data_dir <- file.path(som_dir, "data")
plot_dir <- file.path(som_dir, "plots")

df_som <- read_csv(
  file.path(
    data_dir,
    "somalia_all_data.csv"
  )
)

###################
#### IPC PLOTS ####
###################

p_p3_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p3+_pct_current`),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = `phase_p3+_pct_current`
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p3+_pct_proj`),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = `phase_p3+_pct_proj`
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2021-12-01"),
      y = c(0.45, 0.333),
      label = c("Projected", "Current"),
      color = hdx_hex(c("tomato-hdx", "gray-black"))
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label,
      color = color
    ),
    fontface = "bold"
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "Phase 3+ (% of population)",
    title = "Population in phase 3+"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

p_p4_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p4+_pct_current`),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = `phase_p4+_pct_current`
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(`phase_p4+_pct_proj`),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = `phase_p4+_pct_proj`
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2021-12-01"),
      y = c(0.149, 0.0825),
      label = c("Projected", "Current"),
      color = hdx_hex(c("tomato-hdx", "gray-black"))
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label,
      color = color
    ),
    fontface = "bold"
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "Phase 4+ (% of population)",
    title = "Population in phase 4+"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

p_p5_pct <- ggplot() +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(phase_5_pct_current),
        !ipc_interpolated_current
      ),
    mapping = aes(
      x = date,
      y = phase_5_pct_current
    ),
    color = hdx_hex("gray-black")
  ) +
  geom_point(
    data = df_som %>%
      filter(
        !is.na(phase_5_pct_current),
        !ipc_interpolated_proj
      ),
    mapping = aes(
      x = date,
      y = phase_5_pct_current
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_text(
    data = data.frame(
      x = as.Date("2021-12-01"),
      y = c(0.006, 0.00245),
      label = c("Projected", "Current"),
      color = hdx_hex(c("tomato-hdx", "gray-black"))
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label,
      color = color
    ),
    fontface = "bold"
  ) +
  scale_color_identity() +
  labs(
    x = "",
    y = "Phase 5 (% of population)",
    title = "Population in phase 5"
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  )

# put IPC plots together

p_ipc <- p_p3_pct / p_p4_pct / p_p5_pct + 
  plot_annotation(title = 'IPC phases in Somalia',
                  theme = theme(plot.title = element_text(size = 18)))

ggsave(
  filename = file.path(plot_dir, "ipc.png"),
  plot = p_ipc,
  height = 10,
  width = 6,
  units = "in"
)
