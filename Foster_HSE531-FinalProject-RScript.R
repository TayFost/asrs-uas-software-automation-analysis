#### HSE 531 Final Project R Script ####
#
# Topic: NASA ASRS UAS reports, Software and Automation contributing factors
#
# Goal of this script:
# 1) Import the submitted ASRS CSV export (which contains two header rows)
# 2) Create a reproducible binary outcome for whether a report is coded with
#    the "Software and Automation" contributing factor
# 3) Summarize rates with uncertainty (95% exact binomial confidence intervals)
# 4) Create one polished, publication style figure using ggplot2
# 5) Save the figure so others can reproduce it by re-running this script
#
# How to run:
# Put this .R script and the submitted CSV data file in the same folder.
# Then set your working directory to that folder and run the script top to bottom.


rm(list = ls(all = TRUE))
options(stringsAsFactors = FALSE)


#### 1) Load libraries ####
# If needed, install packages once:
# install.packages(c("dplyr", "ggplot2", "scales"))

library(dplyr)
library(ggplot2)
library(scales)


#### 2) Locate and load the data ####
# The ASRS Database Online export used here has TWO header rows:
# Row 1 contains variable groupings (Time, Place, Aircraft 1, ...).
# Row 2 contains the actual variable names.
# To get clean column names, we skip Row 1.

candidate_files <- c(
  list.files(pattern = "FinalProject-Data.*\\.csv$", ignore.case = TRUE),
  list.files(pattern = "asrs_uas\\.csv$", ignore.case = TRUE)
)

candidate_files <- unique(candidate_files)

if (length(candidate_files) == 0) {
  stop(
    paste(
      "Could not find the CSV data file in your working directory.",
      "Expected a file like 'Foster_HSE531-FinalProject-Data.csv' (preferred) or 'asrs_uas.csv'.",
      "In RStudio: Session > Set Working Directory, then re-run.",
      sep = "\n"
    )
  )
}

# Use the first matching file found
data_file <- candidate_files[1]
message("Reading data file: ", data_file)

uas_raw <- read.csv(
  file = data_file,
  skip = 1,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Make column names unique (the ASRS export repeats some fields for Aircraft 1 and Aircraft 2).
# This avoids downstream errors in dplyr when duplicate names exist.
names(uas_raw)[names(uas_raw) == ""] <- "unnamed"
names(uas_raw) <- make.unique(names(uas_raw))


#### 3) Inspect the imported data ####
# Quick checks to confirm the import worked.

dim(uas_raw)
head(uas_raw[, 1:10])

# Drop columns that are entirely missing (for example, an empty trailing column).
uas_raw <- uas_raw %>%
  select(where(~ !all(is.na(.x))))


#### 4) Create analysis variables ####
# We rename only the columns needed for this analysis, to keep later code readable.

uas <- uas_raw %>%
  rename(
    acn = `ACN`,
    far_part_raw = `Operating Under FAR Part`,
    control_mode_raw = `Control Mode (UAS)`,
    contributing_factors_raw = `Contributing Factors / Situations`
  ) %>%
  mutate(
    # Outcome variable (Y): 1 if the multi-label contributing factor field
    # contains "Software and Automation", otherwise 0.
    y_auto = as.integer(
      grepl(
        pattern = "Software and Automation",
        x = contributing_factors_raw,
        fixed = TRUE
      )
    ),
    
    # FAR part (regulatory regime): unify "Other Part 107" under "Part 107".
    far_part = case_when(
      is.na(far_part_raw) ~ NA_character_,
      far_part_raw == "Other Part 107" ~ "Part 107",
      TRUE ~ far_part_raw
    ),
    
    # Control mode: shorten labels so they are plot-friendly.
    control_mode = case_when(
      is.na(control_mode_raw) | trimws(control_mode_raw) == "" ~ "Unknown or not coded",
      control_mode_raw == "Manual Control" ~ "Manual control",
      control_mode_raw == "Waypoint Flying" ~ "Waypoint flying",
      control_mode_raw == "Autonomous / Fully Automated" ~ "Autonomous or fully automated",
      control_mode_raw == "Transitioning Between Modes" ~ "Transitioning between modes",
      TRUE ~ control_mode_raw
    )
  )

# Confirm one row per report (ACN should be unique).
stopifnot(anyDuplicated(uas$acn) == 0)


#### 5) Outcome prevalence with values.

n_total <- nrow(uas)
n_auto <- sum(uas$y_auto == 1, na.rm = TRUE)
p_auto <- n_auto / n_total

message("Total reports (rows): ", n_total)
message("Software and Automation coded (count): ", n_auto)
message("Software and Automation coded (percent): ", percent(p_auto, accuracy = 0.1))


#### 6) Group summaries with uncertainty (95% exact binomial CI) ####
# Uses exact (Clopper-Pearson) binomial confidence intervals via binom.test.

binom_ci <- function(k, n, conf_level = 0.95) {
  if (is.na(k) || is.na(n) || n == 0) {
    return(c(NA_real_, NA_real_))
  }
  as.numeric(binom.test(k, n, conf.level = conf_level)$conf.int)
}

far_summary <- uas %>%
  filter(!is.na(far_part), trimws(far_part) != "") %>%
  group_by(far_part) %>%
  summarise(
    n = n(),
    k = sum(y_auto, na.rm = TRUE),
    p = k / n,
    .groups = "drop"
  ) %>%
  mutate(
    ci_low = mapply(function(k, n) binom_ci(k, n)[1], k, n),
    ci_high = mapply(function(k, n) binom_ci(k, n)[2], k, n)
  ) %>%
  arrange(p)

mode_summary <- uas %>%
  group_by(control_mode) %>%
  summarise(
    n = n(),
    k = sum(y_auto, na.rm = TRUE),
    p = k / n,
    .groups = "drop"
  ) %>%
  mutate(
    ci_low = mapply(function(k, n) binom_ci(k, n)[1], k, n),
    ci_high = mapply(function(k, n) binom_ci(k, n)[2], k, n)
  ) %>%
  arrange(p)

far_summary
mode_summary


#### 7) One simple effect size ####
# This is descriptive, not causal, because ASRS is voluntary self-report data.

part107_row <- far_summary %>% filter(far_part == "Part 107")
recreational_row <- far_summary %>%
  filter(far_part == "Recreational Operations / Section 44809 (UAS)")

if (nrow(part107_row) == 1 && nrow(recreational_row) == 1) {
  prop_test <- prop.test(
    x = c(part107_row$k, recreational_row$k),
    n = c(part107_row$n, recreational_row$n),
    correct = FALSE
  )
  
  message("\nComparison: Part 107 vs Section 44809 (recreational)")
  message("Part 107 rate: ", percent(part107_row$p, accuracy = 0.1))
  message("Section 44809 rate: ", percent(recreational_row$p, accuracy = 0.1))
  message("Estimated difference (Part 107 minus Section 44809): ",
          round((part107_row$p - recreational_row$p) * 100, 1), " percentage points")
  message("Approx. 95% CI for the difference (prop.test): ",
          round(prop_test$conf.int[1] * 100, 1), ", ",
          round(prop_test$conf.int[2] * 100, 1), " percentage points")
}


#### 8) Final figure (ggplot2) ####
# Rationale:
# This is a multi-panel dot-and-whisker plot, supporting one main conclusion by comparing uncertainty-aware rates across:
# A) regulatory regime (FAR part), and
# B) an operational context feature (UAS control mode).
#
# The figure has points show group percent, whiskers show 95% exact binomial confidence intervals,
# and sample sizes are embedded in the y-axis labels.

wrap_lines <- function(x, width = 28) {
  sapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"))
}

plot_far <- far_summary %>%
  mutate(
    panel = "A. By FAR part (regulatory regime)",
    group_label = paste0(wrap_lines(far_part), "\n(n=", n, ")")
  )

plot_mode <- mode_summary %>%
  mutate(
    panel = "B. By UAS control mode (operational context)",
    group_label = paste0(wrap_lines(control_mode), "\n(n=", n, ")")
  )

plot_summary <- bind_rows(plot_far, plot_mode) %>%
  mutate(panel = factor(panel, levels = c(
    "A. By FAR part (regulatory regime)",
    "B. By UAS control mode (operational context)"
  )))

# Set x-axis max so both panels share the same scale.
x_max <- min(1, max(plot_summary$ci_high, na.rm = TRUE) + 0.05)
# Set x-axis max so both panels share the same scale.
x_max <- min(1, max(plot_summary$ci_high, na.rm = TRUE) + 0.05)

# NEW: panel colors (place here)
panel_cols <- c(
  "A. By FAR part (regulatory regime)" = "#0072B2",
  "B. By UAS control mode (operational context)" = "#D55E00"
)

final_plot <- ggplot(plot_summary, aes(y = reorder(group_label, p), x = p)) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    width = 0.25,
    linewidth = 0.7,
    color = "grey20"
  ) +
  
  geom_point(
    aes(fill = panel),
    size = 2.8, shape = 21, stroke = 0.9, color = "grey20"
  ) +
  
  scale_fill_manual(values = panel_cols) +
  guides(fill = "none") +
  facet_wrap(~ panel, scales = "free_y", nrow = 1) +
  labs(
    title = "Software and Automation contributing factors in ASRS UAS reports",
    subtitle = paste0(
      "Points show percent of reports coded 'Software and Automation' (overall ",
      percent(p_auto, accuracy = 0.1), ", n=", n_total, "). ",
      "Whiskers show 95% exact binomial confidence intervals."
    ),
    x = "Percent of reports",
    y = NULL,
    caption = paste(
      "Source: NASA ASRS Database Online export filtered to UAS operations.",
      "Outcome operationalized as 1 when the multi-label field",
      "'Contributing Factors / Situations' contains 'Software and Automation'.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Reduce clutter but keep enough structure to read values quickly.
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # Facet headers act like panel titles (A and B).
    strip.text = element_text(face = "bold", size = 12, hjust = 0),
    strip.background = element_blank(),
    
    # Make the figure readable if it gets resized in Canvas.
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title.x = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    
    # Keep caption left-justified and readable.
    plot.caption = element_text(size = 9, hjust = 0),
    
    # Add spacing between the two panels.
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

final_plot


#### 9) Save outputs ####

# Saves as a PNG 
name_tag <- "Foster"
if (grepl("[_-]HSE531", data_file, ignore.case = TRUE)) {
  name_tag <- sub("([_-]HSE531.*)$", "", data_file, ignore.case = TRUE)
  name_tag <- sub("\\\\.csv$", "", name_tag, ignore.case = TRUE)
}

figure_file <- paste0(name_tag, "_HSE531-FinalProject-Figure.png")
ggplot2::ggsave(figure_file, plot = final_plot, width = 11, height = 5, dpi = 300)

message("Saved figure to: ", figure_file)


#### 10) Reproducibility info ####
sessionInfo()