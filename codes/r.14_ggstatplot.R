# ============================================================
# Author: Yaort
# Analysis: IOPD vs LOPD
# Visualization: ggstatsplot
# ============================================================
rm(list = ls())
gc()

setwd("/Users/yaoruntian/HuangLab/PDPP")

if (!dir.exists("14_ggstatplot")) {
  dir.create("14_ggstatplot")
}

setwd("14_ggstatplot")


# 1. Load packages --------------------------------------------

library(ggstatsplot)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


# 2. Read and process data ------------------------------------

raw <- read.xlsx("../03_create_matrix/07.final_df.xlsx")

df <- raw |>
  dplyr::filter(
    !is.na(Phenotype),
    trimws(Phenotype) != "",
    trimws(Phenotype) != "Asymptomatic"
  ) |>
  dplyr::mutate(
    Group = dplyr::case_when(
      Phenotype == "Classic infantile" ~ "IOPD",
      Phenotype %in% c(
        "Childhood",
        "Adult",
        "Childhood or Adult"
      ) ~ "LOPD",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(Group)) |>
  dplyr::mutate(
    Group = factor(
      Group,
      levels = c("IOPD", "LOPD")
    )
  )

cat("Rows after filtering:", nrow(df), "\n")
cat("Unique patients:", dplyr::n_distinct(df$Patient_ID), "\n")

print(table(df$Group))


# 3. Define analysis variables -------------------------------

variables <- c(
  "A1_SNP",
  "A2_SNP",
  "A1_Location",
  "A2_Location",
  "Severity",
  "CRIM_status",
  "Predicted_severity"
)

# Variables retaining Unknown
keep_unknown <- c(
  "Severity",
  "CRIM_status"
)


# 4. Global color mapping -------------------------------------

# Collect all categories from the 7 variables

all_categories <- sort(unique(unlist(
  lapply(variables, function(v) {
    
    x <- trimws(as.character(df[[v]]))
    
    # Standardize Unknown
    x[
      !is.na(x) & tolower(x) == "unknown"
    ] <- "Unknown"
    
    # Remove NA and empty values
    x <- x[
      !is.na(x) &
        x != "" &
        (
          v %in% keep_unknown |
            x != "Unknown"
        )
    ]
    
    x
  })
)))

# Generate a unified color palette
category_colors <- grDevices::hcl.colors(
  n = length(all_categories),
  palette = "Dynamic"
)

names(category_colors) <- all_categories

# Export color mapping table
color_table <- data.frame(
  Category = names(category_colors),
  Color = unname(category_colors)
)

openxlsx::write.xlsx(
  color_table,
  "02.category_color_mapping.xlsx",
  overwrite = TRUE
)

print(color_table)


# 5. Data preprocessing function ------------------------------

prepare_data <- function(variable) {
  
  dat <- df |>
    dplyr::transmute(
      Group,
      Category = trimws(
        as.character(.data[[variable]])
      )
    )
  
  # Standardize Unknown labels
  dat$Category[
    !is.na(dat$Category) &
      tolower(dat$Category) == "unknown"
  ] <- "Unknown"
  
  # Remove NA and empty values
  dat <- dat |>
    dplyr::filter(
      !is.na(Category),
      Category != ""
    )
  
  # Remove Unknown except for Severity and CRIM_status
  if (!variable %in% keep_unknown) {
    
    dat <- dat |>
      dplyr::filter(
        Category != "Unknown"
      )
    
  }
  
  # Set factor levels
  category_levels <- sort(unique(dat$Category))
  
  dat$Category <- factor(
    dat$Category,
    levels = category_levels
  )
  
  dat$Group <- droplevels(dat$Group)
  
  return(dat)
}


# 6. Plotting function ----------------------------------------

plot_one <- function(variable) {
  
  dat <- prepare_data(variable)
  
  n_category <- nlevels(dat$Category)
  
  cat(
    "\nVariable:", variable,
    "| Categories:", n_category,
    "| N:", nrow(dat),
    "\n"
  )
  
  if (
    n_category < 2 ||
    nlevels(dat$Group) < 2
  ) {
    
    warning(
      paste("Insufficient categories for", variable)
    )
    
    return(NULL)
  }
  
  p <- ggstatsplot::ggbarstats(
    data = dat,
    
    x = Category,
    y = Group,
    
    title = paste(
      "Chi-square test for LOPD/IOPD and",
      variable
    ),
    
    results.subtitle = TRUE,
    
    label = "percentage",
    
    proportion.test = TRUE,
    
    bf.message = TRUE,
    
    legend.title = variable
  ) +
    
    # Unified color mapping
    ggplot2::scale_fill_manual(
      values = category_colors,
      breaks = levels(dat$Category),
      drop = FALSE
    ) +
    
    # Unified theme
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        hjust = 0
      ),
      
      plot.subtitle = ggplot2::element_text(
        size = 8
      ),
      
      legend.text = ggplot2::element_text(
        size = 7
      ),
      
      legend.title = ggplot2::element_text(
        size = 8,
        face = "bold"
      ),
      
      axis.text = ggplot2::element_text(
        size = 9
      ),
      
      axis.title = ggplot2::element_text(
        size = 9
      ),
      
      panel.grid.major.y = ggplot2::element_line(
        color = "grey85",
        linewidth = 0.3
      ),
      
      panel.grid.minor = ggplot2::element_blank(),
      
      plot.background = ggplot2::element_rect(
        fill = "white",
        color = NA
      )
    )
  
  return(p)
}


# 7. Generate all 7 plots -------------------------------------

plots <- setNames(
  lapply(variables, plot_one),
  variables
)

# View individual plots
plots$A1_SNP
plots$A2_SNP
plots$A1_Location
plots$A2_Location
plots$Severity
plots$CRIM_status
plots$Predicted_severity


# 8. Save individual plots ------------------------------------

for (v in variables) {
  
  if (is.null(plots[[v]])) next
  
  ggplot2::ggsave(
    filename = paste0(v, ".pdf"),
    plot = plots[[v]],
    width = 11,
    height = 7,
    limitsize = FALSE
  )
  
  ggplot2::ggsave(
    filename = paste0(v, ".png"),
    plot = plots[[v]],
    width = 11,
    height = 7,
    dpi = 600,
    limitsize = FALSE
  )
}


# 9. Combine all plots with (A)-(G) tags ----------------------

valid <- Filter(
  Negate(is.null),
  plots
)

combined <- patchwork::wrap_plots(
  valid,
  ncol = 2
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  ) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(
      size = 14,
      face = "bold"
    ),
    
    plot.tag.position = c(0, 1)
  )

# Save combined figure
ggplot2::ggsave(
  filename = "all_7_variables.pdf",
  plot = combined,
  width = 22,
  height = 22,
  limitsize = FALSE
)

ggplot2::ggsave(
  filename = "all_7_variables.png",
  plot = combined,
  width = 22,
  height = 22,
  dpi = 600,
  limitsize = FALSE
)


# 10. Chi-square statistical summary -------------------------

summary <- lapply(variables, function(v) {
  
  dat <- prepare_data(v)
  
  tab <- table(
    dat$Group,
    dat$Category
  )
  
  if (
    nrow(tab) < 2 ||
    ncol(tab) < 2
  ) {
    return(NULL)
  }
  
  test <- suppressWarnings(
    chisq.test(tab)
  )
  
  n <- sum(tab)
  
  # Cramer's V
  cramers_v <- sqrt(
    as.numeric(test$statistic) /
      (
        n *
          min(
            nrow(tab) - 1,
            ncol(tab) - 1
          )
      )
  )
  
  data.frame(
    Variable = v,
    
    N_rows = n,
    
    Chi_square = unname(
      test$statistic
    ),
    
    df = unname(
      test$parameter
    ),
    
    p_value = test$p.value,
    
    Cramers_V = cramers_v,
    
    Min_expected = min(
      test$expected
    ),
    
    Cells_expected_below_5 = sum(
      test$expected < 5
    )
  )
})

summary <- dplyr::bind_rows(summary)

# BH correction
summary <- summary |>
  dplyr::mutate(
    p_adjust_BH = p.adjust(
      p_value,
      method = "BH"
    )
  )

# Export statistical results
openxlsx::write.xlsx(
  summary,
  "01.chi_square_summary.xlsx",
  overwrite = TRUE
)

print(summary)


# 11. Export filtered dataset ---------------------------------

openxlsx::write.xlsx(
  df,
  "03.filtered_dataset.xlsx",
  overwrite = TRUE
)


# 12. Finish --------------------------------------------------

cat("\n")
cat("========================================\n")
cat("Analysis completed!\n")
cat("========================================\n")

cat("Total rows:", nrow(df), "\n")
cat("Unique patients:", dplyr::n_distinct(df$Patient_ID), "\n")
cat("Total variables:", length(valid), "\n")

cat("Unknown retained in:\n")
cat("- Severity\n")
cat("- CRIM_status\n")

cat("Combined figure tags: (A)-(G)\n")

cat("Output directory:", getwd(), "\n")

cat("========================================\n")
