library(dplyr)
library(ggplot2)
library(readr)
library(rvest)
library(sf)

turnout_url <- paste0(
  "https://results.elections.myflorida.com/TurnoutRpt.asp",
  "?ElectionDate=08/18/2026&DATAMODE="
)

county_boundaries <- tigris::counties(
  state = "FL",
  year = 2024,
  cb = TRUE,
  progress_bar = FALSE
) |>
  mutate(county_key = tolower(NAME)) |>
  st_transform(3086)

report_page <- read_html(turnout_url, encoding = "windows-1252")

if (!grepl(
  "August 18, 2026 Primary Election",
  html_text2(report_page),
  fixed = TRUE
)) {
  stop("The Florida Division of Elections page did not return the expected election.")
}

report_table <- html_table(report_page, fill = TRUE)[[1]]

county_turnout <- report_table |>
  filter(tolower(trimws(X1)) %in% county_boundaries$county_key) |>
  transmute(
    county = trimws(X1),
    county_key = tolower(trimws(X1)),
    registered = parse_number(X2),
    ballots = parse_number(X3),
    turnout_pct = parse_number(X4)
  ) |>
  filter(county_key %in% county_boundaries$county_key)

if (nrow(county_turnout) != 67 ||
    anyDuplicated(county_turnout$county_key) > 0 ||
    anyNA(county_turnout) ||
    any(county_turnout$registered <= 0) ||
    any(county_turnout$ballots < 0) ||
    any(county_turnout$turnout_pct < 0 | county_turnout$turnout_pct > 100)) {
  stop("The official turnout report did not provide valid data for all 67 counties.")
}

missing_results <- setdiff(county_boundaries$county_key, county_turnout$county_key)
extra_results <- setdiff(county_turnout$county_key, county_boundaries$county_key)

if (length(missing_results) > 0 || length(extra_results) > 0) {
  stop(
    "County names do not match the Florida boundary file. Missing: ",
    paste(missing_results, collapse = ", "),
    "; unmatched results: ",
    paste(extra_results, collapse = ", "),
    "."
  )
}

map_data <- county_boundaries |>
  left_join(county_turnout, by = "county_key") |>
  select(-county_key)

unavailable <- map_data |>
  filter(is.na(turnout_pct)) |>
  pull(NAME)

if (length(unavailable) > 0) {
  stop("Turnout values are missing for: ", paste(unavailable, collapse = ", "), ".")
}

caption <- paste(strwrap(paste0(
  "Source: Florida Division of Elections official turnout report; ",
  "U.S. Census Bureau 2024 county boundaries. Registration and turnout ",
  "percentages are reported by the Division of Elections."
), width = 115), collapse = "\n")

label_data <- map_data |>
  filter(NAME %in% c("Sarasota", "Miami-Dade", "Hillsborough", "Leon"))

label_points <- st_sf(
  NAME = label_data$NAME,
  geometry = st_point_on_surface(st_geometry(label_data))
)

turnout_map <- ggplot(map_data) +
  geom_sf(aes(fill = turnout_pct), color = "white", linewidth = 0.35) +
  geom_sf_label(
    data = label_points,
    aes(label = NAME),
    color = "#102B45",
    fill = "#F7FAFC",
    fontface = "bold",
    size = 3,
    linewidth = 0,
    label.padding = grid::unit(0.12, "lines")
  ) +
  scale_fill_gradientn(
    colors = c("#D9EFFA", "#86BEDB", "#397DA8", "#092C4C"),
    labels = \(x) paste0(format(round(x, 1), trim = TRUE), "%"),
    name = "Turnout (% of registered voters)",
    na.value = "#B8C2CC",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(8, "cm"),
      barheight = grid::unit(0.35, "cm")
    )
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "Who Showed Up? Florida's 2026 Primary",
    subtitle = "County voter turnout | August 18, 2026",
    caption = caption
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#F7FAFC", color = NA),
    plot.title = element_text(
      face = "bold",
      size = 23,
      color = "#102B45",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(color = "#536B80", size = 12),
    plot.caption = element_text(
      color = "#536B80",
      size = 8.5,
      hjust = 0,
      margin = margin(t = 12)
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", color = "#102B45"),
    legend.text = element_text(color = "#536B80"),
    plot.margin = margin(18, 18, 14, 18)
  )

ggsave(
  "florida_turnout.png",
  plot = turnout_map,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "#F7FAFC"
)
