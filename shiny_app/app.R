## U.S. County Election Results -- interactive map (President / House / Senate, 1990-2024).
## Data built by R/prepare_data.R from release/v0.2.0/*.csv. Run shiny_app/R/prepare_data.R first (or whenever the
## release is rebuilt) before launching this app.
##
## AK/HI: excluded for now (see prepare_data.R header). The map uses real lat/lon (leaflet, no Albers/USA
## projection), so adding them later needs no reprojection -- just re-run prepare_data.R with the exclusion lifted.

suppressMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(htmltools)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
source(file.path("R", "palette.R"))

## ---- data --------------------------------------------------------------------------------------------------
counties_sf <- readRDS("data/counties_sf.rds")
results     <- readRDS("data/results.rds")
gaps        <- readRDS("data/gaps.rds")
candidates  <- readRDS("data/candidates.rds")
meta        <- readRDS("data/meta.rds")

OFFICE_CHOICES <- c("President" = "president", "U.S. House" = "house", "U.S. Senate" = "senate")
OFFICE_LABEL <- setNames(names(OFFICE_CHOICES), OFFICE_CHOICES)

pal_fun <- colorNumeric(palette = DIVERGING_RAMP, domain = c(0, 1), na.color = GAP_FILL)

fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.1f", 100 * x), "%"))
fmt_n   <- function(x) ifelse(is.na(x), "—", format(round(x), big.mark = ","))

## ---- UI ------------------------------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "U.S. County Election Results, 1990–2024",
  theme = bs_theme(version = 5, base_font = font_google("Inter"), primary = "#2a78d6"),
  sidebar = sidebar(
    width = 380,
    radioButtons("office", "Office", choices = OFFICE_CHOICES, selected = "president"),
    sliderInput("year", "Year", min = 1992, max = 2024, value = 2024, step = 4, sep = "", ticks = FALSE),
    helpText("Color: Democratic share of the two-party vote (Dem + Rep). Gray counties have no data for this selection — hover or click for why."),
    hr(),
    h5("Selected county"),
    uiOutput("detail_panel")
  ),
  card(
    full_screen = TRUE,
    style = "padding:0;",
    leafletOutput("map", height = "100%")
  )
)

## ---- server --------------------------------------------------------------------------------------------------
server <- function(input, output, session) {

  ## keep the year slider's range/step in sync with the selected office's actual election years
  observeEvent(input$office, {
    yrs <- sort(meta$years_by_office[[input$office]])
    step <- if (length(yrs) > 1) min(diff(yrs)) else 1
    cur <- isolate(input$year)
    new_val <- yrs[which.min(abs(yrs - cur))]
    updateSliderInput(session, "year", min = min(yrs), max = max(yrs), step = step, value = new_val)
  }, ignoreInit = FALSE)

  sel_results <- reactive({
    req(input$office, input$year)
    results %>% filter(office == input$office, year == input$year)
  })

  ## every county in scope, left-joined to this selection's results -- unmatched rows are genuine gaps
  map_data <- reactive({
    r <- sel_results()
    d <- counties_sf %>% left_join(r %>% select(county_fips, state_po, county_name, n_districts, dem_votes, rep_votes,
                                                  other_votes, total_votes, dem_two_party_share, rep_share_of_total, quality_flag),
                                    by = "county_fips")
    d$fill <- ifelse(is.na(d$dem_two_party_share), GAP_FILL, pal_fun(d$dem_two_party_share))
    d$is_gap <- is.na(d$total_votes)
    d
  })

  gap_reason_for <- function(state_po, yr, off) {
    g <- gaps %>% filter(state_po == !!state_po, year == !!yr, office == !!off)
    if (nrow(g) == 0) return(NA_character_)
    paste0(g$gap_reason[1], if (!is.na(g$note[1]) && nzchar(g$note[1])) paste0(" (", g$note[1], ")") else "")
  }

  county_label <- function(row) {
    nm <- row$county_name_geo
    if (row$is_gap) {
      reason <- gap_reason_for(row$state_fips_geo, input$year, input$office)
      reason_txt <- if (is.na(reason)) "no election held / data not yet available" else reason
      return(HTML(sprintf("<b>%s</b><br/>No data — %s", nm, htmlEscape(reason_txt))))
    }
    HTML(sprintf("<b>%s, %s</b><br/>Dem %s &middot; Rep %s of two-party vote<br/>Total votes: %s",
                  nm, row$state_po, fmt_pct(row$dem_two_party_share), fmt_pct(1 - row$dem_two_party_share), fmt_n(row$total_votes)))
  }

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      fitBounds(unname(meta$bounds["xmin"]), unname(meta$bounds["ymin"]), unname(meta$bounds["xmax"]), unname(meta$bounds["ymax"]))
  })

  observe({
    d <- map_data()
    labels <- lapply(seq_len(nrow(d)), function(i) county_label(d[i, ]))
    leafletProxy("map") %>%
      clearGroup("counties") %>%
      addPolygons(data = d, layerId = ~county_fips, group = "counties",
                  fillColor = ~fill, fillOpacity = 0.85, color = ~ifelse(is_gap, GAP_BORDER, "#ffffff"),
                  weight = 0.4, dashArray = ~ifelse(is_gap, "3,2", NA),
                  label = labels, labelOptions = labelOptions(sticky = TRUE),
                  highlightOptions = highlightOptions(weight = 1.6, color = "#0b0b0b", bringToFront = TRUE)) %>%
      clearControls() %>%
      addLegend(position = "bottomright", pal = pal_fun, values = c(0, 1), title = "Dem. share<br/>of D+R vote",
                labFormat = labelFormat(transform = function(x) 100 * x, suffix = "%"), opacity = 0.9)
  })

  ## ---- click detail --------------------------------------------------------------------------------------
  clicked_fips <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, { clicked_fips(input$map_shape_click$id) })

  output$detail_panel <- renderUI({
    fips <- clicked_fips()
    if (is.null(fips)) return(p(class = "text-muted", "Click a county on the map to see full results."))

    d <- map_data() %>% filter(county_fips == fips) %>% st_drop_geometry()
    if (nrow(d) == 0) return(NULL)
    row <- d[1, ]
    nm <- row$county_name_geo; st <- row$state_fips_geo

    if (row$is_gap) {
      reason <- gap_reason_for(st, input$year, input$office)
      reason_txt <- if (is.na(reason)) "no election held this cycle, or county-level data not yet available" else reason
      return(tagList(h6(paste0(nm, " (", st, ")")), p(class = "text-muted", paste0("No data — ", reason_txt))))
    }

    cands <- candidates %>% filter(office == input$office, year == input$year, county_fips == fips)
    header <- tagList(
      h6(paste0(row$county_name, ", ", row$state_po)),
      p(strong(fmt_pct(row$dem_two_party_share)), " Dem. — ", strong(fmt_pct(1 - row$dem_two_party_share)), " Rep. (two-party share)"),
      p(class = "text-muted small", "Total votes: ", fmt_n(row$total_votes),
        if (!is.na(row$quality_flag)) paste0(" · flag: ", row$quality_flag) else NULL)
    )

    candidate_row <- function(c) {
      pg <- if (is.na(c$party_group) || !c$party_group %in% names(PARTY_COLOR)) "OTHER" else c$party_group
      party_label <- if (!is.na(c$party) && nzchar(c$party)) c$party else pg
      div(style = paste0("border-left:3px solid ", PARTY_COLOR[[pg]], "; padding-left:6px; margin-bottom:3px;"),
          span(c$candidate), span(class = "text-muted", paste0(" (", party_label, ") — ", fmt_n(c$votes))))
    }

    if (input$office == "house" && !is.na(row$n_districts) && row$n_districts > 1) {
      by_district <- cands %>% arrange(district, desc(votes))
      district_blocks <- lapply(split(by_district, by_district$district), function(dd) {
        tagList(p(class = "small fw-bold mt-2 mb-1", paste("District", dd$district[1])),
                lapply(seq_len(nrow(dd)), function(i) candidate_row(dd[i, ])))
      })
      xdist <- tagList(p(class = "small fw-bold mt-3 mb-1", "County-wide total (all districts)"),
                        p(class = "small", "Dem: ", fmt_n(row$dem_votes), " · Rep: ", fmt_n(row$rep_votes), " · Other: ", fmt_n(row$other_votes)))
      return(tagList(header, tags$div(district_blocks), xdist))
    }

    cands <- cands %>% arrange(desc(votes))
    tagList(header, lapply(seq_len(nrow(cands)), function(i) candidate_row(cands[i, ])))
  })
}

shinyApp(ui, server)
