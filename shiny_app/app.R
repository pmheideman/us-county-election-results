## U.S. County Election Results -- interactive map (President / House / Senate, 1990-2024).
## Data built by R/prepare_data.R from release/v1.0.0/*.csv. Run shiny_app/R/prepare_data.R first (or whenever the
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
states_sf   <- readRDS("data/states_sf.rds")
results     <- readRDS("data/results.rds")
gaps        <- readRDS("data/gaps.rds")
candidates  <- readRDS("data/candidates.rds")
meta        <- readRDS("data/meta.rds")
no_ballot   <- readRDS("data/no_ballot.rds")

## state FIPS -> postal code / name (counties_sf only carries the FIPS)
STATES <- results %>% distinct(state_fips, state_po, state) %>% group_by(state_fips) %>% slice(1) %>% ungroup()
state_po_of   <- function(fips) STATES$state_po[match(fips, STATES$state_fips)]
state_name_of <- function(fips) STATES$state[match(fips, STATES$state_fips)]
PARTY_WORD <- c(DEM = "Democrat", REP = "Republican")

OFFICE_CHOICES <- c("President" = "president", "U.S. House" = "house", "U.S. Senate" = "senate")
OFFICE_LABEL <- setNames(names(OFFICE_CHOICES), OFFICE_CHOICES)

pal_fun <- colorNumeric(palette = DIVERGING_RAMP, domain = c(0, 1), na.color = GAP_FILL)

fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.1f", 100 * x), "%"))
fmt_n   <- function(x) ifelse(is.na(x), "—", format(round(x), big.mark = ",", trim = TRUE))

## ---- UI ------------------------------------------------------------------------------------------------------
REPO_URL <- "https://github.com/pmheideman/us-county-election-results"
AUTHOR <- "Paul Heideman"
AUTHOR_EMAIL <- "pmheideman@gmail.com"

app_theme <- bs_theme(
  version = 5,
  bg = "#fffdf9", fg = "#1f1e1c",
  primary = "#1f1e1c", secondary = "#6b6a66",
  base_font = font_google("Inter"),
  heading_font = font_google("Source Serif 4", wght = c(400, 600, 700)),
  "border-color" = "#e2ddd2",
  "border-radius" = "0.5rem"
)

app_header <- div(
  class = "app-header",
  tags$img(src = "logo.svg", class = "app-logo", alt = ""),
  div(class = "app-titles",
      h1(class = "app-title", "U.S. County Election Results"),
      span(class = "app-subtitle", "President · House · Senate  ·  1990–2024")),
  div(class = "app-header-links",
      tags$a(class = "btn-github", href = REPO_URL, target = "_blank", rel = "noopener",
             title = "Source code and data on GitHub",
             icon("github"), span(class = "btn-label", "Code & data")))
)

ui <- page_sidebar(
  title = app_header,
  window_title = "U.S. County Election Results, 1990–2024",
  theme = app_theme,
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/html-to-image@1.11.11/dist/html-to-image.js"),
    tags$script(src = "map_png.js")
  ),
  sidebar = sidebar(
    width = 380,
    radioButtons("office", "Office", choices = OFFICE_CHOICES, selected = "president"),
    ## year slider flanked by previous/next buttons, which step through the office's actual election years
    div(class = "form-group shiny-input-container year-control",
        tags$label(class = "control-label", `for` = "year", "Year"),
        div(class = "year-stepper",
            actionButton("year_prev", NULL, icon = icon("chevron-left"), class = "btn-year-step", title = "Previous election"),
            div(class = "year-slider", sliderInput("year", NULL, min = 1992, max = 2024, value = 2024, step = 4, sep = "", ticks = FALSE, width = "100%")),
            actionButton("year_next", NULL, icon = icon("chevron-right"), class = "btn-year-step", title = "Next election"))),
    helpText("Color: Democratic share of the two-party vote (Dem + Rep). Tan counties had no ballot because the House candidate ran unopposed; gray dashed counties are missing data. Hover or click for details."),
    hr(),
    h5(class = "sidebar-section-title", "Selected county"),
    div(class = "detail-panel", uiOutput("detail_panel")),
    div(class = "sidebar-footer",
        p(class = "author-line", "Built by ", strong(AUTHOR), " · ",
          tags$a(href = paste0("mailto:", AUTHOR_EMAIL), AUTHOR_EMAIL)),
        p(class = "mb-0", "Open data (CC BY 4.0) and code (MIT). Sources, coverage notes and downloads on ",
          tags$a(href = REPO_URL, target = "_blank", rel = "noopener", "GitHub", .noWS = "after"), "."))
  ),
  card(
    class = "map-card",
    full_screen = TRUE,
    style = "padding:0;",
    leafletOutput("map", height = "100%"),
    tags$button(id = "download_png", type = "button", class = "btn-map-png", title = "Download the current map view as a PNG image",
                icon("download"), span("PNG"))
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

  ## previous/next buttons: move to the adjacent election year for the selected office, clamped at the ends. Steps count
  ## from the last requested year, not input$year, so quick repeated clicks each move a year before the slider catches up.
  year_target <- reactiveVal(NULL)
  observeEvent(input$year, year_target(input$year))
  step_year <- function(dir) {
    yrs <- sort(meta$years_by_office[[input$office]])
    cur <- year_target() %||% input$year
    nxt <- if (dir > 0) yrs[yrs > cur][1] else rev(yrs[yrs < cur])[1]
    if (!is.na(nxt)) { year_target(nxt); updateSliderInput(session, "year", value = nxt) }
  }
  observeEvent(input$year_prev, step_year(-1))
  observeEvent(input$year_next, step_year(+1))

  ## the selection the map and panel use: office + year, debounced so that dragging the slider (one value per year passed) or an office switch
  ## that also moves the slider triggers one redraw, not a queue of redraws that land late
  sel <- debounce(reactive(list(office = input$office, year = input$year)), 300)
  sel_office <- reactive(sel()$office)
  sel_year   <- reactive(sel()$year)

  sel_results <- reactive({
    req(sel_office(), sel_year())
    results %>% filter(office == sel_office(), year == sel_year())
  })

  ## this selection's no-ballot House seats (unopposed winner not on the ballot / not tabulated), by county
  sel_no_ballot <- reactive({
    if (sel_office() != "house") return(no_ballot[0, ])
    no_ballot %>% filter(year == sel_year())
  })

  ## every county in scope, left-joined to this selection's results. Counties without results fall in one of three kinds:
  ##   no_ballot -- House: every district in the county was won unopposed and the state put no race on the ballot (a known absence, not missing data)
  ##   no_race   -- no regular race for this office in the state this year (Senate seat not up, or only a special election, which is out of scope)
  ##   missing   -- a race was held but we have not found county returns
  map_data <- reactive({
    r <- sel_results()
    d <- counties_sf %>% left_join(r %>% select(county_fips, state_po, county_name, n_districts, dem_votes, rep_votes,
                                                  other_votes, total_votes, dem_two_party_share, rep_share_of_total, quality_flag),
                                    by = "county_fips")
    d$is_gap <- is.na(d$total_votes)
    nb_whole <- sel_no_ballot() %>% filter(whole_county) %>% pull(county_fips)
    g <- gaps %>% filter(office == sel_office(), year == sel_year())
    special <- g$state_fips[g$gap_reason == "special_election_only"]
    raced <- c(unique(r$state_fips), g$state_fips)       # states with a race for this office this year (covered or listed as a gap)
    d$gap_kind <- ifelse(!d$is_gap, NA_character_,
                  ifelse(d$county_fips %in% nb_whole, "no_ballot",
                  ifelse(d$state_fips_geo %in% special | !d$state_fips_geo %in% raced, "no_race", "missing")))
    d$fill <- ifelse(!d$is_gap, pal_fun(d$dem_two_party_share),
              ifelse(d$gap_kind == "no_ballot", NO_BALLOT_FILL, GAP_FILL))
    d
  })

  ## plain-language sentences for the unopposed seats of one county
  no_ballot_lines <- function(fips) {
    nb <- sel_no_ballot() %>% filter(county_fips == fips) %>% arrange(district)
    if (nrow(nb) == 0) return(character())
    sprintf("%s (%s) ran unopposed in District %d.", nb$candidate, PARTY_WORD[nb$party_group], as.integer(nb$district))
  }
  no_ballot_rule <- function(fips) { nb <- sel_no_ballot() %>% filter(county_fips == fips); if (nrow(nb)) nb$state_rule[1] else NA_character_ }

  gap_sentence <- function(row) {
    st_fips <- row$state_fips_geo; st_name <- state_name_of(st_fips) %||% "this state"; off <- OFFICE_LABEL[[sel_office()]]
    if (row$gap_kind == "no_race") {
      g <- gaps %>% filter(office == sel_office(), year == sel_year(), state_fips == st_fips)
      if (nrow(g) && g$gap_reason[1] == "special_election_only")
        return(sprintf("No regular %s election in %s in %s. The only %s race was a special election, which this dataset does not include yet.", off, st_name, sel_year(), off))
      return(sprintf("No %s election in %s in %s.", off, st_name, sel_year()))
    }
    g <- gaps %>% filter(office == sel_office(), year == sel_year(), state_fips == st_fips)
    note <- if (nrow(g) && g$gap_reason[1] == "source_not_found") " No county-level source has been found yet for this state and year." else ""
    paste0("Missing data: the ", off, " race was held here, but county-level returns are not in the dataset.", note)
  }

  ## hover labels for every county at once (vectorized: building them row by row took ~8 s per redraw, so redraws queued up behind the controls)
  county_labels <- function(d) {
    d <- st_drop_geometry(d)
    nm <- htmlEscape(d$county_name_geo); st <- state_po_of(d$state_fips_geo); st[is.na(st)] <- ""
    ## no-ballot seats per county, collapsed to one string
    nb <- sel_no_ballot() %>% arrange(district) %>% group_by(county_fips) %>%
      summarise(who = paste(sprintf("%s (%s-%d)", candidate, substr(party_group, 1, 1), as.integer(district)), collapse = ", "),
                head_txt = if (all(state_po == "AR")) "No votes counted" else "No ballot", .groups = "drop")
    k <- match(d$county_fips, nb$county_fips)
    ## gap sentences depend only on (state, kind): compute once per combination
    gk <- unique(d[d$is_gap & d$gap_kind != "no_ballot", c("state_fips_geo", "gap_kind")])
    gk$txt <- vapply(seq_len(nrow(gk)), function(i) gap_sentence(gk[i, ]), "")
    gtxt <- gk$txt[match(paste(d$state_fips_geo, d$gap_kind), paste(gk$state_fips_geo, gk$gap_kind))]
    covered <- sprintf("<b>%s, %s</b><br/>Dem %s &middot; Rep %s of two-party vote<br/>Total votes: %s%s", nm, d$state_po,
                       fmt_pct(d$dem_two_party_share), fmt_pct(1 - d$dem_two_party_share), fmt_n(d$total_votes),
                       ifelse(is.na(k), "", "<br/><i>Excludes a district won unopposed (no ballot)</i>"))
    no_ballot_txt <- sprintf("<b>%s, %s</b><br/>%s: %s ran unopposed", nm, st, nb$head_txt[k], htmlEscape(nb$who[k]))
    gap_txt <- sprintf("<b>%s, %s</b><br/>%s", nm, st, htmlEscape(gtxt))
    lapply(ifelse(!d$is_gap, covered, ifelse(d$gap_kind == "no_ballot", no_ballot_txt, gap_txt)), HTML)
  }

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
      addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(crossOrigin = "anonymous")) %>%   # CORS tiles, so the PNG export can read them
      ## state outlines sit in their own pane above the counties: redrawn counties (and hover highlights) are re-added to the
      ## overlay pane, which would otherwise bury lines added to the same pane earlier
      addMapPane("state_lines", zIndex = 450) %>%
      addPolylines(data = states_sf, color = "#000000", weight = 1.1, opacity = 1, fill = FALSE,
                   options = pathOptions(pane = "state_lines", interactive = FALSE)) %>%
      fitBounds(unname(meta$bounds["xmin"]), unname(meta$bounds["ymin"]), unname(meta$bounds["xmax"]), unname(meta$bounds["ymax"]))
  })

  observe({
    d <- map_data()
    labels <- county_labels(d)
    ## no clearGroup(): every redraw draws the same counties, and a polygon with an existing layerId replaces the old one in place,
    ## so the map never goes blank between the clear and the (~1 s) redraw
    leafletProxy("map") %>%
      addPolygons(data = d, layerId = ~county_fips, group = "counties",
                  fillColor = ~fill, fillOpacity = ~ifelse(gap_kind %in% "no_race", 0, 0.85),
                  color = ~ifelse(!is_gap, "#ffffff", ifelse(gap_kind == "no_ballot", NO_BALLOT_BORDER, ifelse(gap_kind == "no_race", "#c9c4b8", GAP_BORDER))),
                  weight = 0.4, dashArray = ~ifelse(gap_kind %in% "missing", "3,2", NA),
                  label = labels, labelOptions = labelOptions(sticky = TRUE),
                  highlightOptions = highlightOptions(weight = 1.6, color = "#0b0b0b", bringToFront = TRUE)) %>%
      clearControls() %>%
      addControl(html = sprintf('<div class="map-title-main">%s &middot; %s</div><div class="map-title-credit">Map: %s &middot; github.com/pmheideman/us-county-election-results</div>',
                                OFFICE_LABEL[[sel_office()]], sel_year(), AUTHOR),
                 position = "topright", className = "map-title") %>%
      addLegend(position = "bottomright", pal = pal_fun, values = c(0, 1), title = "Dem. share<br/>of D+R vote",
                labFormat = labelFormat(transform = function(x) 100 * x, suffix = "%"), opacity = 0.9) %>%
      { kinds <- unique(na.omit(d$gap_kind)); lg <- c(no_ballot = "Unopposed, no ballot", missing = "Data not found")[intersect(c("no_ballot", "missing"), kinds)]
        if (length(lg)) addLegend(., position = "bottomright", colors = c(no_ballot = NO_BALLOT_FILL, missing = GAP_FILL)[names(lg)], labels = unname(lg), opacity = 0.9) else . }
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
      title <- div(class = "detail-county", paste0(nm, ", ", state_po_of(st)))
      if (row$gap_kind == "no_ballot") {
        is_ar <- identical(state_po_of(st), "AR")
        lead <- if (is_ar) sprintf("No votes were counted for the House race here in %s.", sel_year()) else sprintf("There was no House race on the ballot here in %s.", sel_year())
        return(tagList(title, p(strong(lead)), lapply(no_ballot_lines(fips), p), p(class = "text-muted", no_ballot_rule(fips)),
                       p(class = "text-muted small", "This is not missing data: no county returns exist for an unopposed seat.")))
      }
      return(tagList(title, p(gap_sentence(row))))
    }

    cands <- candidates %>% filter(office == sel_office(), year == sel_year(), county_fips == fips)
    flags <- setdiff(strsplit(row$quality_flag %||% "", ";")[[1]], c("excludes_unopposed_seat", NA))   # that one is explained in its own note below
    header <- tagList(
      div(class = "detail-county", paste0(row$county_name, ", ", row$state_po)),
      p(strong(fmt_pct(row$dem_two_party_share)), " Dem. — ", strong(fmt_pct(1 - row$dem_two_party_share)), " Rep. (two-party share)"),
      p(class = "text-muted small", "Total votes: ", fmt_n(row$total_votes),
        if (length(flags)) paste0(" · flag: ", paste(flags, collapse = "; ")) else NULL)
    )

    nb_note <- if (length(no_ballot_lines(fips))) div(class = "nb-note",
      p(class = "small fw-bold mb-1", "Not included: a district with no ballot"),
      lapply(no_ballot_lines(fips), function(t) p(class = "small mb-1", t)),
      p(class = "small text-muted mb-0", no_ballot_rule(fips), " The totals above cover only this county's contested districts.")) else NULL

    candidate_row <- function(c) {
      pg <- if (is.na(c$party_group) || !c$party_group %in% names(PARTY_COLOR)) "OTHER" else c$party_group
      party_label <- if (!is.na(c$party) && nzchar(c$party)) c$party else pg
      div(style = paste0("border-left:3px solid ", PARTY_COLOR[[pg]], "; padding-left:6px; margin-bottom:3px;"),
          span(c$candidate), span(class = "text-muted", paste0(" (", party_label, ") — ", fmt_n(c$votes))))
    }

    if (sel_office() == "house" && !is.na(row$n_districts) && row$n_districts > 1) {
      by_district <- cands %>% arrange(district, desc(votes))
      district_blocks <- lapply(split(by_district, by_district$district), function(dd) {
        tagList(p(class = "small fw-bold mt-2 mb-1", paste("District", dd$district[1])),
                lapply(seq_len(nrow(dd)), function(i) candidate_row(dd[i, ])))
      })
      xdist <- tagList(p(class = "small fw-bold mt-3 mb-1", "County-wide total (all districts)"),
                        p(class = "small", "Dem: ", fmt_n(row$dem_votes), " · Rep: ", fmt_n(row$rep_votes), " · Other: ", fmt_n(row$other_votes)))
      return(tagList(header, tags$div(district_blocks), xdist, nb_note))
    }

    cands <- cands %>% arrange(desc(votes))
    tagList(header, lapply(seq_len(nrow(cands)), function(i) candidate_row(cands[i, ])), nb_note)
  })
}

shinyApp(ui, server)
