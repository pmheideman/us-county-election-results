## Diverging color ramp for the two-party vote share (Democratic share of D+R). Built with colorspace::diverging_hcl
## anchored on this project's blue/red categorical hues (see the dataviz skill's reference palette), validated via
## scripts/validate_palette.js: the two endpoints clear the CVD-separation and normal-vision-floor gates by a wide
## margin (worst-pair Delta E 25.6 CVD / 30.1 normal, targets are >=8 / >=15). The validator's "lightness band" /
## "contrast vs surface" checks do not apply here -- those are categorical-palette gates; a diverging/sequential ramp
## is explicitly out of that check's scope (its own output says so), and dark, saturated endpoints are exactly what
## a diverging polarity scale needs.
suppressMessages(library(colorspace))

## Ordered light (Republican, share -> 0) to dark... actually ordered so index 1 = share 0 (deep red), index 11 = share 1 (deep blue).
DIVERGING_RAMP <- rev(diverging_hcl(11, h = c(253, 12), c = 90, l = c(30, 97), power = 1.1))
DIVERGING_RAMP[6] <- "#f0efec"   # force the exact neutral midpoint from the dataviz skill's reference palette

## Gap / no-data fill: a flat, clearly-non-ramp gray (not to be confused with a near-50/50 county, which sits near
## the ramp's own light gray midpoint) -- deliberately more saturated-looking (darker, warmer-neutral) than the
## ramp's midpoint, plus the app always renders gap counties with a dashed border as a second cue.
GAP_FILL <- "#b0aca3"
GAP_BORDER <- "#8a8578"

## Party-group colors for the candidate detail panel (text, not map fill) -- reuse the same ramp's saturated ends
## for visual consistency with the map.
PARTY_COLOR <- c(DEM = DIVERGING_RAMP[11], REP = DIVERGING_RAMP[1], OTHER = "#6b6a66")
