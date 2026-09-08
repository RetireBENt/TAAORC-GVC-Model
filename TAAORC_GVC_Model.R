# ================================================================
# TAAORC GVC Model
# KiwiSaver GVC Microsimulation — Shiny App
# Te Ara Ahunga Ora Retirement Commission
#
# STRUCTURE
#
#   SUMMARY TAB — three clearly labelled sections:
#     Section 1 "2024 Observed" — fixed IRD data, never changes
#     Section 2 "2026 Default Baseline" — default policy, no user inputs
#     Section 3 "Your Scenario" — reactive to all sidebar inputs,
#                                  shows change vs 2026 default
#     When all inputs are at defaults, Section 3 is greyed out
#     with a note "No changes from 2026 default".
#
#   COLLAPSIBLE SIDEBAR — five accordion panels, each independently
#   expandable/collapsible:
#     1. GVC Settings       (match rate, max cap)
#     2. Income Settings    (income cap, income targeting)
#     3. Age Settings       (eligible bands, age targeting)
#     4. Uptake             (flat or tiered non-contributor uptake)
#     5. Income Growth      (growth toggle and years)
#   All panels start collapsed except GVC Settings.
#   A "Reset all to defaults" button restores every input.
# ================================================================

library(shiny)
library(dplyr)


# ================================================================
# LOAD DATA
# ================================================================

inc_2024 <- read.csv("dataset_by_income.csv", stringsAsFactors = FALSE)
age_2024 <- read.csv("dataset_by_age.csv",    stringsAsFactors = FALSE)
ds26     <- read.csv("dataset_2026.csv",       stringsAsFactors = FALSE)

INC_2024_LEVELS <- c(
  "$1 - $20,000","$20,000.01 - $40,000","$40,000.01 - $60,000",
  "$60,000.01 - $80,000","$80,000.01 - $100,000","$100,000.01 - $120,000",
  "$120,000.01 +","No income info"
)
AGE_LEVELS_KS <- c("00-17","18-24","25-34","35-44","45-54","55-64","65+")
AGE_LEVELS_26 <- c("Under 16","16-17","18-24","25-34","35-44","45-54","55-64","65+")

inc_2024$income_band <- factor(inc_2024$income_band, levels = INC_2024_LEVELS)
age_2024$age_band    <- factor(age_2024$age_band,    levels = AGE_LEVELS_KS)
ds26$age_band        <- factor(ds26$age_band,        levels = AGE_LEVELS_26)


# ================================================================
# CONSTANTS
# ================================================================

TANGAROA  <- "#0e3653"
TANIWHA   <- "#1d9ba0"
WAIRUA    <- "#60bfb1"
TAWHIRI   <- "#4d4d4f"
RANGINUI1 <- "#a8dde7"
WARM      <- "#fbdaa2"
PAPATU    <- "#b5dec7"

OBS_GVC_2024     <- 1022341210
OBS_MEMBERS_2024 <- 3348048
OBS_MEMBERS_2025 <- 3405406

DEF_MATCH       <- 0.25
DEF_MAX_GVC     <- 260.72
DEF_INC_CAP     <- 180000
DEF_AGE_KEYS    <- c("ks_16_17","ks_18_24","ks_25_34","ks_35_44","ks_45_54","ks_55_64")
DEF_INC_THRESH  <- 40000
DEF_UPTAKE_THRESH <- 50000

DEF_MIN_RATE <- 0.035  # default 2026 minimum contribution rate

# Base rate vector — the minimum rate (pp3) is overridden at runtime
# by the user's contribution rate slider. Members at higher rates
# (pp4/pp6/pp8/pp10) are unaffected.
RATES_2026 <- c(pp3=DEF_MIN_RATE, pp4=0.04, pp6=0.06, pp8=0.08, pp10=0.10)

AGE_BAND_MAP <- data.frame(
  key   = c("ks_u16","ks_16_17","ks_18_24","ks_25_34",
            "ks_35_44","ks_45_54","ks_55_64","ks_65p"),
  label = AGE_LEVELS_26,
  stringsAsFactors = FALSE
)


# ================================================================
# CORE GVC CALCULATION FUNCTION
# ================================================================
# Full parameter documentation is in the methodology document.
# members_eff = members that are both age- and income-eligible (0 otherwise);
# used for member-count aggregation in the By Income Band / By Age Band tabs.

calc_gvc_2026 <- function(
  data, match_rate, max_gvc, inc_cap, elig_age_keys,
  min_rate = 0.035,      # minimum contribution rate (shifts pp3 members only)
  uptake_pct = 0, growth_rate = 0, growth_years = 0,
  inc_target_on = FALSE, inc_target_thresh = 0,
  boost_match_inc = NULL, boost_max_gvc_inc = NULL,
  age_target_keys = NULL, boost_match_age = NULL, boost_max_gvc_age = NULL,
  uptake_weight_on = FALSE, uptake_thresh = 0,
  uptake_pct_below = 0, uptake_pct_above = 0
) {
  d <- data %>%
    mutate(
      inc_eff      = inc_mid * ((1 + growth_rate) ^ growth_years),
      # Income eligibility is tested on the GROWN income (inc_eff), not the
      # band's fixed lower bound. Under income growth this lets members whose
      # income rises past the cap cross out of eligibility (GVC -> $0), and it
      # responds to any user-set inc_cap. With growth off, inc_eff == inc_mid
      # and (for a cap on a band boundary) this reproduces the prior partition
      # exactly, so the default baseline is unchanged.
      inc_eligible = (inc_eff <= inc_cap) & (inc_low > 0),
      age_eligible = age_band_key %in% elig_age_keys,
      eff_match    = match_rate,
      eff_cap      = max_gvc
    )

  if (inc_target_on && !is.null(boost_match_inc) && !is.null(boost_max_gvc_inc)) {
    # Threshold compared against GROWN income (inc_eff), so under income growth
    # members whose income rises above the target threshold correctly drop out
    # of the boosted group (consistent with the eligibility-cap treatment).
    below <- d$inc_eff < inc_target_thresh
    d$eff_match[below] <- boost_match_inc
    d$eff_cap[below]   <- boost_max_gvc_inc
  }

  if (!is.null(age_target_keys) && length(age_target_keys) > 0 &&
      !is.null(boost_match_age) && !is.null(boost_max_gvc_age)) {
    in_age <- d$age_band_key %in% age_target_keys
    d$eff_match[in_age] <- boost_match_age
    d$eff_cap[in_age]   <- boost_max_gvc_age
  }

  d <- d %>%
    mutate(
      eff_uptake = if (uptake_weight_on)
        ifelse(inc_eff < uptake_thresh, uptake_pct_below, uptake_pct_above)
        else uptake_pct,
      members_base        = members,
      members_new_contrib = members_base * ppn * (eff_uptake / 100),
      pp3_eff             = pp3 + (members_new_contrib / members_base),
      ppn_eff             = ppn - (members_new_contrib / members_base),
      min_rate_used = min_rate,
      gvc_pp3  = pmin(inc_eff * min_rate          * eff_match, eff_cap),
      gvc_pp4  = pmin(inc_eff * RATES_2026["pp4"] * eff_match, eff_cap),
      gvc_pp6  = pmin(inc_eff * RATES_2026["pp6"] * eff_match, eff_cap),
      gvc_pp8  = pmin(inc_eff * RATES_2026["pp8"] * eff_match, eff_cap),
      gvc_pp10 = pmin(inc_eff * RATES_2026["pp10"]* eff_match, eff_cap),
      total_gvc = ifelse(
        age_eligible & inc_eligible,
        members_base * (pp3_eff*gvc_pp3 + pp4*gvc_pp4 + pp6*gvc_pp6 +
                        pp8*gvc_pp8 + pp10*gvc_pp10),
        0
      ),
      # members_eff counts members who are BOTH age- and income-eligible.
      # (Previously age only, which counted >cap members as "eligible" and
      # prevented the By Income/Age tabs reconciling with the Summary and the
      # methodology. Respecting inc_eligible also makes members leave the
      # eligible-member counts when income growth pushes them past the cap.)
      members_eff = ifelse(age_eligible & inc_eligible, members_base, 0)
    )
  d
}


# ================================================================
# HELPERS
# ================================================================

fmt_m   <- function(x) paste0("$", formatC(round(x/1e6,1), format="f", digits=1), "m")
fmt_dol <- function(x) paste0("$", format(round(x), big.mark=","))
fmt_n   <- function(x) format(round(x), big.mark=",")
fmt_pct <- function(x) paste0(round(x*100, 1), "%")
# Signed member counts, used for coverage changes vs the 2026 default.
fmt_signed <- function(x) {
  x <- round(x)
  ifelse(is.na(x), "-",
    ifelse(x == 0, "0",
      paste0(ifelse(x > 0, "+", "-"), format(abs(x), big.mark=","))))
}
# Share of contributing members receiving their full applicable maximum.
# eff_cap and eff_match already reflect any income or age targeting, so this
# works for every option without needing to know which lever was used.
at_max_share <- function(d) {
  rates <- c(pp4=RATES_2026[["pp4"]], pp6=RATES_2026[["pp6"]],
             pp8=RATES_2026[["pp8"]], pp10=RATES_2026[["pp10"]])
  n_tot <- d$members_eff * d$pp3_eff
  n_max <- n_tot * ((d$inc_eff * d$min_rate_used * d$eff_match) >= d$eff_cap - 1e-9)
  for (tier in names(rates)) {
    nt <- d$members_eff * d[[tier]]
    n_tot <- n_tot + nt
    n_max <- n_max + nt * ((d$inc_eff * rates[[tier]] * d$eff_match) >= d$eff_cap - 1e-9)
  }
  list(total=sum(n_tot, na.rm=TRUE), at_max=sum(n_max, na.rm=TRUE))
}

metric_card <- function(value, label, sub=NULL, cls="card-neutral") {
  div(class=paste("summary-card", cls),
    div(class="card-value", value),
    div(class="card-label", label),
    if (!is.null(sub)) div(class="card-sub", sub)
  )
}

change_card <- function(value_m, label, ref_label) {
  sign_str <- if (value_m >= 0) "+" else ""
  cls      <- if (value_m >= 0) "card-positive" else "card-negative"
  div(class=paste("summary-card", cls),
    div(class="card-value", paste0(sign_str, fmt_m(value_m))),
    div(class="card-label", label),
    div(class="card-sub",   ref_label)
  )
}

# Collapsible panel using Shiny's built-in bsCollapse-style HTML
# (pure HTML/CSS accordion — no bslib dependency required)
accordion_panel <- function(id, title, ..., open=FALSE) {
  panel_id <- paste0("panel_", id)
  icon_id  <- paste0("icon_", id)
  div(class="accordion-item",
    tags$div(class="accordion-header",
      `data-target`=paste0("#", panel_id),
      onclick=paste0(
        "var el=document.getElementById('", panel_id, "');",
        "var ic=document.getElementById('", icon_id, "');",
        "if(el.style.display==='none'){el.style.display='block';ic.innerHTML='&#9650;';}",
        "else{el.style.display='none';ic.innerHTML='&#9660;';}"
      ),
      tags$span(class="accordion-title", title),
      tags$span(id=icon_id, class="accordion-icon",
                HTML(if (open) "&#9650;" else "&#9660;"))
    ),
    div(id=panel_id, class="accordion-body",
        style=if (open) "display:block;" else "display:none;",
        ...)
  )
}

bar_chart <- function(values, groups, title, ylab, colours,
                      ref_values=NULL, ref_label=NULL) {
  ymax <- max(c(values, ref_values), na.rm=TRUE) * 1.22
  if (is.na(ymax) || ymax == 0) ymax <- 1
  ymin <- if (any(values < 0, na.rm=TRUE)) min(values, na.rm=TRUE)*1.2 else 0
  par(mar=c(8.5,5.5,3.5,1.5), bg="#f7f9fa", family="sans")
  bp <- barplot(values, names.arg=groups, las=2, col=colours, border=NA,
                ylim=c(ymin,ymax), ylab=ylab, main=title,
                cex.names=0.78, cex.axis=0.85, cex.lab=0.9,
                col.axis=TAWHIRI, col.lab=TANGAROA, col.main=TANGAROA)
  if (!is.null(ref_values)) {
    lines(bp, ref_values, col=WARM, lwd=2, lty=2)
    points(bp, ref_values, col=WARM, pch=19, cex=0.8)
    if (!is.null(ref_label))
      legend("topright", legend=ref_label, col=WARM,
             lty=2, lwd=2, bty="n", cex=0.78)
  }
  abline(h=0, col=TAWHIRI, lty=1, lwd=0.5)
  nonzero <- which(abs(values) > ymax*0.005)
  text(bp[nonzero], values[nonzero]+sign(values[nonzero])*ymax*0.018,
       paste0("$", round(abs(values[nonzero]),1), "m"),
       cex=0.68, col=TANGAROA, adj=c(0.5,0.5))
  invisible(bp)
}

# Reporting bands run in uniform $10k steps to $180k. The underlying data is
# in $1,000 bands throughout this range, so no detail is lost or invented by
# reporting at this resolution. Uniform steps also keep eligibility caps, which
# move in $10k increments, aligned to band edges rather than falling inside a
# band.
BROAD_INC_BREAKS <- c(0,10000,20000,30000,40000,50000,60000,70000,
                       80000,90000,100000,110000,120000,130000,140000,
                       150000,160000,170000,180000,Inf)
BROAD_INC_LABELS <- c("$1-$10k","$10k-$20k","$20k-$30k","$30k-$40k",
                       "$40k-$50k","$50k-$60k","$60k-$70k","$70k-$80k",
                       "$80k-$90k","$90k-$100k","$100k-$110k","$110k-$120k",
                       "$120k-$130k","$130k-$140k","$140k-$150k",
                       "$150k-$160k","$160k-$170k","$170k-$180k","Over $180k")

assign_broad_band <- function(inc_low) {
  cut(inc_low, breaks=BROAD_INC_BREAKS, labels=BROAD_INC_LABELS,
      right=TRUE, include.lowest=TRUE)
}

section_header <- function(label, colour) {
  div(style=paste0(
    "background:", colour, "; color:white; font-family:Calibri,Arial,sans-serif;",
    "font-weight:700; font-size:0.72em; text-transform:uppercase;",
    "letter-spacing:0.07em; padding:6px 14px; margin:18px -15px 10px -15px;",
    "border-radius:0;"
  ), label)
}


# ================================================================
# CSS
# ================================================================

APP_CSS <- paste0("
  body { font-family: Calibri, Arial, sans-serif; color:", TAWHIRI,";
         background:#f7f9fa; }
  .title-bar { background:", TANGAROA,"; color:white; padding:16px 24px 12px;
               margin-bottom:0; border-bottom:4px solid ", TANIWHA,"; }
  .title-bar h2 { color:white; margin:0 0 3px 0; font-size:1.35em; }
  .title-bar p  { color:", RANGINUI1,"; margin:0; font-size:0.85em; }
  .sidebar-panel { background:white; border:1px solid #dde4ea;
                   border-radius:4px; padding:0 14px 14px 14px;
                   overflow-y:auto; max-height:calc(100vh - 100px); }

  /* Accordion */
  .accordion-item { margin-bottom:2px; }
  .accordion-header {
    background:", TANGAROA, "; color:white; padding:8px 12px;
    cursor:pointer; display:flex; justify-content:space-between;
    align-items:center; user-select:none; margin:0 -14px;
    font-size:0.82em; font-weight:600; letter-spacing:0.03em;
  }
  .accordion-header:hover { background:", TANIWHA, "; }
  .accordion-title { flex:1; }
  .accordion-icon  { font-size:0.75em; margin-left:8px; }
  .accordion-body  { padding:8px 0 4px 0; }

  /* Summary tab cards */
  .summary-section-title {
    font-size:0.72em; font-weight:700; text-transform:uppercase;
    letter-spacing:0.07em; padding:6px 0 4px 0;
    border-bottom:2px solid; margin-bottom:10px;
  }
  .summary-section-title.s2024 { color:#0e3653; border-color:#0e3653; }
  .summary-section-title.s2026 { color:#1d9ba0; border-color:#1d9ba0; }
  .summary-section-title.sscen { color:#7b5ea7; border-color:#7b5ea7; }
  .summary-card {
    background:white; border-radius:4px; padding:10px 14px;
    margin-bottom:8px; border-left:4px solid #ddd;
  }
  .card-neutral   { border-color:", TANIWHA, "; }
  .card-2024      { border-color:", TANGAROA, "; }
  .card-2026      { border-color:", TANIWHA, "; }
  .card-positive  { border-color:#2a7a4b; }
  .card-negative  { border-color:#c0392b; }
  .card-inactive  { border-color:#ccc; background:#f9f9f9; }
  .card-value { font-size:1.45em; font-weight:700; color:", TANGAROA, "; }
  .card-value.positive { color:#2a7a4b; }
  .card-value.negative { color:#c0392b; }
  .card-value.inactive { color:#aaa; }
  .card-label { font-size:0.80em; color:", TAWHIRI, "; line-height:1.3; }
  .card-sub   { font-size:0.74em; color:#888; margin-top:2px; }

  .no-change-note {
    background:#f5f5f5; border:1px dashed #ccc; border-radius:4px;
    padding:10px 14px; color:#888; font-size:0.84em; font-style:italic;
    text-align:center; margin-bottom:8px;
  }
  .divider { border:none; border-top:1px solid #e0e8ed; margin:14px 0 10px 0; }

  /* Other */
  .section-label { font-size:0.72em; font-weight:700; text-transform:uppercase;
                   letter-spacing:.06em; color:", TANIWHA, "; margin:10px 0 5px; }
  .nav-tabs>li.active>a { color:", TANGAROA, ";
                          border-top:3px solid ", TANIWHA, "; font-weight:600; }
  .assumption-box { background:#f0f9fa; border-left:3px solid ", WAIRUA, ";
                    padding:7px 10px; margin:6px 0; font-size:0.80em;
                    border-radius:0 4px 4px 0; }
  .boost-box { background:#fff8f0; border-left:3px solid ", WARM, ";
               padding:7px 10px; margin:6px 0; font-size:0.80em;
               border-radius:0 4px 4px 0; }
  /* Remove the browser's number-input spinner arrows on the GVC amount
     fields. The arrows snap the value onto a step ladder anchored to the
     field's min attribute, which silently moved typed values (e.g. $725
     became $730, and the $260.72 default became $260 or $270). Values are
     typed directly instead. */
  #max_gvc::-webkit-outer-spin-button, #max_gvc::-webkit-inner-spin-button,
  #boost_max_gvc_inc::-webkit-outer-spin-button,
  #boost_max_gvc_inc::-webkit-inner-spin-button,
  #boost_max_gvc_age::-webkit-outer-spin-button,
  #boost_max_gvc_age::-webkit-inner-spin-button {
    -webkit-appearance: none; appearance: none; margin: 0;
  }
  #max_gvc, #boost_max_gvc_inc, #boost_max_gvc_age {
    -moz-appearance: textfield; appearance: textfield;
  }

  .reset-btn { width:100%; margin:10px 0 4px 0; font-size:0.82em; }
  .footnote { font-size:0.76em; color:#888; margin-top:8px; line-height:1.4; }
  table { font-size:0.86em !important; }
  hr { border:none; border-top:1px solid #dde4ea; margin:10px 0; }

  /* Income/Age band tables: label column left, all data columns centred */
  #tbl_income table td:first-child, #tbl_income table th:first-child,
  #tbl_age    table td:first-child, #tbl_age    table th:first-child {
    text-align:left;
  }
  #tbl_income table td:not(:first-child), #tbl_income table th:not(:first-child),
  #tbl_age    table td:not(:first-child), #tbl_age    table th:not(:first-child) {
    text-align:center;
  }
")


# ================================================================
# UI
# ================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML(APP_CSS))),

  div(class="title-bar",
    tags$h2("KiwiSaver Government Contribution - Microsimulation Model"),
    tags$p("Te Ara Ahunga Ora Retirement Commission  |  Internal Policy Tool  |  ",
           "2024 IRD data baseline; 2025 KiwiSaver monitoring data for 2026 simulation")
  ),

  sidebarLayout(
    # ── Sidebar ──────────────────────────────────────────────────────
    sidebarPanel(width=3, class="sidebar-panel",

      div(style="padding:10px 0 6px 0;",
        div(class="section-label", "Policy View"),
        radioButtons("policy", NULL,
          choices=c("2026 simulation"="2026","2024 observed"="2024"),
          selected="2026")
      ),

      conditionalPanel("input.policy == '2026'",

        # Reset button
        actionButton("reset_all", "↺  Reset all to 2026 defaults",
                     class="btn btn-default btn-sm reset-btn"),

        # ── Accordion 1: GVC Settings ───────────────────────────
        accordion_panel("gvc", "GVC Settings", open=TRUE,
          div(class="section-label", "Base parameters"),
          sliderInput("match_rate","Match rate ($ per $1 contributed)",
                      min=0.25, max=2.00, value=DEF_MATCH, step=0.25),
          # min=0 and step=0.01 leave every dollars-and-cents value on the
          # step ladder, so keyboard arrow keys cannot snap a typed figure
          # onto a coarser grid the way min=50/step=10 did.
          numericInput("max_gvc","Max government contribution ($/yr)",
                       value=DEF_MAX_GVC, min=0, step=0.01),
          div(class="section-label", "Minimum contribution rate"),
          sliderInput("min_rate","Member minimum contribution rate (%)",
                      min=2, max=10, value=3.5, step=0.5,
                      post="%"),
          div(class="assumption-box",
            "Shifts the rate applied to members on the minimum contribution ",
            "rate tier (pp3), observed at 3% and modelled at 3.5% under the ",
            "2026 default. Members already on higher rates (4%, 6%, 8%, 10%) ",
            "are unaffected. New uptake members also join at this rate."
          )
        ),

        # ── Accordion 2: Income Settings ────────────────────────
        accordion_panel("income", "Income Settings",
          div(class="section-label", "Eligibility"),
          sliderInput("inc_cap","Income eligibility cap ($)",
                      min=10000, max=180000, value=DEF_INC_CAP,
                      step=10000, pre="$", sep=","),
          checkboxInput("inc_no_cap",
                        "No cap — include income above $180k", value=FALSE),
          div(class="assumption-box",
            "Steps in $10k up to $180k — the finest resolution the data ",
            "supports. \"No cap\" adds the single unresolved \"Over $180k\" ",
            "group (~120k eligible members, valued at an assumed $200k; ",
            "Assumption A9) and overrides the slider."),
          tags$hr(),
          div(class="section-label", "Income targeting"),
          checkboxInput("inc_target_on",
                        "Boost GVC below an income threshold", value=FALSE),
          conditionalPanel("input.inc_target_on == true",
            sliderInput("inc_target_thresh","Income threshold ($)",
                        min=10000, max=180000, value=DEF_INC_THRESH,
                        step=10000, pre="$", sep=","),
            div(class="boost-box",
              "Below threshold: boosted rate/cap. Above: base rate/cap."),
            sliderInput("boost_match_inc","Boosted match rate",
                        min=0.25, max=2.00, value=DEF_MATCH, step=0.25),
            numericInput("boost_max_gvc_inc","Boosted max GVC ($/yr)",
                         value=DEF_MAX_GVC, min=0, step=0.01)
          )
        ),

        # ── Accordion 3: Age Settings ────────────────────────────
        accordion_panel("age", "Age Settings",
          div(class="section-label", "Eligible bands"),
          div(class="assumption-box",
            "Each band is included or excluded in full."),
          checkboxGroupInput("age_bands_selected", NULL,
            choices=c("Under 16"="ks_u16","16-17"="ks_16_17","18-24"="ks_18_24",
                      "25-34"="ks_25_34","35-44"="ks_35_44","45-54"="ks_45_54",
                      "55-64"="ks_55_64","65+"="ks_65p"),
            selected=DEF_AGE_KEYS),
          tags$hr(),
          div(class="section-label", "Age targeting"),
          uiOutput("ui_age_target_checkboxes"),
          conditionalPanel("length(input.age_target_keys) > 0",
            div(class="boost-box",
              "Ticked bands: boosted rate/cap. Remaining eligible: base rate/cap.",
              " Age targeting takes precedence over income targeting."),
            sliderInput("boost_match_age","Boosted match rate",
                        min=0.25, max=2.00, value=DEF_MATCH, step=0.25),
            numericInput("boost_max_gvc_age","Boosted max GVC ($/yr)",
                         value=DEF_MAX_GVC, min=0, step=0.01)
          )
        ),

        # ── Accordion 4: Uptake ──────────────────────────────────
        accordion_panel("uptake", "Non-Contributor Uptake",
          checkboxInput("uptake_weight_on","Different uptake by income",
                        value=FALSE),
          conditionalPanel("input.uptake_weight_on == false",
            sliderInput("uptake_pct","Uptake (%)",
                        min=0, max=100, value=0, step=5),
            div(class="assumption-box",
              "% of non-contributors assumed to start contributing at the ",
              "minimum contribution rate set above (3.5% by default).")
          ),
          conditionalPanel("input.uptake_weight_on == true",
            sliderInput("uptake_thresh","Income threshold ($)",
                        min=10000, max=180000, value=DEF_UPTAKE_THRESH,
                        step=10000, pre="$", sep=","),
            sliderInput("uptake_pct_below","Uptake below threshold (%)",
                        min=0, max=100, value=0, step=5),
            sliderInput("uptake_pct_above","Uptake above threshold (%)",
                        min=0, max=100, value=0, step=5),
            div(class="assumption-box",
              "New contributors in both tiers join at the minimum ",
              "contribution rate set above (3.5% by default).")
          )
        ),

        # ── Accordion 5: Income Growth ───────────────────────────
        accordion_panel("growth", "Income Growth",
          checkboxInput("growth_on","Apply income growth (3.5% p.a.)",
                        value=FALSE),
          conditionalPanel("input.growth_on == true",
            sliderInput("growth_years","Years of growth",
                        min=1, max=10, value=2, step=1),
            div(class="assumption-box",
              "Shifts effective income within each band. Members near ",
              "band boundaries may cross the income cap or GVC cap threshold.")
          )
        )
      ), # end conditionalPanel 2026

      conditionalPanel("input.policy == '2024'",
        div(style="padding:8px 0;",
          div(class="assumption-box",
            "Displaying 2024 observed IRD data. ",
            "No simulation parameters apply."
          )
        )
      )
    ), # end sidebarPanel

    # ── Main panel ────────────────────────────────────────────────────
    mainPanel(width=9,
      tabsetPanel(id="tabs",

        # ── Tab 1: Summary ──────────────────────────────────────────
        # Always shows all three sections regardless of the policy toggle.
        # The toggle only affects the distribution and W&L tabs.
        tabPanel("Summary", br(),

          # ── Section 1: 2024 Observed (always visible, never changes) ──
          div(class="summary-section-title s2024",
              "2024 Observed — Fixed reference (IRD data)"),
          fluidRow(
            column(3, metric_card(fmt_dol(OBS_GVC_2024),
              "Total GVC paid", "Source: IRD 2024", "card-2024")),
            column(3, metric_card(fmt_n(OBS_MEMBERS_2024),
              "Total members", "30 June 2024", "card-2024")),
            column(3, metric_card("$521.43 cap  |  $0.50 match",
              "2024 policy settings", "Age 18–65, no income cap", "card-2024")),
            column(3, metric_card("$305.35",
              "Average GVC per member", "Across all members", "card-2024"))
          ),
          fluidRow(
            column(3, metric_card("1,711,285",
              "Members received full GVC", "51% of total", "card-2024")),
            column(3, metric_card("519,626",
              "Members received partial GVC", "16% of total", "card-2024")),
            column(3, metric_card("1,117,137",
              "Members received no GVC", "33% of total", "card-2024")),
            column(3, metric_card("$34,762",
              "Income at which 2024 cap was reached", "At 3% contribution rate", "card-2024"))
          ),

          tags$hr(class="divider"),

          # ── Section 2: 2026 Default Baseline (always visible, never changes) ──
          div(class="summary-section-title s2026",
              "2026 Default Baseline — Fixed (no scenario changes)"),
          fluidRow(
            column(3, uiOutput("ui_2026_cost")),
            column(3, uiOutput("ui_2026_vs_2024")),
            column(3, metric_card("$260.72 cap  |  $0.25 match",
              "2026 default policy", "Ages 16–64, $180k income cap", "card-2026")),
            column(3, metric_card("$29,797",
              "Income at which cap is reached", "At 3.5% contribution rate", "card-2026"))
          ),
          fluidRow(
            column(3, metric_card(fmt_n(sum(ds26$members)),
              "Total members", "Model's simulated population", "card-2026")),
            column(3, uiOutput("ui_2026_contributing")),
            column(3, uiOutput("ui_2026_noncontrib")),
            column(3, uiOutput("ui_2026_excl_income"))
          ),

          tags$hr(class="divider"),

          # ── Section 3: Your Scenario (reactive, 2026 mode only) ─────
          div(class="summary-section-title sscen",
              "Your Scenario — Adjustments from 2026 default"),
          conditionalPanel("input.policy == '2024'",
            div(class="no-change-note",
              "Switch to 2026 simulation in the sidebar to model policy scenarios."
            )
          ),
          conditionalPanel("input.policy == '2026'",
            uiOutput("ui_scenario_section")
          )
        ), # end Summary tab

        # ── Tab 2: By Income Band ───────────────────────────────────
        tabPanel("By Income Band", br(),
          fluidRow(
            column(8, plotOutput("plot_income_cost",    height="360px")),
            column(4, plotOutput("plot_income_members", height="360px"))
          ),
          br(),
          tableOutput("tbl_income"),
          tags$p(class="footnote",
            "2024: 8 coarse income bands from IRD data. ",
            "2026: 19 reporting bands, running in uniform $10,000 steps to $180,000 plus a ",
            "single over-$180k group, aggregated from the IRD $1,000-wide taxable income distribution. ",
            "Income bands above $180,000 receive $0 GVC under 2026 settings. ",
            "When income targeting is active, the threshold boundary is reflected in per-band costs.")
        ),

        # ── Tab 3: By Age Band ──────────────────────────────────────
        tabPanel("By Age Band", br(),
          fluidRow(
            column(8, plotOutput("plot_age_cost",    height="360px")),
            column(4, plotOutput("plot_age_members", height="360px"))
          ),
          br(),
          tableOutput("tbl_age"),
          tags$p(class="footnote",
            "2024: 00-17 and 65+ bands have low avg GVC reflecting pro-rata eligibility. ",
            "2026: under-16 and 65+ bands receive $0 GVC. ",
            "16-17 band newly eligible under 2026 policy. ",
            "When age targeting is active, boosted bands show higher cost than the default line.")
        ),

        # ── Tab 4: Distributional Impacts ─────────────────────────────────
        tabPanel("Distributional Impacts", br(),
          conditionalPanel("input.policy == '2026'",
            tags$p(style="font-size:0.87em;color:#666;",
              "Change vs the 2026 default policy (25c match, $260.72 cap, ",
              "$180k income cap, 16-17 through 55-64, no uptake, no targeting). ",
              "Green = higher cost than default; Red = lower cost than default."),

            tags$h4(style=paste0("color:#0e3653;font-size:1em;",
                                  "margin-bottom:6px;margin-top:4px;"),
                    "By income band"),
            fluidRow(
              column(8, plotOutput("plot_wl_income", height="340px")),
              column(4, plotOutput("plot_wl_age",    height="340px"))
            ),
            br(),
            tableOutput("tbl_wl"),

            tags$hr(),
            tags$h4(style=paste0("color:#0e3653;font-size:1em;",
                                  "margin-bottom:6px;"),
                    "By age band"),
            tableOutput("tbl_wl_age")
          ),
          conditionalPanel("input.policy == '2024'",
            tags$p("Switch to 2026 simulation to view Distributional Impacts analysis.")
          )
        ),

        # ── Tab 5: Validation ───────────────────────────────────────
        tabPanel("Model Validation", br(),
          verbatimTextOutput("validation_text")
        )
      )
    )
  )
)


# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {

  # ── Reset button ───────────────────────────────────────────────────
  observeEvent(input$reset_all, {
    updateSliderInput(session,  "match_rate",        value=DEF_MATCH)
    updateNumericInput(session, "max_gvc",            value=DEF_MAX_GVC)
    updateSliderInput(session,  "min_rate",           value=3.5)
    updateSliderInput(session,  "inc_cap",            value=DEF_INC_CAP)
    updateCheckboxInput(session,"inc_no_cap",         value=FALSE)
    updateCheckboxInput(session,"inc_target_on",      value=FALSE)
    updateSliderInput(session,  "inc_target_thresh",  value=DEF_INC_THRESH)
    updateSliderInput(session,  "boost_match_inc",    value=DEF_MATCH)
    updateNumericInput(session, "boost_max_gvc_inc",  value=DEF_MAX_GVC)
    updateCheckboxGroupInput(session, "age_bands_selected", selected=DEF_AGE_KEYS)
    updateCheckboxGroupInput(session, "age_target_keys",    selected=character(0))
    updateSliderInput(session,  "boost_match_age",    value=DEF_MATCH)
    updateNumericInput(session, "boost_max_gvc_age",  value=DEF_MAX_GVC)
    updateCheckboxInput(session,"uptake_weight_on",   value=FALSE)
    updateSliderInput(session,  "uptake_pct",         value=0)
    updateSliderInput(session,  "uptake_thresh",      value=DEF_UPTAKE_THRESH)
    updateSliderInput(session,  "uptake_pct_below",   value=0)
    updateSliderInput(session,  "uptake_pct_above",   value=0)
    updateCheckboxInput(session,"growth_on",          value=FALSE)
    updateSliderInput(session,  "growth_years",       value=2)
  })

  # ── Dynamic age targeting checkboxes ──────────────────────────────
  output$ui_age_target_checkboxes <- renderUI({
    selected_bands <- input$age_bands_selected
    if (is.null(selected_bands) || length(selected_bands)==0)
      return(tags$p(style="font-size:0.80em;color:#888;",
                    "Select eligible bands above to enable age targeting."))
    available <- setNames(AGE_BAND_MAP$key, AGE_BAND_MAP$label)
    available  <- available[available %in% selected_bands]
    checkboxGroupInput("age_target_keys",
                       "Boost these bands (subset of eligible):",
                       choices=available, selected=character(0))
  })

  # ── Scenario helper ────────────────────────────────────────────────
  run_scenario <- function() {
    req(input$policy=="2026", !is.null(input$age_bands_selected))
    age_targets <- if (!is.null(input$age_target_keys) &&
                       length(input$age_target_keys)>0)
                     input$age_target_keys else character(0)
    calc_gvc_2026(
      data=ds26, match_rate=input$match_rate, max_gvc=input$max_gvc,
      inc_cap=if (isTRUE(input$inc_no_cap)) Inf else input$inc_cap,
      elig_age_keys=input$age_bands_selected,
      min_rate        = input$min_rate / 100,
      uptake_pct      = if (!input$uptake_weight_on) input$uptake_pct else 0,
      growth_rate     = if (input$growth_on) 0.035 else 0,
      growth_years    = if (input$growth_on) input$growth_years else 0,
      inc_target_on   = input$inc_target_on,
      inc_target_thresh = if (input$inc_target_on) input$inc_target_thresh else 0,
      boost_match_inc = if (input$inc_target_on) input$boost_match_inc else NULL,
      boost_max_gvc_inc = if (input$inc_target_on) input$boost_max_gvc_inc else NULL,
      age_target_keys = age_targets,
      boost_match_age = if (length(age_targets)>0) input$boost_match_age else NULL,
      boost_max_gvc_age = if (length(age_targets)>0) input$boost_max_gvc_age else NULL,
      uptake_weight_on = input$uptake_weight_on,
      uptake_thresh   = if (input$uptake_weight_on) input$uptake_thresh else 0,
      uptake_pct_below = if (input$uptake_weight_on) input$uptake_pct_below else 0,
      uptake_pct_above = if (input$uptake_weight_on) input$uptake_pct_above else 0
    )
  }

  scen_df <- reactive({ run_scenario() })

  default_df <- reactive({
    calc_gvc_2026(data=ds26, match_rate=DEF_MATCH, max_gvc=DEF_MAX_GVC,
                  inc_cap=DEF_INC_CAP, elig_age_keys=DEF_AGE_KEYS,
                  min_rate=DEF_MIN_RATE,
                  uptake_pct=0, growth_rate=0, growth_years=0)
  })

  scen_cost    <- reactive(sum(scen_df()$total_gvc,    na.rm=TRUE))
  default_cost <- reactive(sum(default_df()$total_gvc, na.rm=TRUE))
  # Recipients = contributing members who are both age- and income-eligible.
  scen_recipients    <- reactive(sum(scen_df()$members_eff    * (1 - scen_df()$ppn_eff),
                                     na.rm=TRUE))
  default_recipients <- reactive(sum(default_df()$members_eff * (1 - default_df()$ppn_eff),
                                     na.rm=TRUE))

  # ── Check if scenario equals the default ──────────────────────────
  at_default <- reactive({
    req(!is.null(input$age_bands_selected))
    age_tgt <- if (!is.null(input$age_target_keys)) input$age_target_keys else character(0)
    isTRUE(
      abs(input$match_rate - DEF_MATCH)     < 0.001 &&
      abs(input$max_gvc   - DEF_MAX_GVC)   < 0.01  &&
      abs(input$min_rate  - 3.5)            < 0.01  &&
      abs(input$inc_cap   - DEF_INC_CAP)   < 1     &&
      !isTRUE(input$inc_no_cap)                    &&
      setequal(input$age_bands_selected, DEF_AGE_KEYS) &&
      !input$inc_target_on &&
      length(age_tgt) == 0 &&
      !input$uptake_weight_on &&
      input$uptake_pct == 0 &&
      !input$growth_on
    )
  })

  # ── Section 2: fixed 2026 default metrics ─────────────────────────
  output$ui_2026_cost <- renderUI({
    metric_card(fmt_m(default_cost()),
                "2026 default GVC cost",
                "Default policy, no scenario changes", "card-2026")
  })
  output$ui_2026_vs_2024 <- renderUI({
    chg <- default_cost() - OBS_GVC_2024
    change_card(chg, "Change vs 2024 observed",
                paste0(round(chg/OBS_GVC_2024*100,1), "% from $1,022m"))
  })

  # ── Section 2: member composition (reactive — replaces old hardcoded
  #    "2,147,110" / "786,358" / "125,497" strings so these stay correct
  #    if dataset_2026.csv is ever rebuilt) ─────────────────────────────
  default_elig_stats <- reactive({
    d    <- default_df()
    elig <- d[d$age_eligible & d$inc_eligible, ]
    total_elig   <- sum(elig$members_base, na.rm=TRUE)
    contributing <- sum(elig$members_base * (1 - elig$ppn), na.rm=TRUE)
    noncontrib   <- total_elig - contributing
    excl_income  <- sum(d$members_base[d$age_eligible & !d$inc_eligible], na.rm=TRUE)
    total_all    <- sum(d$members_base, na.rm=TRUE)
    list(total_elig=total_elig, contributing=contributing, noncontrib=noncontrib,
         excl_income=excl_income, total_all=total_all)
  })
  output$ui_2026_contributing <- renderUI({
    s <- default_elig_stats()
    metric_card(fmt_n(s$contributing), "Contributing members (eligible)",
                paste0(round(s$contributing/s$total_elig*100,1), "% of eligible population"),
                "card-2026")
  })
  output$ui_2026_noncontrib <- renderUI({
    s <- default_elig_stats()
    metric_card(fmt_n(s$noncontrib), "Non-contributing (eligible)",
                paste0(round(s$noncontrib/s$total_elig*100,1), "% of eligible population"),
                "card-2026")
  })
  output$ui_2026_excl_income <- renderUI({
    s <- default_elig_stats()
    metric_card(fmt_n(s$excl_income), "Excluded: income above $180k",
                paste0(round(s$excl_income/s$total_all*100,1), "% of total members"),
                "card-2026")
  })

  # ── Section 3: scenario metrics (reactive) ─────────────────────────
  output$ui_scenario_section <- renderUI({
    if (at_default()) {
      return(div(class="no-change-note",
        "No changes from the 2026 default. Adjust the policy parameters ",
        "in the sidebar to see your scenario results here."
      ))
    }

    chg_vs_default <- scen_cost() - default_cost()
    new_contrib    <- sum(scen_df()$members_eff * scen_df()$ppn *
                         (scen_df()$eff_uptake/100), na.rm=TRUE)
    scen_rec        <- scen_recipients()
    def_rec         <- default_recipients()
    chg_coverage    <- scen_rec - def_rec
    avg_per_contrib <- if (scen_rec > 0) scen_cost()/scen_rec else 0
    # Reachability of the maximum. eff_match and eff_cap already carry any
    # income or age targeting, so the enhanced tier is reflected automatically.
    # Restrict to the group receiving the enhanced contribution, since that is
    # the group a targeted design is meant to reach. With no targeting active
    # this is simply every eligible contributor.
    tgt <- scen_df()
    if (isTRUE(input$inc_target_on)) {
      tgt <- tgt[tgt$inc_eff < input$inc_target_thresh, ]
    } else if (length(input$age_target_keys) > 0) {
      tgt <- tgt[tgt$age_band_key %in% input$age_target_keys, ]
    }
    reach   <- at_max_share(tgt)
    pct_max <- if (reach$total > 0) reach$at_max/reach$total*100 else 0
    eff_m   <- if (isTRUE(input$inc_target_on)) input$boost_match_inc else
               if (length(input$age_target_keys) > 0) input$boost_match_age else
               input$match_rate
    eff_c   <- if (isTRUE(input$inc_target_on)) input$boost_max_gvc_inc else
               if (length(input$age_target_keys) > 0) input$boost_max_gvc_age else
               input$max_gvc
    targeted <- isTRUE(input$inc_target_on) || length(input$age_target_keys) > 0

    # Build a text summary of what's been changed
    changes <- c()
    if (abs(input$match_rate - DEF_MATCH) > 0.001)
      changes <- c(changes, paste0("Match rate: $", input$match_rate))
    if (abs(input$min_rate - 3.5) > 0.01)
      changes <- c(changes, paste0("Min contribution rate: ", input$min_rate, "%"))
    if (abs(input$max_gvc - DEF_MAX_GVC) > 0.01)
      changes <- c(changes, paste0("Max GVC: $", input$max_gvc))
    if (isTRUE(input$inc_no_cap))
      changes <- c(changes, "Income cap: none (incl. >$180k)")
    else if (abs(input$inc_cap - DEF_INC_CAP) > 1)
      changes <- c(changes, paste0("Income cap: $", format(input$inc_cap, big.mark=",")))
    if (!setequal(input$age_bands_selected, DEF_AGE_KEYS))
      changes <- c(changes, "Age eligibility changed")
    if (input$inc_target_on)
      changes <- c(changes, paste0("Income boost below $",
                   format(input$inc_target_thresh, big.mark=",")))
    age_tgt <- if (!is.null(input$age_target_keys)) input$age_target_keys else character(0)
    if (length(age_tgt)>0)
      changes <- c(changes, paste0("Age boost: ",
                   paste(AGE_BAND_MAP$label[AGE_BAND_MAP$key %in% age_tgt], collapse=", ")))
    if (input$uptake_weight_on || input$uptake_pct>0)
      changes <- c(changes, "Non-contributor uptake applied")
    if (input$growth_on)
      changes <- c(changes, paste0("Income growth: 3.5% × ", input$growth_years, " yrs"))

    sign_str <- if (chg_vs_default>=0) "+" else ""
    sign_v   <- if (chg_vs_default>=0) "positive" else "negative"
    cls_chg  <- if (chg_vs_default>=0) "card-positive" else "card-negative"
    sign_cov <- if (chg_coverage>=0) "+" else ""
    cov_v    <- if (chg_coverage>=0) "positive" else "negative"
    cls_cov  <- if (chg_coverage>=0) "card-positive" else "card-negative"

    tagList(
      # Change summary badge
      div(style="font-size:0.80em;color:#666;margin-bottom:8px;",
        tags$b("Active changes: "),
        paste(changes, collapse=" | ")
      ),
      fluidRow(
        column(3,
          div(class=paste("summary-card", "card-neutral"),
            div(class="card-value", fmt_m(scen_cost())),
            div(class="card-label", "Scenario GVC cost"),
            div(class="card-sub",   "Your current settings")
          )
        ),
        column(3,
          div(class=paste("summary-card", cls_chg),
            div(class=paste("card-value", sign_v),
                paste0(sign_str, fmt_m(chg_vs_default))),
            div(class="card-label", "Change vs 2026 default"),
            div(class="card-sub",
                paste0(sign_str, round(chg_vs_default/default_cost()*100,1), "%"))
          )
        ),
        column(3,
          div(class="summary-card card-neutral",
            div(class="card-value", sprintf("$%.2f", avg_per_contrib)),
            div(class="card-label", "Average GVC per contributing member"),
            div(class="card-sub",
                paste0("Across ", fmt_n(scen_rec), " members receiving a contribution"))
          )
        ),
        column(3,
          div(class=paste("summary-card", cls_cov),
            div(class=paste("card-value", cov_v),
                paste0(sign_cov, fmt_n(chg_coverage))),
            div(class="card-label", "Change in coverage"),
            div(class="card-sub",
                paste0(sign_cov,
                       if (def_rec>0) round(chg_coverage/def_rec*100,1) else 0,
                       "% vs 2026 default"))
          )
        )
      ),
      fluidRow(
        column(3,
          if (eff_m > 0) {
            thresh <- eff_c / ((input$min_rate/100) * eff_m)
            div(class="summary-card card-neutral",
              div(class="card-value", fmt_dol(thresh)),
              div(class="card-label",
                  paste0("Income needed to reach the maximum (", input$min_rate, "% rate)")),
              div(class="card-sub",
                  if (targeted) "Applies to the enhanced group; others keep the base settings"
                  else "Income at which the full maximum is reached")
            )
          } else {
            div(class="summary-card card-inactive",
              div(class="card-value inactive", "N/A"),
              div(class="card-label", "No match rate set")
            )
          }
        ),
        column(3,
          div(class="summary-card card-neutral",
            div(class="card-value", paste0(round(pct_max,1), "%")),
            div(class="card-label",
                if (targeted) "Enhanced group receiving the full maximum"
                else "Contributors receiving the full maximum"),
            div(class="card-sub",
                paste0(fmt_n(reach$at_max), " of ", fmt_n(reach$total),
                       " members, across all contribution rates"))
          )
        ),
        column(3,
          if (new_contrib > 0) {
            div(class="summary-card card-neutral",
              div(class="card-value", fmt_n(new_contrib)),
              div(class="card-label", "New contributors (uptake)"),
              div(class="card-sub",   paste0("Members newly contributing at ", input$min_rate, "%"))
            )
          }
        )
      )
    )
  })

  # ── Aggregation helpers ────────────────────────────────────────────
  agg_income_2026 <- reactive({
    scen_df() %>%
      mutate(broad=assign_broad_band(inc_low),
             contributing_eff = members_eff * (1 - ppn_eff)) %>%
      group_by(broad) %>%
      summarise(members=sum(members_eff,na.rm=TRUE),
                contributing=sum(contributing_eff,na.rm=TRUE),
                gvc=sum(total_gvc,na.rm=TRUE),.groups="drop") %>%
      mutate(broad=factor(broad,levels=BROAD_INC_LABELS)) %>% arrange(broad)
  })
  agg_income_default <- reactive({
    default_df() %>%
      mutate(broad=assign_broad_band(inc_low),
             contributing_eff = members_eff * (1 - ppn_eff)) %>%
      group_by(broad) %>%
      summarise(contributing_def=sum(contributing_eff,na.rm=TRUE),
                gvc=sum(total_gvc,na.rm=TRUE),.groups="drop") %>%
      mutate(broad=factor(broad,levels=BROAD_INC_LABELS)) %>% arrange(broad)
  })
  agg_age_2026 <- reactive({
    scen_df() %>%
      mutate(contributing_eff = members_eff * (1 - ppn_eff)) %>%
      group_by(age_band) %>%
      summarise(members=sum(members_eff,na.rm=TRUE),
                contributing=sum(contributing_eff,na.rm=TRUE),
                gvc=sum(total_gvc,na.rm=TRUE),.groups="drop") %>%
      mutate(age_band=factor(age_band,levels=AGE_LEVELS_26)) %>% arrange(age_band)
  })
  agg_age_default <- reactive({
    default_df() %>%
      mutate(contributing_eff = members_eff * (1 - ppn_eff)) %>%
      group_by(age_band) %>%
      summarise(contributing_def=sum(contributing_eff,na.rm=TRUE),
                gvc=sum(total_gvc,na.rm=TRUE),.groups="drop") %>%
      mutate(age_band=factor(age_band,levels=AGE_LEVELS_26)) %>% arrange(age_band)
  })

  # ── Tab 2: Income charts ───────────────────────────────────────────
  output$plot_income_cost <- renderPlot({
    if (input$policy=="2024") {
      bar_chart(inc_2024$total_gvc_2024/1e6,
                c("$1-$20k","$20k-$40k","$40k-$60k","$60k-$80k",
                  "$80k-$100k","$100k-$120k","$120k+","No info"),
                "Annual GVC cost by income band — 2024 observed",
                "GVC ($m)", rep(TANIWHA,8))
    } else {
      d    <- agg_income_2026(); def_d <- agg_income_default()
      vals <- d$gvc/1e6;         def_v <- def_d$gvc/1e6
      bar_chart(vals, as.character(d$broad),
                "Annual GVC cost by income band — 2026 scenario",
                "GVC ($m)", ifelse(vals>=def_v,TANIWHA,"#e07b54"),
                ref_values=def_v, ref_label="2026 default")
    }
  })
  output$plot_income_members <- renderPlot({
    if (input$policy=="2024") {
      vals   <- inc_2024$members/1000
      groups <- c("$1-$20k","$20k-$40k","$40k-$60k","$60k-$80k",
                  "$80k-$100k","$100k-$120k","$120k+","No info")
    } else {
      d <- agg_income_2026(); vals <- d$members/1000; groups <- as.character(d$broad)
    }
    ymax <- max(vals,na.rm=TRUE)*1.2
    par(mar=c(8.5,5,3,1),bg="#f7f9fa")
    bp <- barplot(vals,names.arg=groups,las=2,col=WAIRUA,border=NA,
                  ylim=c(0,ymax),ylab="Members (thousands)",main="Members by income band",
                  cex.names=0.75,cex.axis=0.85,col.axis=TAWHIRI,col.lab=TANGAROA,col.main=TANGAROA)
    text(bp,vals+ymax*0.015,ifelse(vals>0,paste0(round(vals),"k"),""),
         cex=0.68,col=TANGAROA,adj=c(0.5,0))
  })
  output$tbl_income <- renderTable({
    if (input$policy=="2024") {
      inc_2024 %>% arrange(income_band) %>%
        transmute(`Income band`=as.character(income_band),
                  Members=format(members,big.mark=","),
                  `Avg GVC ($/member)`=paste0("$",round(avg_gvc_2024,2)),
                  `Total GVC`=paste0("$",format(round(total_gvc_2024),big.mark=",")),
                  `% of total`=paste0(round(total_gvc_2024/OBS_GVC_2024*100,1),"%"))
    } else {
      d <- left_join(agg_income_2026(),
                     agg_income_default() %>% select(broad, contributing_def),
                     by="broad") %>%
             mutate(cov_chg = contributing - contributing_def)
      tot <- sum(d$gvc)
      body <- d %>% transmute(`Income band`=as.character(broad),
                      `Total eligible members`=format(round(members),big.mark=","),
                      `Contributing members`=format(round(contributing),big.mark=","),
                      `Non-contributing members`=format(round(members-contributing),big.mark=","),
                      `Change in contributors`=fmt_signed(cov_chg),
                      `GVC ($m)`=sprintf("%.2f",gvc/1e6),
                      `Avg GVC/total member`=ifelse(members>0,sprintf("$%.2f",gvc/members),"$0.00"),
                      `Avg GVC/contributor`=ifelse(contributing>0,sprintf("$%.2f",gvc/contributing),"$0.00"),
                      `% of total`=paste0(round(gvc/tot*100,1),"%"))
      totals <- data.frame(
        `Income band`="TOTAL",
        `Total eligible members`=format(round(sum(d$members)),big.mark=","),
        `Contributing members`=format(round(sum(d$contributing)),big.mark=","),
        `Non-contributing members`=format(round(sum(d$members)-sum(d$contributing)),big.mark=","),
        `Change in contributors`=fmt_signed(sum(d$cov_chg)),
        `GVC ($m)`=sprintf("%.2f",tot/1e6),
        `Avg GVC/total member`=ifelse(sum(d$members)>0,sprintf("$%.2f",tot/sum(d$members)),"$0.00"),
        `Avg GVC/contributor`=ifelse(sum(d$contributing)>0,sprintf("$%.2f",tot/sum(d$contributing)),"$0.00"),
        `% of total`="100.0%",
        check.names=FALSE, stringsAsFactors=FALSE)
      bind_rows(body, totals)
    }
  }, striped=TRUE, hover=TRUE, bordered=FALSE)

  # ── Tab 3: Age charts ──────────────────────────────────────────────
  output$plot_age_cost <- renderPlot({
    if (input$policy=="2024") {
      bar_chart(age_2024$total_gvc_2024/1e6, as.character(age_2024$age_band),
                "Annual GVC cost by age band — 2024 observed",
                "GVC ($m)", rep(TANIWHA,nrow(age_2024)))
    } else {
      d    <- agg_age_2026(); def_d <- agg_age_default()
      vals <- d$gvc/1e6;      def_v <- def_d$gvc/1e6
      bar_chart(vals, as.character(d$age_band),
                "Annual GVC cost by age band — 2026 scenario",
                "GVC ($m)", ifelse(vals>=def_v,TANIWHA,"#e07b54"),
                ref_values=def_v, ref_label="2026 default")
    }
  })
  output$plot_age_members <- renderPlot({
    if (input$policy=="2024") {
      vals <- age_2024$members/1000; groups <- as.character(age_2024$age_band)
    } else {
      d <- agg_age_2026(); vals <- d$members/1000; groups <- as.character(d$age_band)
    }
    ymax <- max(vals,na.rm=TRUE)*1.2
    par(mar=c(6,5,3,1),bg="#f7f9fa")
    bp <- barplot(vals,names.arg=groups,las=2,col=WAIRUA,border=NA,
                  ylim=c(0,ymax),ylab="Members (thousands)",main="Members by age band",
                  cex.names=0.85,cex.axis=0.85,col.axis=TAWHIRI,col.lab=TANGAROA,col.main=TANGAROA)
    text(bp,vals+ymax*0.015,ifelse(vals>0,paste0(round(vals),"k"),""),
         cex=0.72,col=TANGAROA,adj=c(0.5,0))
  })
  output$tbl_age <- renderTable({
    if (input$policy=="2024") {
      age_2024 %>% arrange(age_band) %>%
        transmute(`Age band`=as.character(age_band),
                  Members=format(members,big.mark=","),
                  `Avg GVC ($/member)`=paste0("$",round(avg_gvc_2024,2)),
                  `Total GVC`=paste0("$",format(round(total_gvc_2024),big.mark=",")),
                  `% of total`=paste0(round(total_gvc_2024/OBS_GVC_2024*100,1),"%"))
    } else {
      d <- left_join(agg_age_2026(),
                     agg_age_default() %>% select(age_band, contributing_def),
                     by="age_band") %>%
             mutate(cov_chg = contributing - contributing_def)
      tot <- sum(d$gvc)
      body <- d %>% transmute(`Age band`=as.character(age_band),
                      `Total eligible members`=format(round(members),big.mark=","),
                      `Contributing members`=format(round(contributing),big.mark=","),
                      `Non-contributing members`=format(round(members-contributing),big.mark=","),
                      `Change in contributors`=fmt_signed(cov_chg),
                      `GVC ($m)`=sprintf("%.2f",gvc/1e6),
                      `Avg GVC/total member`=ifelse(members>0,sprintf("$%.2f",gvc/members),"$0.00"),
                      `Avg GVC/contributor`=ifelse(contributing>0,sprintf("$%.2f",gvc/contributing),"$0.00"),
                      `% of total`=paste0(round(gvc/tot*100,1),"%"))
      totals <- data.frame(
        `Age band`="TOTAL",
        `Total eligible members`=format(round(sum(d$members)),big.mark=","),
        `Contributing members`=format(round(sum(d$contributing)),big.mark=","),
        `Non-contributing members`=format(round(sum(d$members)-sum(d$contributing)),big.mark=","),
        `Change in contributors`=fmt_signed(sum(d$cov_chg)),
        `GVC ($m)`=sprintf("%.2f",tot/1e6),
        `Avg GVC/total member`=ifelse(sum(d$members)>0,sprintf("$%.2f",tot/sum(d$members)),"$0.00"),
        `Avg GVC/contributor`=ifelse(sum(d$contributing)>0,sprintf("$%.2f",tot/sum(d$contributing)),"$0.00"),
        `% of total`="100.0%",
        check.names=FALSE, stringsAsFactors=FALSE)
      bind_rows(body, totals)
    }
  }, striped=TRUE, hover=TRUE, bordered=FALSE)

  # ── Tab 4: Distributional Impacts ────────────────────────────────────────
  wl_income <- reactive({
    req(input$policy=="2026")
    left_join(agg_income_2026(),
              agg_income_default() %>% rename(gvc_def=gvc), by="broad") %>%
      mutate(diff=gvc-gvc_def, cov_chg=contributing-contributing_def)
  })
  wl_age <- reactive({
    req(input$policy=="2026")
    left_join(agg_age_2026(),
              agg_age_default() %>% rename(gvc_def=gvc), by="age_band") %>%
      mutate(diff=gvc-gvc_def, cov_chg=contributing-contributing_def)
  })
  output$plot_wl_income <- renderPlot({
    d <- wl_income(); vals <- d$diff/1e6
    ymax <- max(abs(vals),na.rm=TRUE)*1.3; if(is.na(ymax)||ymax==0) ymax<-1
    par(mar=c(8.5,5.5,3.5,1.5),bg="#f7f9fa")
    bp <- barplot(vals,names.arg=as.character(d$broad),las=2,
                  col=ifelse(vals>=0,"#2a7a4b","#c0392b"),border=NA,
                  ylim=c(-ymax,ymax),ylab="Change vs 2026 default ($m)",
                  main="Distributional Impacts by income band (vs 2026 default)",
                  cex.names=0.75,cex.axis=0.85,col.axis=TAWHIRI,col.lab=TANGAROA,col.main=TANGAROA)
    abline(h=0,col=TAWHIRI,lwd=1.2,lty=2)
    nz <- which(abs(vals)>ymax*0.03)
    if(length(nz)>0) text(bp[nz],vals[nz]+sign(vals[nz])*ymax*0.06,
      paste0(ifelse(vals[nz]>=0,"+",""),round(vals[nz],1),"m"),
      cex=0.68,col=TANGAROA,adj=c(0.5,0.5))
  })
  output$plot_wl_age <- renderPlot({
    d <- wl_age(); vals <- d$diff/1e6
    ymax <- max(abs(vals),na.rm=TRUE)*1.3; if(is.na(ymax)||ymax==0) ymax<-1
    par(mar=c(6,5.5,3.5,1.5),bg="#f7f9fa")
    bp <- barplot(vals,names.arg=as.character(d$age_band),las=2,
                  col=ifelse(vals>=0,"#2a7a4b","#c0392b"),border=NA,
                  ylim=c(-ymax,ymax),ylab="Change vs 2026 default ($m)",
                  main="By age band",
                  cex.names=0.85,cex.axis=0.85,col.axis=TAWHIRI,col.lab=TANGAROA,col.main=TANGAROA)
    abline(h=0,col=TAWHIRI,lwd=1.2,lty=2)
    nz <- which(abs(vals)>ymax*0.03)
    if(length(nz)>0) text(bp[nz],vals[nz]+sign(vals[nz])*ymax*0.07,
      paste0(ifelse(vals[nz]>=0,"+",""),round(vals[nz],1),"m"),
      cex=0.72,col=TANGAROA,adj=c(0.5,0.5))
  })
  output$tbl_wl <- renderTable({
    req(input$policy=="2026")
    d <- wl_income()
    body <- d %>%
      transmute(`Income band`=as.character(broad),
                `Scenario GVC ($m)`=sprintf("%.2f",gvc/1e6),
                `Default GVC ($m)`=sprintf("%.2f",gvc_def/1e6),
                `Change ($m)`=sprintf("%+.2f",diff/1e6),
                `Change (%)`=ifelse(gvc_def>0,paste0(round(diff/gvc_def*100,1),"%"),"-"),
                `Scenario contributors`=format(round(contributing),big.mark=","),
                `Default contributors`=format(round(contributing_def),big.mark=","),
                `Change in coverage`=fmt_signed(cov_chg))
    totals <- data.frame(
      `Income band`="TOTAL",
      `Scenario GVC ($m)`=sprintf("%.2f",sum(d$gvc)/1e6),
      `Default GVC ($m)`=sprintf("%.2f",sum(d$gvc_def)/1e6),
      `Change ($m)`=sprintf("%+.2f",sum(d$diff)/1e6),
      `Change (%)`=ifelse(sum(d$gvc_def)>0,
                          paste0(round(sum(d$diff)/sum(d$gvc_def)*100,1),"%"),"-"),
      `Scenario contributors`=format(round(sum(d$contributing)),big.mark=","),
      `Default contributors`=format(round(sum(d$contributing_def)),big.mark=","),
      `Change in coverage`=fmt_signed(sum(d$cov_chg)),
      check.names=FALSE, stringsAsFactors=FALSE)
    bind_rows(body, totals)
  }, striped=TRUE, hover=TRUE, bordered=FALSE)

  output$tbl_wl_age <- renderTable({
    req(input$policy=="2026")
    d <- wl_age()
    body <- d %>%
      transmute(`Age band`=as.character(age_band),
                `Scenario GVC ($m)`=sprintf("%.2f",gvc/1e6),
                `Default GVC ($m)`=sprintf("%.2f",gvc_def/1e6),
                `Change ($m)`=sprintf("%+.2f",diff/1e6),
                `Change (%)`=ifelse(gvc_def>0,paste0(round(diff/gvc_def*100,1),"%"),"-"),
                `Scenario contributors`=format(round(contributing),big.mark=","),
                `Default contributors`=format(round(contributing_def),big.mark=","),
                `Change in coverage`=fmt_signed(cov_chg))
    totals <- data.frame(
      `Age band`="TOTAL",
      `Scenario GVC ($m)`=sprintf("%.2f",sum(d$gvc)/1e6),
      `Default GVC ($m)`=sprintf("%.2f",sum(d$gvc_def)/1e6),
      `Change ($m)`=sprintf("%+.2f",sum(d$diff)/1e6),
      `Change (%)`=ifelse(sum(d$gvc_def)>0,
                          paste0(round(sum(d$diff)/sum(d$gvc_def)*100,1),"%"),"-"),
      `Scenario contributors`=format(round(sum(d$contributing)),big.mark=","),
      `Default contributors`=format(round(sum(d$contributing_def)),big.mark=","),
      `Change in coverage`=fmt_signed(sum(d$cov_chg)),
      check.names=FALSE, stringsAsFactors=FALSE)
    bind_rows(body, totals)
  }, striped=TRUE, hover=TRUE, bordered=FALSE)

  # ── Tab 5: Validation ──────────────────────────────────────────────
  output$validation_text <- renderText({
    def_gvc <- sum(default_df()$total_gvc,na.rm=TRUE)
    paste0(
      "=== MODEL VALIDATION ===\n\n",
      "── 2024 baseline ──\n",
      "  Income table GVC: $",format(round(sum(inc_2024$total_gvc_2024)),big.mark=","),"\n",
      "  Age table GVC:    $",format(round(sum(age_2024$total_gvc_2024)),big.mark=","),"\n",
      "  IRD observed GVC: $",format(OBS_GVC_2024,big.mark=","),"\n\n",
      "── 2026 simulation grid ──\n",
      "  Rows:         ",nrow(ds26)," (",length(unique(ds26$income_band)),
        " income bands × ",length(unique(ds26$age_band_key))," age bands)\n",
      "  Total members:",format(round(sum(ds26$members)),big.mark=","),
        " (actual 2025: ",format(OBS_MEMBERS_2025,big.mark=","),")\n\n",
      "── 2026 default baseline (backward-compatibility check) ──\n",
      "  Default GVC: $",format(round(def_gvc),big.mark=","),
        "  (expected $491,359,100)\n",
      "  Match: ",if(abs(def_gvc-491359100)<5)"PASS ✓" else "FAIL ✗","\n\n",
      "── Key assumptions ──\n",
      "  A1.  2024 baseline uses directly observed IRD average GVC per income and\n",
      "       age band. Ensures exact reproduction of $1,022,341,210 total.\n\n",
      "  A2.  KiwiSaver age band member totals are taken directly from Sheet 2 of\n",
      "       the 2025 monitoring data. Avoids the 1.3% rounding gap from the\n",
      "       previous participation rate derivation.\n\n",
      "  A3.  The taxable income distribution is used only as a proportional\n",
      "       shape within each age band; it does not determine totals. This\n",
      "       separates the role of the two datasets: monitoring data for\n",
      "       totals, taxable income data for within-band shape.\n\n",
      "  A4.  The 15-19 age group in the taxable income distribution is split\n",
      "       equally across five ages (1/5 per year) to map into KiwiSaver age\n",
      "       bands. No finer published split is available.\n\n",
      "  A5.  PAYE contribution rate mix from Sheet 6 (2025) is applied by $10k\n",
      "       income band. Most recent available data.\n\n",
      "  A6.  Members currently on the 3% rate move to 3.5% under the 2026 minimum\n",
      "       at default settings. This rate is a user-adjustable slider\n",
      "       (2%-10%); the assumption holds as stated only while the slider is at\n",
      "       its 3.5% default. The 2026 policy raises the minimum from 3% to 3.5%.\n\n",
      "  A7.  Unmatched contributors (530,592 — a mix of PAYE members missed by\n",
      "       the Sheet 15 snapshot and genuinely self-employed, passive, hybrid,\n",
      "       and no-income contributors; 62% are in fact PAYE — see Section 4.5)\n",
      "       are distributed uniformly across income bands using the 15.58%\n",
      "       overall rate. Sheet 14 provides a breakdown by income type but not\n",
      "       crossed with income band, so the rate cannot be allocated by band\n",
      "       on any better basis.\n\n",
      "  A8.  Unmatched contributors are split across the pp3-pp10 rate tiers\n",
      "       using each income type's own Sheet 15 rate-tier mix (66.70% pp3,\n",
      "       15.18% pp4, 6.71% pp6, 5.31% pp8, 6.10% pp10), rather than assigned\n",
      "       entirely to pp3. This split is a national average by income type,\n",
      "       applied uniformly regardless of income band.\n\n",
      "  A9.  The 'Over $180,000' band uses $200,000 as its midpoint. This band\n",
      "       is ineligible for GVC so the midpoint does not affect cost.\n\n",
      " A10.  Non-contributor uptake can be uniform across income bands and age\n",
      "       groups, or set separately above and below an income threshold.\n",
      "       No empirical basis for differential uptake. Treat slider as a\n",
      "       sensitivity tool.\n\n"
    )
  })
}

# ================================================================
shinyApp(ui, server)
