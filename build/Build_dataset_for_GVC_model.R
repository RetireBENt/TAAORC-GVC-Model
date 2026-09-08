# ================================================================
# Build dataset for GVC model
# KiwiSaver GVC Microsimulation — Dataset Builder
# Te Ara Ahunga Ora Retirement Commission
#
# WHAT THIS SCRIPT DOES:
#   - Builds the 2024 observed baseline as two lookup tables (by income band
#     and by age band) directly from IRD administrative data — no estimation.
#   - Builds the 2026 simulation grid: 182 fine income bands × 8 KiwiSaver age
#     bands (1,456 rows), each with member count, income midpoint, and revised
#     contribution-rate proportions (pp3, pp4, pp6, pp8, pp10, ppn).
#
# CONTRIBUTION-RATE PROPORTIONS (see methodology Sections 4.4-4.5):
#   Sheet 6/15 capture a 30-June point-in-time rate for 1,799,359 members.
#   Sheet 14 shows 2,329,951 members actually contributed over the year. The
#   530,592-member gap ("unmatched contributors") did contribute but had no
#   rate on the snapshot. This group is NOT exclusively non-PAYE — broken down
#   by income type (Sheet 14 contributed minus Sheet 15 rate-captured, per
#   type) it is:
#     PAYE 328,170 (62%) / Self-employed 123,110 / Passive 65,153 /
#     Hybrid 8,087 / No income 6,072
#   unmatched_rate = 530,592 / 3,405,406 = 15.58% of all members.
#   These are moved OUT of ppn and distributed across ALL FIVE rate tiers
#   (pp3-pp10), each type weighted by its own Sheet 15 rate mix. The aggregate
#   split is pp3 66.70% / pp4 15.18% / pp6 6.71% / pp8 5.31% / pp10 6.10%
#   — they are NOT all assigned to pp3 (that was the superseded approach).
#
# DATA-MAPPING FIXES BAKED INTO THIS BUILD:
#   - get_rate_props() rate bands start at lo=0, so the $0.01-$100 income band
#     maps to the lowest rate tier (previously it fell through to the top tier).
#   - Three income-distribution source-label overlaps ($20,000 / $50,000 /
#     print the previous band's upper bound as their own lower bound) are
#     corrected by +1 so they map to the correct $10k rate tier (see below).
#
# SOURCE DATA FILES:
#   - 2024_IRD_Administrative_KiwiSaver_Data.xlsx
#       IRD administrative data. Supplies the 2024 observed baseline lookup
#       tables in Section 1. Values are transcribed as literals below rather
#       than read at run time, so this file is provenance, not an input.
#   - IRD_Taxable_income_distribution_of_individuals_2025.xlsx
#       IRD published taxable income statistics. Supplies the income
#       bands and the income-by-age distribution. This is the only file
#       read programmatically (Section 2, "Age by income band
#       distribution" sheet).
#   - IRD_KiwiSaver_Monitoring_Project_Data_at_30_June_2025.xlsx
#       IRD. Source of every "Sheet n" reference in this script: Sheet 2
#       (member counts by age), Sheet 6 (PAYE rates by income band),
#       Sheet 14 (members who contributed), Sheet 15 (rate by income type).
#       Values are transcribed as literals, so this file is also provenance.
#
# VALIDATION TARGETS:
#   2024 baseline: $1,022,341,210 (reproduced exactly)
#   2026 baseline: ~$491.4m at default settings
#     (Treasury 2026 BEFU KiwiSaver-subsidy forecast: $560m for 2026)
# ================================================================

library(dplyr)
library(readxl)


# ================================================================
# 1. 2024 BASELINE (observed IRD data)
# ================================================================

dataset_by_income <- data.frame(
  income_band  = c("$1 - $20,000","$20,000.01 - $40,000","$40,000.01 - $60,000",
                   "$60,000.01 - $80,000","$80,000.01 - $100,000",
                   "$100,000.01 - $120,000","$120,000.01 +","No income info"),
  members      = c(919295,537617,464486,488519,326374,205772,394090,11895),
  avg_gvc_2024 = c(76.954395,207.533394,418.001039,457.080097,
                   457.543895,457.523222,454.316828,4.907798),
  income_mid   = c(10000.5,30000.0,50000.0,70000.0,90000.0,110000.0,135000.0,0.0),
  stringsAsFactors = FALSE
)
dataset_by_income$total_gvc_2024 <- dataset_by_income$members * dataset_by_income$avg_gvc_2024

dataset_by_age <- data.frame(
  age_band     = c("00-17","18-24","25-34","35-44","45-54","55-64","65+"),
  members      = c(194690,397937,741929,694207,576451,518070,224764),
  avg_gvc_2024 = c(1.172582,313.068077,322.783764,334.912190,
                   378.171317,388.720905,27.445023),
  age_mid      = c(9,21,29,39,49,59,68),
  stringsAsFactors = FALSE
)
dataset_by_age$total_gvc_2024 <- dataset_by_age$members * dataset_by_age$avg_gvc_2024


# ================================================================
# 2. 2026 SIMULATION DATASET
# ================================================================

# ── Load taxable income × age distribution ──────────────────────
inc_raw <- read_excel(
  "IRD_Taxable_income_distribution_of_individuals_2025.xlsx",
  sheet = "Age by income band distribution",
  col_names = TRUE
)
colnames(inc_raw) <- c("income_band","u15","a15_19","a20_24","a25_29","a30_34",
                       "a35_39","a40_44","a45_49","a50_54","a55_59","a60_64",
                       "a65_69","a70_74","a75p")

inc_df <- inc_raw %>%
  filter(!trimws(income_band) %in% c("nil","All")) %>%
  mutate(across(u15:a75p, as.numeric))

# ── Parse income band boundaries ──────────────────────────────────
parse_bounds <- function(band_str) {
  s <- trimws(band_str)
  if (grepl("Over \\$180,000", s)) return(c(180001, 999999))
  nums <- as.numeric(gsub(",","",
    regmatches(s, gregexpr("[0-9,]+\\.?[0-9]*", s))[[1]]))
  if (length(nums) >= 2) return(c(nums[1], nums[2]))
  return(c(nums[1], nums[1]))
}
inc_df$inc_low  <- sapply(inc_df$income_band, function(x) parse_bounds(x)[1])
inc_df$inc_high <- sapply(inc_df$income_band, function(x) parse_bounds(x)[2])

# ── Fix 3 known income-distribution source-label overlaps ────────────
# These 3 bands' printed lower bound duplicates the previous band's
# upper bound, e.g. "$20,000 - $21,000" instead of "$20,001 - $21,000"
# (same quirk at $50,000 and $80,000). Left uncorrected, get_rate_props()
# matches inc_low=20000/50000/80000 to the $10k rate band below the
# correct one (e.g. $10,001-$20,000 instead of $20,001-$30,000).
inc_df$inc_low[inc_df$inc_low %in% c(20000, 50000, 80000)] <-
  inc_df$inc_low[inc_df$inc_low %in% c(20000, 50000, 80000)] + 1

inc_df$inc_mid  <- (inc_df$inc_low + inc_df$inc_high) / 2
inc_df$inc_mid[inc_df$inc_low == 180001] <- 200000

# ── Map income-distribution age groups → KiwiSaver age bands ──────────
inc_df <- inc_df %>%
  mutate(
    ks_u16   = u15 + a15_19*(1/5),
    ks_16_17 = a15_19*(2/5),
    ks_18_24 = a15_19*(2/5) + a20_24,
    ks_25_34 = a25_29 + a30_34,
    ks_35_44 = a35_39 + a40_44,
    ks_45_54 = a45_49 + a50_54,
    ks_55_64 = a55_59 + a60_64,
    ks_65p   = a65_69 + a70_74 + a75p
  )

# ── Scale income-distribution counts to KiwiSaver actual totals ───
# SOURCE: Sheet 2, IRD_KiwiSaver_Monitoring_Project_Data_at_30_June_2025.xlsx
# These are the directly observed member counts per age band.
# The income distribution is used only to determine the SHAPE
# distribution within each age band; totals come from monitoring data.
#
# scale_factor(age) = KS_actual(age) / inc_dist_total(age)
# ks_members(age, income) = inc_dist(age, income) × scale_factor(age)
#
# This exactly reproduces observed age band totals with zero rounding gap.

ks_actual <- c(
  ks_u16=113884, ks_16_17=55603, ks_18_24=393184, ks_25_34=740046,
  ks_35_44=736764, ks_45_54=590744, ks_55_64=537553, ks_65p=237455
)

ks_age_bands <- names(ks_actual)

# Compute scale factors: KS actual / income-distribution column sum
inc_dist_totals <- sapply(ks_age_bands, function(b) sum(inc_df[[b]]))
scale_factors   <- ks_actual / inc_dist_totals

# Apply to get KS member counts at fine income band level
for (b in ks_age_bands) {
  inc_df[[paste0("ks_m_", b)]] <- inc_df[[b]] * scale_factors[b]
}


# ================================================================
# 3. REVISED CONTRIBUTION RATE PROPORTIONS
#
# Source: Sheet 6 (PAYE rates) + Sheet 14 (actual contributor status)
#         + Sheet 15 (rate by income type), for the type-specific split
#
# KEY FIX: Sheet 14 shows 2,329,951 members made a contribution in 2025.
# Sheet 15 (equivalently Sheet 6, summed across income bands) only
# captures 1,799,359 members with an identifiable rate on record — this
# is a point-in-time snapshot, not a period outcome, so it understates
# actual contributors. The 530,592-member gap is NOT specifically
# "non-PAYE contributors" — breaking it down by income type (Sheet 14
# minus Sheet 15, per type) shows 328,170 of the 530,592 (62%) are
# themselves PAYE members simply missed by the snapshot (e.g. a job
# change or employment gap around the reference pay period), with the
# remainder genuinely self-employed/passive/hybrid/no-income:
#   PAYE 328,170 / Self-employed 123,110 / Passive 65,153 /
#   Hybrid 8,087 / No income 6,072
# These "unmatched" contributors are moved from ppn into pp3-pp10,
# split by income type rather than assigned entirely to pp3.
#
# unmatched_rate = 530,592 / 3,405,406 = 15.58% of all members
#
# Type-specific split (each income type's unmatched contributors,
# weighted by that same type's own Sheet 15 rate-tier mix — including
# PAYE's own mix for the 328,170 PAYE members above; see methodology
# Section 4.5 for the full derivation):
#   pp3: 66.70%   pp4: 15.18%   pp6: 6.71%   pp8: 5.31%   pp10: 6.10%
#
# For each income band, this split of unmatched_rate is added across the
# five rate tiers (previously: 100% into pp3):
#   pp3_new  = pp3_old  + unmatched_rate * 0.6670
#   pp4_new  = pp4_old  + unmatched_rate * 0.1518
#   pp6_new  = pp6_old  + unmatched_rate * 0.0671
#   pp8_new  = pp8_old  + unmatched_rate * 0.0531
#   pp10_new = pp10_old + unmatched_rate * 0.0610
#   ppn_new  = max(0, ppn_old - unmatched_rate)
# ================================================================

TOTAL_KS_2025         <- 3405406
RATE_CAPTURED_MEMBERS <- 1799359  # Sheet 15/6: members across ALL income
                                   # types with a captured 3%-10% rate
TOTAL_CONTRIB         <- 2329951  # Sheet 14: members who actually contributed
UNMATCHED_CONTRIBS    <- TOTAL_CONTRIB - RATE_CAPTURED_MEMBERS  # = 530,592
UNMATCHED_RATE        <- UNMATCHED_CONTRIBS / TOTAL_KS_2025     # = 0.1558

# Income-type-specific split of UNMATCHED_RATE across rate tiers
# (derived from Sheet 14 minus Sheet 15, by income type — see Section 4.5)
UNMATCHED_SPLIT <- c(pp3=0.6670, pp4=0.1518, pp6=0.0671, pp8=0.0531, pp10=0.0610)
stopifnot(abs(sum(UNMATCHED_SPLIT) - 1) < 1e-4)


# PAYE rate counts by $10k income band (Sheet 6)
contrib_bands <- list(
  list(lo=0,     hi=10000,  p3=59099, p4=7686,  p6=4880, p8=2984,  p10=5114,  pn=597262),
  list(lo=10001, hi=20000,  p3=52692, p4=8206,  p6=5078, p8=3151,  p10=4619,  pn=124239),
  list(lo=20001, hi=30000,  p3=65739, p4=10990, p6=6281, p8=3959,  p10=5058,  pn=234288),
  list(lo=30001, hi=40000,  p3=73282, p4=13378, p6=6743, p8=4800,  p10=5431,  pn=117018),
  list(lo=40001, hi=50000,  p3=90420, p4=18607, p6=8862, p8=6702,  p10=6961,  pn=87619),
  list(lo=50001, hi=60000,  p3=119771,p4=28590, p6=13000,p8=10079, p10=9732,  pn=75848),
  list(lo=60001, hi=70000,  p3=127569,p4=34071, p6=14389,p8=11136, p10=10988, pn=64177),
  list(lo=70001, hi=80000,  p3=114954,p4=32579, p6=12333,p8=10774, p10=10375, pn=57739),
  list(lo=80001, hi=90000,  p3=91056, p4=27486, p6=10090,p8=8654,  p10=8353,  pn=40796),
  list(lo=90001, hi=100000, p3=71650, p4=22816, p6=7878, p8=7080,  p10=6777,  pn=30862),
  list(lo=100001,hi=110000, p3=57349, p4=19036, p6=6375, p8=5909,  p10=5820,  pn=24787),
  list(lo=110001,hi=120000, p3=43827, p4=14960, p6=5025, p8=4712,  p10=4530,  pn=18851),
  list(lo=120001,hi=130000, p3=33994, p4=11833, p6=4016, p8=3648,  p10=3658,  pn=15377),
  list(lo=130001,hi=140000, p3=26787, p4=9395,  p6=3141, p8=2866,  p10=2841,  pn=12295),
  list(lo=140001,hi=150000, p3=21285, p4=7342,  p6=2528, p8=2353,  p10=2285,  pn=10269),
  list(lo=150001,hi=999999, p3=101602,p4=36201, p6=12429,p8=11918, p10=11947, pn=68921)
)

# Build proportions data frame with revised ppn
rate_props <- do.call(rbind, lapply(contrib_bands, function(b) {
  tot <- b$p3 + b$p4 + b$p6 + b$p8 + b$p10 + b$pn
  # Original PAYE proportions
  pp3_old  <- b$p3  / tot
  pp4_old  <- b$p4  / tot
  pp6_old  <- b$p6  / tot
  pp8_old  <- b$p8  / tot
  pp10_old <- b$p10 / tot
  ppn_old  <- b$pn  / tot
  # Revised: move unmatched contributors from ppn into pp3-pp10, split by
  # income-type rate mix (UNMATCHED_SPLIT) rather than assigning all to pp3
  pp3_new  <- pp3_old  + UNMATCHED_RATE * UNMATCHED_SPLIT[["pp3"]]
  pp4_new  <- pp4_old  + UNMATCHED_RATE * UNMATCHED_SPLIT[["pp4"]]
  pp6_new  <- pp6_old  + UNMATCHED_RATE * UNMATCHED_SPLIT[["pp6"]]
  pp8_new  <- pp8_old  + UNMATCHED_RATE * UNMATCHED_SPLIT[["pp8"]]
  pp10_new <- pp10_old + UNMATCHED_RATE * UNMATCHED_SPLIT[["pp10"]]
  ppn_new  <- pmax(0.0, ppn_old - UNMATCHED_RATE)
  data.frame(
    lo=b$lo, hi=b$hi,
    pp3=pp3_new, pp4=pp4_new, pp6=pp6_new,
    pp8=pp8_new, pp10=pp10_new, ppn=ppn_new
  )
}))

# Lookup function
get_rate_props <- function(inc_low) {
  for (i in seq_len(nrow(rate_props))) {
    if (inc_low >= rate_props$lo[i] && inc_low <= rate_props$hi[i])
      return(rate_props[i,])
  }
  return(rate_props[nrow(rate_props),])
}


# ================================================================
# 4. BUILD 2026 SIMULATION GRID
# ================================================================

age_labels <- c("Under 16","16-17","18-24","25-34","35-44","45-54","55-64","65+")
rows_list  <- vector("list", nrow(inc_df) * length(ks_age_bands))
idx <- 1L

for (i in seq_len(nrow(inc_df))) {
  rp <- get_rate_props(inc_df$inc_low[i])
  for (j in seq_along(ks_age_bands)) {
    ab   <- ks_age_bands[j]
    mcol <- paste0("ks_m_", ab)
    rows_list[[idx]] <- data.frame(
      income_band  = inc_df$income_band[i],
      inc_low      = inc_df$inc_low[i],
      inc_high     = inc_df$inc_high[i],
      inc_mid      = inc_df$inc_mid[i],
      age_band     = age_labels[j],
      age_band_key = ab,
      members      = inc_df[[mcol]][i],
      pp3          = rp$pp3, pp4 = rp$pp4, pp6 = rp$pp6,
      pp8          = rp$pp8, pp10= rp$pp10,ppn = rp$ppn,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
}

dataset_2026 <- do.call(rbind, rows_list)


# ================================================================
# 5. VALIDATION
# ================================================================

OBS_GVC_2024 <- 1022341210

cat("\n=== VALIDATION ===\n\n")

cat("── 2024 baseline ──\n")
cat(sprintf("  Income table GVC: $%s  (IRD: $%s)  diff=$%+.0f\n",
    format(round(sum(dataset_by_income$total_gvc_2024)),big.mark=","),
    format(OBS_GVC_2024,big.mark=","),
    sum(dataset_by_income$total_gvc_2024)-OBS_GVC_2024))
cat(sprintf("  Age table GVC:    $%s  (IRD: $%s)  diff=$%+.0f\n",
    format(round(sum(dataset_by_age$total_gvc_2024)),big.mark=","),
    format(OBS_GVC_2024,big.mark=","),
    sum(dataset_by_age$total_gvc_2024)-OBS_GVC_2024))

cat("\n── 2026 grid ──\n")
cat(sprintf("  Rows:          %d  (%d income bands × %d age bands)\n",
    nrow(dataset_2026), length(unique(dataset_2026$income_band)),
    length(ks_age_bands)))
cat(sprintf("  Total members: %s  (actual 2025: %s)  diff=%+.0f\n",
    format(round(sum(dataset_2026$members)),big.mark=","),
    format(TOTAL_KS_2025,big.mark=","),
    sum(dataset_2026$members) - TOTAL_KS_2025))

# Quick 2026 baseline GVC (default settings)
elig <- dataset_2026[
  dataset_2026$age_band_key %in% c("ks_16_17","ks_18_24","ks_25_34",
                                    "ks_35_44","ks_45_54","ks_55_64") &
  dataset_2026$inc_low <= 180000, ]

RATES <- c(pp3=0.035, pp4=0.04, pp6=0.06, pp8=0.08, pp10=0.10)
match_rate <- 0.25; max_gvc <- 260.72

gvc_2026 <- with(elig, {
  g <- numeric(nrow(elig))
  for (r in names(RATES)) {
    g <- g + elig[[r]] * members * pmin(inc_mid * RATES[r] * match_rate, max_gvc)
  }
  g
})
cat(sprintf("\n  2026 baseline GVC (default):  $%s\n",
    format(round(sum(gvc_2026)),big.mark=",")))
cat(sprintf("  Treasury forecast (2026 BEFU):  $560m\n"))
cat(sprintf("  2024 observed GVC:            $%s\n",
    format(OBS_GVC_2024,big.mark=",")))

cat("\n── Contribution rate revision ──\n")
cat(sprintf("  Rate-captured members, all types (Sheet 15/6): %s  (%s%%)\n",
    format(RATE_CAPTURED_MEMBERS,big.mark=","),
    round(RATE_CAPTURED_MEMBERS/TOTAL_KS_2025*100,1)))
cat(sprintf("  All contributors (Sheet 14):       %s  (%s%%)\n",
    format(TOTAL_CONTRIB,big.mark=","),
    round(TOTAL_CONTRIB/TOTAL_KS_2025*100,1)))
cat(sprintf("  Unmatched contributors reclassified:%s  (%s%%)\n",
    format(UNMATCHED_CONTRIBS,big.mark=","),
    round(UNMATCHED_RATE*100,2)))
cat(sprintf("  True non-contributors:             %s  (%s%%)\n",
    format(TOTAL_KS_2025-TOTAL_CONTRIB,big.mark=","),
    round((TOTAL_KS_2025-TOTAL_CONTRIB)/TOTAL_KS_2025*100,1)))

# ================================================================
# 6. SAVE
# ================================================================

write.csv(dataset_by_income, "dataset_by_income.csv", row.names=FALSE)
write.csv(dataset_by_age,    "dataset_by_age.csv",    row.names=FALSE)
write.csv(dataset_2026,      "dataset_2026.csv",      row.names=FALSE)

cat(sprintf("\nSaved: dataset_by_income.csv (%d rows)\n", nrow(dataset_by_income)))
cat(sprintf("Saved: dataset_by_age.csv    (%d rows)\n", nrow(dataset_by_age)))
cat(sprintf("Saved: dataset_2026.csv      (%d rows × %d cols)\n",
    nrow(dataset_2026), ncol(dataset_2026)))
