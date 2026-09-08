# KiwiSaver Government Contribution microsimulation model

An interactive model for estimating the fiscal cost and distributional effects of changes to the KiwiSaver Government Contribution (GVC).

Built and maintained by Te Ara Ahunga Ora Retirement Commission.

<!-- TODO: paste the Posit Connect Cloud URL here once the app is deployed, then delete this comment -->
**Live app:** _to be added_

**Model version:** v14. The version history in the methodology note is the authoritative record of what changed and when.

---

## Status

This is prototype code, shared so that users can interact with the model and give us feedback. It is not a finished product and it is not a Te Ara Ahunga Ora publication.

The model is a research tool for comparing policy settings against each other. It is not a forecast, and its outputs do not constitute Te Ara Ahunga Ora advice or a recommendation on any policy option. Despite reasonable measures to ensure accuracy, Te Ara Ahunga Ora makes no warranty, express or implied, and accepts no liability for any loss arising from use of this model or its outputs.

---

## What this model does

The model estimates what the GVC would cost, and who would receive it, under alternative policy settings. It works from a grid of 1,456 cells covering 182 income bands by 8 KiwiSaver age bands, each carrying a member count, an income midpoint, and the distribution of members across contribution-rate tiers.

For every cell, the model applies the GVC formula (member contribution, multiplied by the match rate, capped at the maximum) and aggregates upward. It reproduces the 2024 observed outturn of $1,022,341,210 exactly from IRD administrative data, and produces a verified 2026 baseline of $491,359,100 under current policy settings.

It was developed to support analysis of Recommendation 3 of the 2025 Review of Retirement Income Policies, which proposed redistributing the GVC toward lower-income savers within the existing spending envelope. It is general enough to model any parameter change to the scheme, not only redistribution options.

## What you can change

| Lever | Default | Range |
|---|---|---|
| Match rate | $0.25 per $1 contributed | $0.25 to $2.00 |
| Maximum annual contribution | $260.72 | Any value |
| Member minimum contribution rate | 3.5% | 2% to 10% |
| Income eligibility cap | $180,000 | $10,000 to $180,000, or none |
| Age eligibility | The six bands from 16-17 to 55-64 | Any combination of the eight bands |
| Income targeting | Off | Enhanced match and maximum below a chosen income |
| Age targeting | Off | Enhanced match and maximum for chosen age bands |
| Non-contributor uptake | 0% | Share of non-contributors who begin contributing, applied uniformly or at separate rates above and below an income threshold |
| Income growth | 0% | Applied before eligibility and entitlement are assessed |

Where both income and age targeting are active, age targeting takes precedence.

## What you get back

Five tabs:

- **Summary** compares the 2024 observed outturn, the 2026 default baseline, and your scenario side by side. Scenario metrics include total cost, change against the default, average GVC per contributing member, change in coverage, the income needed to reach the maximum, and the share of the enhanced group actually reaching it.
- **By income band** and **By age band** break cost and membership down across 19 reporting income bands (uniform $10,000 steps to $180,000, plus a single over-$180,000 group) or the eight age bands.
- **Distributional impacts** shows which groups gain and which lose against the 2026 default, in dollars, percentage, and coverage.
- **Model validation** documents the assumptions, the data sources, and the checks the model runs against known figures on startup.

## Two results worth understanding before you use it

**The maximum binds for most people.** At default settings a member contributing 3.5% reaches the $260.72 maximum at an income of about $29,797. Above that, extra income adds nothing. This is why raising the match rate saturates quickly while raising the maximum drives real fiscal cost, and why a large share of an economy-wide match increase flows to people earning under $30,000.

**A generous headline maximum is not the same as a generous outcome.** At default settings 83.1% of contributing members reach the full maximum. Raising the maximum to $725 without changing anything else more than doubles the cost and pushes the income needed to reach it to $82,857, at which point only 41.0% get the full amount. The model reports this reachability figure directly, because options with the most generous headline numbers often deliver least to the people they are aimed at.

---

## Running it

### In the browser

Use the hosted version linked above. Nothing to install.

### Locally

Requires R with the `shiny` and `dplyr` packages.

```r
install.packages(c("shiny", "dplyr"))
shiny::runApp()
```

Or run it straight from this repository without cloning:

```r
shiny::runGitHub("TAAORC-GVC-Model", "RetireBENt")
```

You do **not** need the source spreadsheet or the build script to run the model. The app reads the three prepared CSV files in this directory, which are committed here.

### Rebuilding the dataset

Only needed if the source data changes. Move `IRD_Taxable_income_distribution_of_individuals_2025.xlsx` from `build/source-data/` into the same directory as the build script, then:

```r
source("Build_dataset_for_GVC_model.R")
```

This regenerates `dataset_2026.csv`, `dataset_by_income.csv`, and `dataset_by_age.csv`, and prints validation output including the 2024 and 2026 baseline totals. Both should reproduce the figures above exactly. If they do not, do not use the output.

The taxable income distribution is the only spreadsheet the build script reads. Figures from the other two sources are transcribed into the script as fixed values, so those files are not needed to rebuild.

---

## Repository contents

```
app.R                            Launcher. Hosting platforms look for this name.
TAAORC_GVC_Model.R               The model and interface.
dataset_2026.csv                 The 1,456-cell simulation grid.
dataset_by_income.csv            2024 observed outturn by income band.
dataset_by_age.csv               2024 observed outturn by age band.
manifest.json                    Package and R version pins for deployment.
README.md                        This file.
build/
  Build_dataset_for_GVC_model.R  Builds the three CSVs from source data.
  source-data/
    IRD_Taxable_income_distribution_of_individuals_2025.xlsx
docs/
  methodology-note.pdf           Full methodology, assumptions, and validation.
```

## Data sources

| File | Source | Used for | In this repository |
|---|---|---|---|
| `2024_IRD_Administrative_KiwiSaver_Data.xlsx` | Inland Revenue, custom extract prepared for the 2025 Review of Retirement Income Policies | The 2024 observed baseline, reproduced exactly rather than estimated | No. Figures transcribed into the build script as fixed values |
| `IRD_Taxable_income_distribution_of_individuals_2025.xlsx` | Inland Revenue, published statistical release | Income bands, and the shape of the income distribution within each age band | Yes, in `build/source-data/` |
| `IRD_KiwiSaver_Monitoring_Project_Data_at_30_June_2025.xlsx` | Inland Revenue, published statistical release | Member counts by age, contribution rates, and contributor status | No. Figures transcribed into the build script as fixed values |

All three datasets originate with Inland Revenue. The taxable income distribution determines the shape of the income distribution within each age band only. Totals come from KiwiSaver monitoring data, so the model is anchored to observed membership rather than to estimates.

The two published releases are available from Inland Revenue's website. The 2024 administrative extract was prepared for a specific piece of work and is not a published release; every figure drawn from it appears in the build script and in the methodology note.

## Limitations

This is a static microsimulation. It is a tool for comparing policy settings against each other, not a forecast.

- **No behavioural response**, other than the uptake setting, which is a sensitivity tool with no empirical basis rather than an estimate. Members are assumed not to change their contribution rate in response to a change in the GVC.
- **Single year.** The model reports annual cost and distribution. It says nothing about lifetime effects, accumulated balances, or retirement income adequacy.
- **No interaction with other transfers.** Effects on other income support are not modelled.
- **Income growth is uniform.** The growth slider applies one rate to every member. Differential growth by income or age is not modelled.
- **Targeting acts on one dimension at a time.** Income targeting and age targeting each apply to a single dimension, and the income eligibility cap is global, so the tool cannot currently express an intersection such as boosting ages 55 to 64 only below $80,000.
- **Contribution rates are a point-in-time snapshot.** 530,592 members contributed during the year without a rate recorded on the 30 June snapshot. These are distributed across all five rate tiers rather than assumed to be at the minimum. About 62% are PAYE members the snapshot missed, not self-employed. See the methodology note, section 4.5.

Every assumption is listed in the model's own validation tab and documented in full in the methodology note.

## Related publications

Policy analysis drawing on this model is published on the Te Ara Ahunga Ora website rather than in this repository. All figures in that analysis can be reproduced in the app. Where an analysis was carried out outside the model, the publication says so explicitly.

---

## Citation

Te Ara Ahunga Ora Retirement Commission (2026). _KiwiSaver Government Contribution microsimulation model_, version 14. <!-- add the app URL once deployed -->

## Licence

No licence is applied at this stage. This is prototype code shared for user testing, so all rights are reserved by default: you are welcome to read the code, run the app, and use the results in your own thinking, but not to redistribute or adapt the model.

If you would like to reuse or build on it, get in touch and we will consider it. A formal open licence may follow if the model moves beyond a testing arrangement.

## Contact

ben@retirement.govt.nz

Issues and corrections are welcome through this repository's issue tracker. If you find a discrepancy between a figure in the app and a figure in the methodology note, please raise it. Both are maintained together and should never disagree.
