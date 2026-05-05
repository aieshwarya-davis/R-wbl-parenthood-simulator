# R-wbl-parenthood-simulator

# WBL Parenthood Pillar — Analytics and Simulation Tool

An interactive R Shiny application for exploring Women, Business and the Law (WBL)
Parenthood Pillar data. Built as a proof-of-concept to support policy dialogue on
legal reform, gender equality, and economic outcomes.

Link to the tool: https://aieshdavis.shinyapps.io/WBLTool_Parenthood/ 

Developed by Aieshwarya Davis.

## What the tool does

- Compares any country against regional or income-group peers on 2024 WBL
  Parenthood score or reform progress since 2010
- Categorizes countries into performance levels and assigns percentile labels
- Tracks how GDP per capita and female labor force participation move alongside
  WBL performance levels over time (2010–2024)
- Computes Spearman correlations between WBL performance and economic outcomes
- Generates gap analysis and talking points for policy engagement

A Policy Simulation tab is included as a preview of forthcoming scenario modeling
capabilities.

## Repository structure

```
r-wbl-parenthood-simulator/
├── app.R                          
├── data/
│   ├── WBL Simulator workbook.xlsx    
│   ├── GDP per capita data.xlsx       
│   └── Labor force participation.xlsx 
└── README.md
```

## Requirements

R 4.0 or later. Install required packages before running:

```r
install.packages(c("shiny", "plotly", "dplyr", "readxl",
                   "ggplot2", "tibble", "tidyr"))
```

## Running the app

```r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and click **Run App**.

## Data sources

- WBL Parenthood scores and reform history: World Bank Women, Business and the Law
  dataset (wbl.worldbank.org)
- GDP per capita (current USD): World Bank Open Data
- Female labor force participation rate: World Bank Open Data

## Notes on methodology

**Performance percentiles** represent score-based performance level categories,
not distributional percentiles. Countries with identical WBL scores are grouped
into the same category. With three distinct score levels in a comparison group,
labels read "Upper 33rd percentile", "Middle 33rd percentile", and "Lower 33rd
percentile" regardless of how many countries sit in each group.

**Correlation analysis** uses Spearman rank correlation (ρ) computed annually
across performance levels. Percentile scores are rescaled so that higher values
always indicate better WBL performance, making positive correlations intuitive:
a positive ρ means better WBL performance is associated with better economic outcomes.

## Author

Aieshwarya Davis  
aieshwaryadavis@gmail.com
