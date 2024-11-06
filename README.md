# MLB Switch-Hitting Analysis Project

## Overview
This project analyzes MLB switch-hitters to determine if certain players would benefit from abandoning switch-hitting in favor of batting exclusively from their stronger side. Using advanced statistical analysis and machine learning techniques, we developed a model that predicts player performance if they were to hit from only one side of the plate.

### Key Findings
- Five of 19 sampled switch-hitters are predicted to improve their OPS by only hitting right-handed
- One switch-hitter is predicted to improve by only hitting left-handed
- Model validation using Cedric Mullins' actual transition data showed predictions within 0.004 of his actual OPS
- The model can be applied to any level of baseball, given sufficient data

## Background
Switch-hitters traditionally provide lineup flexibility and matchup advantages against both left and right-handed pitchers. However, some players have shown significant performance improvements after abandoning switch-hitting to focus on their stronger side, like Cedric Mullins of the Baltimore Orioles. This study aims to identify similar candidates who might benefit from such a change.

## Data Sources
- Fangraphs.com Major League Leaders Splits Statistics (2018-2023)
- Sample includes:
  - 19 switch-hitters (600+ plate appearances vs both LHP and RHP)
  - 123 left-handed hitters (200+ PA vs LHP, 500+ PA vs RHP)
  - 230 right-handed hitters (250+ PA vs LHP, 500+ PA vs RHP)

## Methodology

### Metrics Analyzed
- Weighted On-Base Average (wOBA)
- On-Base Plus Slugging (OPS)
- Ground Ball Percentage (GB%)
- Line Drive Percentage (LD%)
- Hard Hit Percentage (Hard%)
- Walk Percentage (BB%)
- Strikeout Percentage (K%)

### Model Development
- Two random forest algorithms were developed:
  1. Left-handed prediction model (19.75% variance explained)
  2. Right-handed prediction model (34.32% variance explained)
- Root Mean Square Error (RMSE):
  - Left-handed model: 0.005
  - Right-handed model: 0.004

## Installation and Usage

### Prerequisites
- R (version 4.0 or higher)
- Required R packages:
  ```R
  library(tidyverse)
  library(readr)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)
  library(randomForest)
  library(caTools)
  library(janitor)
  library(Metrics)
  library(knitr)
  library(kableExtra)
  library(htmltools)
  ```

### Setup
1. Clone the repository
2. Ensure all required R packages are installed
3. Set working directory to project folder
4. Run the main analysis script

## Project Structure
```
├── R/
│   └── Switch Hitting Project1.R
├── data/
│   ├── 2018-2023 LHH vs LHP - min200.csv
│   ├── 2018-2023 LHH vs RHP - min500.csv
│   ├── 2018-2023 RHH vs LHP - min250.csv
│   ├── 2018-2023 RHH vs RHP - min500.csv
│   ├── 2018-2023 Switch-Hitter vs LHP as RHH.xlsx
│   └── 2018-2023 Switch-Hitter vs RHP as LHH.xlsx
└── docs/
    ├── Switch Hitting Final Project.docx
    └── Switch-Hitting Executive Summary.docx
```

## Limitations and Future Work
1. Data Coverage
   - Limited access to advanced metrics (e.g., Statcast data)
   - Relatively low variance explanation in models
   - Minimum plate appearance requirements limit sample size

2. Future Improvements
   - Incorporate additional advanced metrics
   - Expand analysis to minor league players
   - Develop more robust prediction models
   - Include additional seasons of data

## Authors
- Greg Halperin
- Patrick Holsey

## Acknowledgments
This project was completed as part of MSDS456 - Sports Performance Analytics (August 2024)

## References
- Mailhot, J. *Finding Switch-Hitters Who Should Stop Switch-Hitting*, Fangraphs.com Blogs, 2022
- Lindbergh, B. *Overthinking It: Searching for Switch-Hitters Who Shouldn't Switch-Hit*, BaseballProspectus.com, 2014
- Winston, W., Nestler, S., & Pelechrinis, K. *Mathletics*, Chapter 12: The Platoon Effect, 2022
- Lichtman, Michael. *The Book: Playing the Percentages in Baseball*, 2014

## License
This project is licensed under the MIT License - see the LICENSE file for details
