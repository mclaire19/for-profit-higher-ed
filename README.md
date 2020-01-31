# For-Profit Higher Education: An R Shiny Tool
R Shiny App exploring Department of Education four-year higher education institution data, especially for-profit ones.

### Link to final R Shiny App: https://madeline-pickens.shinyapps.io/ForProfitEducation/

This analysis focuses on the impact of federal spending on for-profit institutions of higher education. I used aggregated data from the Department of Education to focus on federal education expenditures and outcomes, specifically with regard to for-profit private institutions. Using RShiny, I built both static and interactive plots to demonstrate relationships between proportion of students receiving Pell Grants, proportion of students receiving federal student loans, tuition costs, and rates of repayment, completion, and default. Ultimately, my analysis shows that for-profit universities cost more than equivalent public universities, students who attend them take out more loans and are awarded more Pell grants (have greater financial need), and their outcomes are worse; repayment rates are lower, completion rates are lower, and default rates are higher.

### Data Sources
The data source was from the Department of Education College Scorecard, available [here](https://collegescorecard.ed.gov/data/). I limited my analysis to the most recent institution-level data, which as of December 2019 was from 2017-2018. Very detailed [technical documentation](https://collegescorecard.ed.gov/assets/FullDataDocumentation.pdf) and a [data dictionary](https://collegescorecard.ed.gov/data/documentation/) was also available. This data combines sources from the Integrated Post Education Data System (IPEDS), National Student Loan Data System (NSLDS), and administrative earnings data from tax records.

In the **/data** directory, there is a script called `read_full.R` that reads the original CSV from the Department of Education (not included due to file size) and filters it to include only four-year institutions, resulting in the `fouryr.csv` file that is used in the rest of the Shiny app.

### Scripts

The remainder of the Shiny app is built using the `app.R` script. Additional data wrangling was also performed within this script.
