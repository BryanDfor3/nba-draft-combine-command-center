# NBA Draft Combine Command Center (2025)

<img src="https://github.com/BryanDfor3/nba-draft-combine-command-center/blob/main/nba-draft-combine-command-center.gif" align="left"/> <br>  

## Background

The NBA Draft Combine is where prospective NBA players are invited to participate in drills, scrimmages, and undergo a thorough anthropomorphic measurements process with an audience of NBA team personnel present to evaluate skills and physical attributes. This data is tracked and listed [here](/https://www.nba.com/stats/draft/combine-anthro) on NBA.com, but lacks a seamless way for historical comparison. As an outsider, it can also be hard to gauge what “good” is for a particular drill or measurement. For example, understanding the significance of [Grant Nelson's 2.53 second Shuttle Run](/https://www.instagram.com/reel/DJmxdfzys6C/) in 2025. 

The app that I’ve published addresses these issues by consolidating the data and presenting in multiple views in a clean UI:

- **Radar Chart View -** Allows for comparing players’ measurements against the average of their positional peer group
- **Percentile Values View** - Allows for viewing the percentile score for each of a player’s measurements relative to all participants measured since 2000
- **Raw Measurements View** - Contains the raw values from NBA.com to view the actual measurement values (e.g. weight in lbs rather than percentile value)

This tool was created primarily for data exploration, as there are options to filter by and/or select from the full universe of individual players, schools, draft combine year, and player positions from the combine's participants. The app also maintains the ability to sort by an individual measurement as is currently available on NBA.com. 

## Structure

- `scripts/01_nba_draft_data_pull.py`: Pulls historical combine data from the NBA API
- `scripts/02_nba_draft_data_prep.py`: Cleans and analyzes data, calculates percentiles
- `scripts/nba_draft_combine_shiny_app.R`: R Shiny dashboard for visualizing combine measurements
- `data/`: Storage for datasets like `nba_draft_combine_data.csv`, `nba_draft_combine_ptiles.csv`, `combine_scrimmage_boxscores.csv`, and `2025-headshots-and-origin.csv`

## Running the Project

**I. Python Scripts (data pull and analysis):**

#### `scripts/01_nba_draft_data_pull.py`
- Pulls NBA Draft Combine data using the NBA stats API <br />
- Prompts for whether to append or overwrite data

#### `scripts/02_nba_draft_data_prep.py`
- Takes the output from from `01_nba_draft_data_pull.py` and calculates percentile values for each combine measurement <br />
- Pulls player info (school, country) from the NBA API

#### Usage:
```bash
python scripts/01_nba_draft_data_pull.py
python scripts/02_nba_draft_data_prep.py
```

#### II. R Shiny App
#### `scripts/nba_draft_combine_shiny_app.R`
Generates an interactive dashboard comparing player measurements against positional averages and historical context using radar charts, lollipop charts, and a gt table

#### Usage:
In R or R Studio: shiny::runApp("shiny_app")

OR 

Open `nba_draft_combine_shiny_app.R` in R Studio and click "Run App"

#### Required R packages:
- dplyr 
- ggradar
- ggplot2
- patchwork
- shiny
- stringr
- bslib
- tidyr
- purrr
- shadowtext
- gt
- gtExtras
- glue

## Notes
- Python scripts generate `nba_draft_combine_data_ptiles.csv`, used by the Shiny app
- Set your working directory to the project root when running scripts
- All API requests respect rate limits via pauses with time.sleep() in Python

## Data Availability
- **Measurements Data:** Data dates back to the 2000 NBA Draft Combine (individual player participation may vary)
- **Scrimmage Data:** I have been able to track down the box scores from all four scrimmages for each NBA Draft Combine back to 2021, as well as one scrimmage box score from 2018 and 2019. Has beeen consolidated into `combine_scrimmage_boxscores.csv`
## Outcomes
- Identified Cedric Coward and Yanic Konan Niederhauser (both 2025 draft prospects) as having two of the five best combine measurements relative to their position in combine history. Coward and Niederhauser were ranked #31 and #60 in consensus mock drafts before the combine, and ended up being drafted #11 and #30 respectively, likely in part due to their strong combine performances.
- Given that the New Orleans Pelicans traded a 2026 unprotected first round pick to move up to #13 from #23, it can be argued that Coward's rise (#31 to #11) is of equal or higher value to New Orleans' perceived value of their 2026 pick

