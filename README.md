# NBA Draft Combine Command Center

## Background

The NBA Draft Combine is where prospective NBA players are invited to participate in drills, scrimmages, and undergo a thorough anthropomorphic measurements process with an audience of NBA team personnel present to evaluate skills and physical attributes. This data is tracked and listed here on NBA.com, but lacks a seamless way for historical comparison. As an outsider, it can be hard to gauge what “good” is for a particular attribute. 

The app that I’ve published addresses these issues by consolidating the data and presenting in multiple views in a clean UI:

- **Radar Chart View -** Allows for comparing players’ measurements against the average of their positional peer group
- **Percentile Values View** - Allows for viewing the percentile score for each of a player’s measurements relative to all participants measured since 2000
- **Raw Measurements View** - Contains the raw values from NBA.com to view the actual measurement values (e.g. weight in lbs rather than percentile value)

Furthermore, this tool was created with the intention of data exploration. There are options to filter by individual player, school, draft combine year, and position, as well as the ability to sort by an individual measurement, similar to what’s already available on NBA.com. Two new capabilities offered by the app over NBA.com is the ability to filter by a minimum height or minimum wingspan threshold. 

## Structure

- `scripts/01_nba_draft_data_pull.py`: Pulls historical combine data from the NBA API
- `scripts/02_nba_draft_data_prep.py`: Cleans and analyzes data, calculates percentiles
- `scripts/nba_draft_combine_shiny_app.R`: R Shiny dashboard for visualizing combine measurements
- `data/`: Storage for datasets like `nba_draft_combine_data.csv`, `nba_draft_combine_ptiles.csv`

## Running the Project

**I. Python Scripts (data pull and analysis):**

#### `scripts/01_nba_draft_data_pull.py`
- Pulls NBA Draft Combine data using the NBA stats API <br />
- Prompts for whether to append or overwrite data

#### `scripts/02_data_pull.py`
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
- glue

## Notes
- Python scripts generate `nba_draft_combine_data_ptiles.csv`, used by the Shiny app
- Set your working directory to the project root when running scripts
- All API requests respect rate limits via pauses with time.sleep() in Python
