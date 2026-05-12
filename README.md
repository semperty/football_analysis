# The Process & The Pitch: Soccer Analytics

### Scraping xG, Financials, and the Variance in Between
As a data analyst with a background in video analysis, my goal is to blend objective data with tactical context. This is a living project where I scrape, clean, and structure soccer data to tell better stories about the game.

Current data sources include:
* **Understat:** Advanced match and player metrics (xG, xAG)
* **Capology / Transfermarkt:** Financial data and market valuations

### Technical Stack
* **Language:** R
* **Data Handling:** `dlyr`, `purrr`, `stringr`, `here`
* **Scraping:** `rvest`, `httr`, `jsonlite`
* **Storage:** `arrow` (All processed data is stored in Parquet format for efficiency)
* **Visualization:** `ggplot2`, `patchwork`, `ggbeeswarm`

### Project Structure
`/scripts`: R scripts for web scraping and data tidying

`/data`: Local storage for Parquet files (match stats and financial records)

`README.md`: Project documentation

### Future Goals
* Integrate match event data from WhoScored
* Develop visualizations that bridge the gap between statistical trends and video observation
* Maintain a longitudinal database of player wages vs. on-pitch production
--------------------------------------------------------------------------
**Note:** This project is a work in progress. Scripts are added and updated as the analysis evolves.
