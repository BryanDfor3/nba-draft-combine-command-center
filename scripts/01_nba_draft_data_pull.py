import requests
import pandas as pd
import time

def main():
    # Set headers to mimic a browser request (NBA API blocks some automated requests)
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        "Referer": "https://www.nba.com/"
    }

    years = [
        "2025-26",
        "2024-25", "2023-24", "2022-23", "2021-22", "2020-21",
        "2019-20", "2018-19", "2017-18", "2016-17", "2015-16",
        "2014-15", "2013-14", "2012-13", "2011-12", "2010-11",
        "2009-10", "2008-09", "2007-08", "2006-07", "2005-06",
        "2004-05", "2003-04", "2002-03", "2001-02", "2000-01"
    ]

    combine_data = {}

    selection = input("Do you have an existing file that you would like to append data to? (Y/N): ")

    try:
        while selection.lower() not in ['y','n']:
            selection = input("Invalid input. Do you have an existing file that you would like to append data to? (Y/N): ")
    except:
        selection = input("Invalid input. Do you have an existing file that you would like to append data to? (Y/N): ")

    if selection.lower() == 'y':
        path = input("Enter the filename of the existing file: ")
        years = input("Enter the season(s) to append to the existing file (ex. 2025-26), separating by spaces: ")
        years = years.split()
    else:
        print("Pulling data for all available years")

    print('Retrieving draft combine data...\n')
    for i, year in enumerate(years):
        # API URL for Draft Combine Stats
        url = f"https://stats.nba.com/stats/draftcombinestats?LeagueID=00&SeasonYear={year}"

        # Send request
        try:
            response = requests.get(url, headers=headers)
            data = response.json()

            # Extract column headers and rows
            columns = data["resultSets"][0]["headers"]
            rows = data["resultSets"][0]["rowSet"]
            combine_data[year] = pd.DataFrame(rows, columns=columns)
            print(f"Added data for {year}! \n")
            time.sleep(5)
        
        except:
            print(f"Failed to add data for {year} \n")
    print("Finished adding all available draft combine data!")

    latest_combine_data = pd.concat(combine_data.values(), axis=0)

    if selection.lower() == 'y':
        try:
            existing_file = pd.read_csv(path)
            all_combine_data = pd.concat([latest_combine_data, existing_file], axis=0, ignore_index=True)
            print("Added new rows to existing data.")
        
        except:
            print("Error loading file")
            return

    else:
        all_combine_data = latest_combine_data

    all_combine_data.reset_index(drop=True, inplace=True)
    all_combine_data.to_csv('nba_draft_combine_data.csv', index=False)
    print("Data saved to 'nba_draft_combine_data.csv'.")

if __name__ == "__main__":
    main()