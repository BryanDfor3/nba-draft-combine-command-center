import pandas as pd
import numpy as np
import scipy.stats as stats
from nba_api.stats.endpoints import commonplayerinfo
import time

def main(): 
    #Expand output window size
    pd.set_option('display.max_columns', 50)

    #Read in the data
    data = pd.read_csv('nba_draft_combine_data.csv')

    #Drop all players whose heights weren't measured (e.g. non-participants)
    data.dropna(subset=['HEIGHT_WO_SHOES'], axis=0, inplace=True)
    data.dropna(subset=['WINGSPAN'], axis=0, inplace=True)

    #Correct the display of player names by replacing question marks in the names with apostrophes
    names = ['FIRST_NAME', 'LAST_NAME', 'PLAYER_NAME']
    data[names] = data[names].apply(lambda x: x.str.replace('?', "'"))

    #Keep extra data in 'full_data' to be used at the end, and remove extra space in measurements columns
    full_data = data.copy()
    full_data = full_data[['SEASON', 'PLAYER_ID', 'HEIGHT_WO_SHOES_FT_IN', 'WINGSPAN_FT_IN', 
                            'STANDING_REACH_FT_IN', 'STANDING_VERTICAL_LEAP', 'MAX_VERTICAL_LEAP', 
                            'LANE_AGILITY_TIME', 'THREE_QUARTER_SPRINT', 'HAND_LENGTH', 'HAND_WIDTH']]
    measurements = ['HEIGHT_WO_SHOES_FT_IN', 'WINGSPAN_FT_IN', 'STANDING_REACH_FT_IN']
    full_data[measurements] = full_data[measurements].apply(lambda x: x.str.replace(' ', ''))

    #Keep most relevant data in 'data' dataframe by slicing, then drop more superfluous data fields in between the slicing indexes
    data = data.loc[:,'SEASON':'THREE_QUARTER_SPRINT']
    data.drop(columns=['HEIGHT_W_SHOES','HEIGHT_W_SHOES_FT_IN','BODY_FAT_PCT'], inplace=True)

    #Create 'HAND_SIZE' and 'PLUS_WS' variables
    data['HAND_SIZE'] = data['HAND_LENGTH'] * data['HAND_WIDTH']
    data['PLUS_WS'] = [
        "unmeasured" if pd.isna(x) 
        else ("+0" if round(x-y) == 0
            else(str(round(x - y)) if x < y 
            else "+" + str(round(x - y)))) 
        for x,y in zip(data['WINGSPAN'],data['HEIGHT_WO_SHOES'])]

    #Define first set of player positions in a list to iterate over
    positions = ['PG','SG','SF','PF','C']

    #Create player position columns in the dataframe, and assign 1/0 values based on whether they fall into the position group
    for pos in positions:
        data[pos] = data['POSITION'].str.find(pos)
        data[pos] = np.where(data[pos]>-1,1,data[pos])
        data[pos] = np.where(data[pos]==-1,0,data[pos])

    #Define the second set of player positions (position groups)
    def group(row):
        if (row['PG'] == 1 or row['SG'] == 1):
            return 'GUARD'
        elif row['SF'] == 1:
            return 'WING'
        else:
            return 'BIG'

    data['POS_GROUP'] = data.apply(group, axis=1)

    #Identify the numeric data columns in a list
    all_numeric_data_columns = ['HEIGHT_WO_SHOES','WEIGHT','WINGSPAN', 'STANDING_REACH',
        'MAX_VERTICAL_LEAP','LANE_AGILITY_TIME','THREE_QUARTER_SPRINT','HAND_SIZE']

    player_info = data.loc[:,['SEASON', 'PLAYER_ID','FIRST_NAME','LAST_NAME', 'PLAYER_NAME', 'POSITION', 'POS_GROUP', 'HEIGHT_WO_SHOES', 'PLUS_WS', 'WEIGHT', 'WINGSPAN']]

    def height_conversion(inches):
        ''' Converts height in inches to a rounded [(feet)'(inches)"] format
        Input:
        measure

        Output:
        df

        '''
        feet = str(int(round(inches) // 12))+"'"
        r_inches = round(round(inches) % 12)
        return f'{feet}{r_inches}"'

    player_info['HEIGHT'] = player_info['HEIGHT_WO_SHOES'].apply(height_conversion)
    player_info['WEIGHT'] = [x if pd.isna(x) else round(x) for x in player_info['WEIGHT']]
    player_info['WEIGHT'] = player_info['WEIGHT'].astype('Int64')

    #Function to chop the datasets to only contain index + measure
    def df_chop(df, measure):
        '''Takes a dataframe and chops it to include the measure information only
        INPUTS:
        df: The original dataframe
        measure: The measure to be evaluated

        OUTPUTS:
        new_df: The resulting dataframe
        '''
        #Keep all information columns and the measure of interest
        new_df = df.loc[:,[measure]]
        
        #Filter the data to only keep rows containing the measure of interest
        new_df = new_df[new_df[measure]>0]
        return new_df

    #Create dataframes for each measure of interest
    all_heights = df_chop(data, 'HEIGHT_WO_SHOES')
    all_weights = df_chop(data, 'WEIGHT')
    all_wingspans = df_chop(data, 'WINGSPAN')
    all_reach = df_chop(data, 'STANDING_REACH')
    all_svert = df_chop(data, 'STANDING_VERTICAL_LEAP')
    all_vert = df_chop(data, 'MAX_VERTICAL_LEAP')
    all_agility = df_chop(data, 'LANE_AGILITY_TIME')
    all_sprint = df_chop(data, 'THREE_QUARTER_SPRINT')
    all_hands = df_chop(data, 'HAND_SIZE')

    #Create an empty dictionary to loop into, and add the position groups to the positions list
    data_dict = {}
    positions += ['GUARD', 'WING', 'BIG']

    #Bring all of the draft combine data together into a list of dataframes
    combine_data = [all_heights, all_weights, all_wingspans, all_reach, all_svert, all_vert, all_agility, all_sprint, all_hands]

    #Calculate the z-scores and percentile values for each measurement. Ensure timed drills reward lower time values, and measurements reward higher measurement values
    for i, data in enumerate(combine_data):    
        data.rename(columns={data.columns[-1]: str(data.columns[-1])+'_PTILE'}, inplace=True)
        if i in [0, 1, 2, 3, 4, 5, 8]:
            data.iloc[:,-1] = stats.zscore(data.iloc[:,-1])
        else:
            data.iloc[:,-1] = -1 * stats.zscore(data.iloc[:,-1])
        data.iloc[:,-1] = 1-stats.norm.sf(data.iloc[:,-1]).round(4)
        data_dict[i] = data

    #Concatenate all of the dataframes in data_dict
    all_combine_data = pd.concat(data_dict.values(),axis=1)

    #Join the player info with the combine data from the previous step
    all_data = player_info.join(all_combine_data)

    #Create a temporary column to do a basic sum of the percentiles to evaluate player performance at the combine
    all_data['PERFORMANCE'] = all_data['HEIGHT_WO_SHOES_PTILE'] + all_data['WINGSPAN_PTILE'] + all_data['STANDING_REACH_PTILE'] + +all_data['STANDING_VERTICAL_LEAP_PTILE'] + all_data['MAX_VERTICAL_LEAP_PTILE'] + all_data['LANE_AGILITY_TIME_PTILE'] + all_data['THREE_QUARTER_SPRINT_PTILE']

    #Sort by the performance column and then drop from the dataset
    all_data.sort_values(by=['SEASON', 'PERFORMANCE'], ascending=[False, False], inplace=True)
    all_data.drop('PERFORMANCE',axis=1, inplace=True)

    #Create an empty data dictionary to prepare for pulling in 'School' and 'Country' for each player from the NBA API
    data_dict = {}

    #Count the total number of rows to track progress of pulling info from NBA API
    rows = len(all_data['PLAYER_ID'])

    #Loop through all player ids in the dataset and add their school and country to data_dict
    for index, player_id in enumerate(all_data['PLAYER_ID']):
        print("Retrieving info for player ", player_id, " (", index+1, "of", rows, ")")
        try:      
            info = commonplayerinfo.CommonPlayerInfo(player_id=player_id)
            temp_dict = info.get_dict()
            school = temp_dict['resultSets'][0]['rowSet'][0][8]
            country = temp_dict['resultSets'][0]['rowSet'][0][9]
            data_dict[player_id] = {"School": school, "Country": country}
            print("Successfuly pulled Player ID ", player_id, '\n')
            time.sleep(0.5)
                
        except:
            print("Failed to pull Player ID", player_id, '\n')

    #Convert data_dict to a dataframe and merge with all_data
    info_df = pd.DataFrame.from_dict(data_dict, orient='index')
    all_data = pd.merge(all_data, full_data, how = 'left', on = ['SEASON', 'PLAYER_ID'])
    all_data = pd.merge(all_data, info_df, how = 'left', right_index = True, left_on = 'PLAYER_ID')

    #Export to CSV
    all_data.to_csv('nba_draft_combine_ptiles.csv')

if __name__ == "__main__":
    main()