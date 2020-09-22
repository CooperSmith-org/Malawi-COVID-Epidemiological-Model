"""
Copied almost entirely from:
https://towardsdatascience.com/how-to-access-google-sheet-data-using-the-python-api-and-convert-to-pandas-dataframe-5ec020564f0e
"""

from __future__ import print_function
from apiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
import pandas as pd
import geopandas as gpd


SPREADSHEET_ID = "1mebqhggUF38iz-2n4nFxQ7nZHfuafYRyz2ElcNgbc2o" ## from url
RANGE_NAME = "Confimed Aggregates!A:O" ## worksheet name


def get_google_sheet(spreadsheet_id, range_name):
    """ Retrieve sheet data using OAuth credentials and Google Python API. """
    scopes = 'https://www.googleapis.com/auth/spreadsheets.readonly'
    # Setup the Sheets API
    store = file.Storage('../secrets/credentials.json')
    creds = store.get()
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets('../secrets/client_secret.json', scopes)
        creds = tools.run_flow(flow, store)
    service = build('sheets', 'v4', http=creds.authorize(Http()))

    # Call the Sheets API
    gsheet = service.spreadsheets().values().get(spreadsheetId=spreadsheet_id, range=range_name).execute()
    return gsheet


def to_pandas(gsheet):
    """
    Converts dictionary returned by get_google_sheet
    to pandas df
    Returns empty data frame if gsheet has no values argument
    """

    data = gsheet.get('values', [])
    # df = pd.DataFrame(columns=data[0])
    d = {c: [] for c in data[0]}
    for row in data[1:]:
        if "Total" not in row:
            for i, c in enumerate(data[0]):
                if i < len(row):
                    d[c].append(row[i])
                else:
                    d[c].append(None)
    df = pd.DataFrame(d)

    return df


def expand_CI_to_ADM3(shape, CI, bundles):
    """
    associates CI files with TAs corresponding to districts
    """

    gdf = shape[['ADM2_PCODE', 'ADM3_PCODE']]
    gdf = gdf.merge(CI, how='inner', left_on='ADM2_PCODE', right_on='ADM2_PCode')
    gdf.loc[gdf[bundles['TO_BUNDLE']].isin(bundles['REGIONS']), bundles['SUBREGION']] = \
        gdf.loc[gdf[bundles['TO_BUNDLE']].isin(bundles['REGIONS']), bundles['TO_BUNDLE']]

    return gdf[['ADM3_PCODE', 'Confirmed']].set_index('ADM3_PCODE')


def main(shape, bundles):
    """
    Retrieves dataframe
    """
    gsheet = get_google_sheet(SPREADSHEET_ID, RANGE_NAME)
    df = to_pandas(gsheet)
    df.loc[df['Confirmed']=='', 'Confirmed'] = '0'
    df['Confirmed'] = df['Confirmed'].str.extract('(\d+)').astype('int')
    df = df.loc[df['ADM2_PCode'].notnull(), ['Confirmed', 'ADM2_PCode']]
    df = df.groupby('ADM2_PCode')['Confirmed'].sum().reset_index()
    df = expand_CI_to_ADM3(shape, df, bundles)

    return df