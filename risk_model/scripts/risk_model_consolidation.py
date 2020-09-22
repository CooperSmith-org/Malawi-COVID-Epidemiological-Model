import pandas as pd
import geopandas as gpd
from os.path import join
import sys

sys.path.append("../pop_decay_model/scripts/")

import drop_to_dropbox

ROOT = "../static"

CITIES = ["MW210", "MW107", "MW314", "MW315"]

def import_frame():

	df = pd.read_csv(join(ROOT, 'MW_Cleansed_Shell.csv'))
	df = df[['Code', 'Population']]

	return df


def prep_shape():

	mw = gpd.read_file("../static/shape/Malawi_TA_2018Census_FixedGeom.shp")
	mw = mw[['DIST_NAME', 'DIST_CODE', 'TA_NAME', 'TA_CODE']]
	mw['TA_CODE'] = mw['TA_CODE'].astype('object').str.strip()
	mw.loc[mw['DIST_CODE'].isin(CITIES), 'TA_CODE'] = mw.loc[mw['DIST_CODE'].isin(CITIES), 'DIST_CODE'].str[2:]
	mw.loc[mw['DIST_CODE'].isin(CITIES), 'TA_NAME'] = mw.loc[mw['DIST_CODE'].isin(CITIES), 'DIST_NAME']
	mw.drop_duplicates(inplace=True, ignore_index=True)
	# NAMES = {
	# 	"DIST_CODE": "ADM2_PCODE",
	# 	"DIST_NAME": "ADM2_EN",
	# 	"TA_CODE": "ADM3_PCODE", 
	# 	"TA_NAME": "ADM3_EN" 
	# 		}

	# mw.rename(NAMES, axis=1, inplace=True)
	# mw['ADM3_PCODE'] = mw['ADM3_PCODE'].apply(lambda x: "MW" + str(x))

	return mw


def import_pop_density():

	df = pd.read_csv(join(ROOT, 'PopulationDensity_TA_UNAdjPop.csv'))
	df = df[['TA_CODE', 'DIST_CODE', '_Popsum', 'Squarekm']]
	df.loc[df['DIST_CODE'].isin(CITIES), 'TA_CODE'] = df.loc[df['DIST_CODE'].isin(CITIES), 'DIST_CODE'].str[2:]
	df['TA_CODE'] = df['TA_CODE'].astype(str).str.strip()

	df = df.groupby('TA_CODE')['_Popsum', 'Squarekm'].sum().reset_index()
	df['pop_density'] = df['_Popsum'] / df['Squarekm']

	# df['Code'] = df['ADM3_PCODE']
	# df.loc[df['ADM2_PCODE'].isin(CITIES), 'Code'] = df['ADM2_PCODE']
	# return df
	return df[['TA_CODE', 'pop_density']]


def import_elderly():

	df = pd.read_csv(join(ROOT, 'ElderlyPop_2020_CensusShapefile.csv'))
	df = df[(['TA_CODE', 'DIST_CODE', '_Popsum', '60-64', '65-69',
		'70-74', '75-79', '80+', 'Pop>60'])]
	df['TA_CODE'] = df['TA_CODE'].astype(str).str.strip()

	df.loc[df['DIST_CODE'].isin(CITIES), 'TA_CODE'] = df.loc[df['DIST_CODE'].isin(CITIES), 'DIST_CODE'].str[2:]
	df = df.groupby('TA_CODE')['_Popsum', '60-64', '65-69', '70-74', '75-79', '80+'].sum().reset_index()
	df['pct_elderly'] = df[['60-64', '65-69', '70-74', '75-79', '80+']].sum(axis=1) / df['_Popsum']
	df = df[['TA_CODE', '_Popsum', 'pct_elderly']].rename({'_Popsum': 'population'}, axis=1)
	return df


def import_dry():

	df = pd.read_csv(join(ROOT, "DrySeason_PopMovemement_MNO_v3.csv"))
	df = df[['Code', 'Rainy_Season_Movement']]
	return df


def import_health():
	
	df = pd.read_csv(join(ROOT, 'HFCoverage_v3.csv'))
	df['pct_not_covered'] = df['%NotCovered'].astype('float')
	df.rename({'code': 'Code'}, axis=1, inplace=True, errors='raise')
	df = df[['Code', 'pct_not_covered']]
	return df


def pop_decay_model():

	df = pd.read_csv('../decay_model_output.csv')
	df['TA_CODE'] = df['TA_CODE'].str[2:]
	df.rename({'Cases': 'interpolated_CI'}, axis=1, inplace=True)
	df = df[['TA_CODE', 'interpolated_CI']]
	return df


def main():

	TAs = prep_shape()
	pop_density = import_pop_density()
	elderly = import_elderly()
	decay_model = pop_decay_model()

	dfs = [elderly, pop_density, decay_model]#pop_density, elderly]#, drymwd, health, decay_model]

	# merge everything together
	for df in dfs:
		TAs = TAs.merge(df, how='left', left_on='TA_CODE', right_on='TA_CODE')

	# calc norm
	for c in TAs.columns:
		if c not in ('TA_CODE', 'TA_NAME', 'DIST_CODE', 'DIST_NAME', 'population'):
			total = TAs[c].sum()
			TAs[c] = TAs[c].apply(lambda x: float(x)/total * 100)

	TAs['final_score'] = TAs["pop_density"] + TAs["pct_elderly"] + TAs["interpolated_CI"]

	TAs.to_csv("../risk_model_output/risk_model_output.csv", index=False)
	drop_to_dropbox.upload_risk_model(TAs, "../pop_decay_model/")

	# return TAs

if __name__ == '__main__':
	main()