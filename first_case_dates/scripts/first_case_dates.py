import pandas as pd
import geopandas as gpd
import datetime as dt
import numpy as np
from os.path import join

SHAPE_PATH = "../static/shape/mwi_admbnda_adm3_nso_20181016.shp"
EXCLUSIONS = {"ADM3_PCODE": (["MW20115", "MW30399", "MW30904", "MW30299",
			"MW20511", "MW30199", "MW31009", "MW31305",
			"MW30807", "MW31110", "MW20207", "MW10106",
			"MW10206", "MW10410", "MW10511", "MW10411"])}
BUNDLES = {"TO_BUNDLE": "ADM2_PCODE",
			"REGIONS": (["MW210", "MW315", "MW314", "MW107"]),
			"SUBREGION": "ADM3_PCODE"}
# CONSOLIDATIONS = {"MW315": "MW305",
# 				"MW210": "MW206",
# 				"MW107": "MW105",
# 				"MW314": "MW303"}


TAs =  (["MW31204", "MW30507", "MW31001",
		"MW30401", "MW10101", "MW20806",
		"MW20420", "MW10220", "MW20101",
		"MW210", "MW30222", "MW30120",
		"MW20705", "MW30801", "MW30601",
		"MW107", "MW31302",
		"MW10302", "MW20204", "MW31104",
		"MW20920", "MW30902", "MW10403",
		"MW20504", "MW30708", "MW314"])

ADJACENCY_MATRIX = 'adj_matrix.csv'
FIRST_CASES_DATA = "TA_first_cases.csv"
TA_NAME_MAP = "ta_name_map.csv"
TA_LVL_MAP = "ta_lvl_map.csv"
LVLS_MAP = "lvlmap.csv"
REPLACEMENTS = ({"MW10739": "MW107", "MW30306": "MW314"})
OUTPATH = "../first_case_output"


def offset_date(date, offset, x):
	return date + pd.DateOffset(days=(x * offset))

def get_source(row, df):

	source_row = df.loc[row['ADM3_PCODE']]
	for date, source in zip(source_row, source_row.index):
		if date == row['first_case_date']:
			return source


def calculate_dates(adj_matrix, first_case_data, offset):

	start_date_list = []

	### calculate offset
	for row in first_case_data.itertuples():
		start_dates = adj_matrix[row.ta].apply(lambda x: offset_date(row.date, offset, x))
		start_date_list.append(start_dates)

	### find minimum offset
	start_date_df = pd.concat(start_date_list, axis=1)
	first_dates = start_date_df.min(axis=1).reset_index()
	first_dates.columns = ['ADM3_PCODE', 'first_case_date']

	first_dates = first_dates.merge(first_case_data, left_on='ADM3_PCODE', right_on='ta', how='left')
	first_dates.loc[first_dates['ta'].notnull(), 'first_case_date'] = \
		first_dates.loc[first_dates['ta'].notnull(), 'date']
	first_dates.drop(['ta', 'date'], axis=1, inplace=True)

	### find source
	first_dates['source'] = first_dates.apply(lambda row: get_source(row, start_date_df), axis=1)

	return first_dates


def reformat_for_output(mw_shape, names, first_dates):

	merged = first_dates.merge(mw_shape[['ADM2_PCODE', 'ADM3_PCODE', 'ADM3_EN']], on='ADM3_PCODE', how='left')
	merged.loc[merged['ADM2_PCODE'].isnull(), 'ADM2_PCODE'] = merged.loc[merged['ADM2_PCODE'].isnull(), 'ADM3_PCODE']
	merged = merged.merge(mw_shape[['ADM2_PCODE', 'ADM2_EN']].drop_duplicates(ignore_index=True), on='ADM2_PCODE', how='left')
	# out = pd.concat([first_dates, mw], axis=1)

	merged.loc[merged['first_case_date'] > dt.datetime(2021, 1, 1), 'first_case_date'] = np.nan

	### merge on names
	final_df = merged.merge(names, how="left", on="ADM3_PCODE")

	final_df = final_df[['UID', 'Lvl1', 'Lvl2', 'Lvl3', 'Lvl4', 'first_case_date']]
	final_df.rename({"first_case_date": "Start Date"}, axis=1, inplace=True)
	min_date = final_df['Start Date'].min()
	final_df['Date_from_0'] = (final_df['Start Date'] - min_date) / np.timedelta64(1, 'D')
	final_df.sort_values('UID', inplace=True)

	return final_df


def main():

	### load files
	adj_matrix = pd.read_csv(ADJACENCY_MATRIX, index_col=0)
	np.fill_diagonal(adj_matrix.values, 0)

	adj_matrix.fillna(1000, inplace=True)
	first_case_data = pd.read_csv(FIRST_CASES_DATA, parse_dates=['date'])
	first_case_data.replace(REPLACEMENTS, inplace=True)

	mw_shape = gpd.read_file(SHAPE_PATH)[['ADM2_PCODE', 'ADM3_PCODE', 'ADM3_EN', 'ADM2_EN']]
	names = pd.read_csv(TA_LVL_MAP)
	
	for offset in range(1, 51):
		print("running for offset", offset)
		first_dates = calculate_dates(adj_matrix, first_case_data, offset)
		final_df = reformat_for_output(mw_shape, names, first_dates)
		final_df.to_csv(join(OUTPATH, 'first_dates_offset_param_{}.csv'.format(offset)), index=False)


	# start_date_list = []

	# ### calculate offset
	# for row in first_case_data.itertuples():
	# 	start_dates = adj_matrix[row.ta].apply(lambda x: offset_date(row.date, offset, x))
	# 	start_date_list.append(start_dates)

	# ### find minimum offset
	# start_date_df = pd.concat(start_date_list, axis=1)
	# # return start_date_df
	# first_dates = start_date_df.min(axis=1).reset_index()
	# first_dates.columns = ['ADM3_PCODE', 'first_case_date']

	# first_dates = first_dates.merge(first_case_data, left_on='ADM3_PCODE', right_on='ta', how='left')
	# first_dates.loc[first_dates['ta'].notnull(), 'first_case_date'] = \
	# 	first_dates.loc[first_dates['ta'].notnull(), 'date']
	# first_dates.drop(['ta', 'date'], axis=1, inplace=True)

	# ### find source
	# first_dates['source'] = first_dates.apply(lambda row: get_source(row, start_date_df), axis=1)
	# # return first_dates
	# # first_dates = df.min(axis=1).reset_index()


	# merged = first_dates.merge(mw[['ADM2_PCODE', 'ADM3_PCODE', 'ADM3_EN']], on='ADM3_PCODE', how='left')
	# merged.loc[merged['ADM2_PCODE'].isnull(), 'ADM2_PCODE'] = merged.loc[merged['ADM2_PCODE'].isnull(), 'ADM3_PCODE']
	# merged = merged.merge(mw[['ADM2_PCODE', 'ADM2_EN']].drop_duplicates(ignore_index=True), on='ADM2_PCODE', how='left')
	# # out = pd.concat([first_dates, mw], axis=1)

	# merged.loc[merged['first_case_date'] > dt.datetime(2021, 1, 1), 'first_case_date'] = np.nan

	# ### merge on names
	# final_df = merged.merge(names, how="left", on="ADM3_PCODE")

	# final_df = final_df[['UID', 'Lvl1', 'Lvl2', 'Lvl3', 'Lvl4', 'first_case_date']]
	# final_df.rename({"first_case_date": "Start Date"}, axis=1, inplace=True)
	# min_date = final_df['Start Date'].min()
	# final_df['Date_from_0'] = (final_df['Start Date'] - min_date) / np.timedelta64(1, 'D')
	# final_df.sort_values('UID', inplace=True)

	# return final_df


def execute_adj_matrix(degree):

	mw = gpd.read_file(SHAPE_PATH)
	adj_dict = create_adj_dict(mw, EXCLUSIONS, BUNDLES)
	adj_matrix = create_adj_matrix(adj_dict, degree)

	return adj_matrix



def create_adj_matrix(adj_dict, degree):
	"""
	creates adjacency matrix from adj_dict
	inputs:
		adj_dict (dict) where keys are TAs and values are lists of adjacent
		TAs
		degree (int) is the maximum number of adjacent TAs to search
	returns pandas df
	"""

	matrix = {}
	for adm3 in adj_dict.keys():
		print(adm3)
		matrix.update(find_connections(adm3, adj_dict, degree))

	matrix = sort_df(pd.DataFrame(matrix))

	return matrix


def sort_df(df):
	"""
	sorts columns and index for df with same items in columns and indices
	"""

	df = df.sort_index()
	df = df[df.index]
	return df


def find_connections(adm3, adm3_to_adm3, degree, iteration=1, do_not_go=set()):
	'''
	Recursively builds list of connections to the nth degree
	Inputs:
		adm3 (string):  TA for which connections are being searched
		adm3_to_adm2 (dict): keys - list of adm3s, values - list of adj adm2s
		adm3_to_adm3 (dict): keys - list of adm3s, values - list of adj adm3s
		degree (int): maximum number of adj TAs we are searching
		interation (int): tracks which degree is being calculated
	'''
	# print()
	# print("  " * iteration + "find_connections called")
	# print("  " * iteration + "adm3: {}; iteration: {}".format(adm3, iteration))

	if iteration == 1:
		do_not_go = set([adm3])
	adm3_list = adm3_to_adm3.get(adm3, [])
	# already_connected.add(adm3)
	connections = []

	if iteration == degree:
		connections += [(adj_adm3, iteration) for adj_adm3 in adm3_list if adj_adm3 not in do_not_go]


	else:
		# do_not_go_list = []
		# for TA in from_TAs:
		# 	do_not_go_list.extend(adm3_to_adm3[])
		for adj_adm3 in adm3_to_adm3.get(adm3, []):
			# print("  " * iteration + "adj_adm3: {}".format(adj_adm3))
			# print("  " * iteration + str(do_not_go))
			if adj_adm3 in do_not_go:

			# if adj_adm3 in already_connected:
				# print("  " * iteration + "not going...")
			# if adm3 == adj_adm3 or adj_adm3 in already_connected:
				continue
			connections += [(adj_adm3, iteration)]
			do_not_go_new = do_not_go.copy()
			do_not_go_new | set(adm3_to_adm3[adm3])
			# print()
			connections += find_connections(adj_adm3, adm3_to_adm3, degree, iteration=iteration+1, do_not_go=do_not_go_new)

		### add adm3 to list if first columns
		if iteration == 1:
			min_connects = {}
			for c in connections:
				min_connects[c[0]] = min(min_connects.get(c[0], c[1]), c[1])
				# final_connections.append((adm3, c[0], c[1]))
			connections = {adm3: min_connects}

	# print()

	return connections


def create_adj_dict(shape, exclusions, bundles):
	"""
	adj_num (int): number of adjacent tiles
	"""

	reg_map = shape[['ADM2_PCODE', 'ADM3_PCODE', 'geometry']]

	df = apply_exclusions(reg_map, exclusions)
	# df = apply_consolidations(df, consolidations)
	df = apply_bundles(df, bundles)

	### connect adjacent adm3s
	adj = spatial_selfjoin(df, 'ADM3_PCODE')
	adj_dict = df_to_dict(adj)

	return adj_dict


def spatial_selfjoin(df, col):
	"""
	returns df with adjacent pairs of TAs
	"""

	tmp = gpd.sjoin(df[[col, 'geometry']], df[[col, 'geometry']], how="left", 
		op='intersects', lsuffix='orig', rsuffix='adj')
	adj = tmp.loc[tmp[col + '_orig'] != tmp[col + '_adj'], [col + '_orig', col + '_adj']]
	adj = adj[[col + '_orig', col + '_adj']].drop_duplicates(ignore_index=True)
	return adj


def apply_exclusions(df, exclusions):

	reg_name = list(exclusions.keys())[0]
	reg_list = list(exclusions.values())[0]
	df = df.loc[~df[reg_name].isin(reg_list), :]

	return df


def apply_bundles(df, bundles):

	reg_name = bundles['TO_BUNDLE']
	reg_list = bundles['REGIONS']
	subreg_name = bundles['SUBREGION']
	df.loc[df[reg_name].isin(reg_list), subreg_name] = \
	df.loc[df[reg_name].isin(reg_list), reg_name]

	return df


def apply_consolidations(df, consolidations):

	df.replace(consolidations, inplace=True)
	return df


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}
	for k, v in df.itertuples(index=False, name=None):
		d[k] = d.get(k, []) + [v]

	return d
