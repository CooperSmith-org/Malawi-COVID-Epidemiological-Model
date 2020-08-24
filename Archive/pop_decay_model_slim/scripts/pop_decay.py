import numpy as np
import pandas as pd
import geopandas as gpd



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


def convert_adj_matrix_to_multipliers(adj_matrix, multipliers):
	"""
	replaces adjacency numbers with multiplier values
	"""

	adj_matrix.replace(multipliers, inplace=True)
	adj_matrix.fillna(0, inplace=True)
	adj_matrix[adj_matrix > 1] = 0
	return adj_matrix


def calculate_decay(adj_matrix, CI, multipliers):
	"""
	right multiplies adj_matrix (DataFrame) by CI (Series)
	returns product as pd.Series
	"""

	## convert adj_matrix to multipliers
	adj_matrix = convert_adj_matrix_to_multipliers(adj_matrix, multipliers)
	rv = adj_matrix @ CI ## @ operator is matrix multiplication for pandas
	return rv


def find_connections(adm3, adm3_to_adm3, degree, iteration=1):
	'''
	Recursively builds list of connections to the nth degree
	Inputs:
		adm3 (string):  TA for which connections are being searched
		adm3_to_adm2 (dict): keys - list of adm3s, values - list of adj adm2s
		adm3_to_adm3 (dict): keys - list of adm3s, values - list of adj adm3s
		degree (int): maximum number of adj TAs we are searching
		interation (int): tracks which degree is being calculated
	'''

	adm3_list = adm3_to_adm3.get(adm3, [])
	connections = []

	if iteration == degree:
		connections += [(adj_adm3, iteration) for adj_adm3 in adm3_list if adm3 != adj_adm3]

	else:
		for adj_adm3 in adm3_to_adm3.get(adm3, []):
			if adm3 == adj_adm3:
				continue
			connections += [(adj_adm3, iteration)]
			connections += find_connections(adj_adm3, adm3_to_adm3, degree, iteration=iteration+1)

		### add adm3 to list if first columns
		if iteration == 1:
			min_connects = {}
			for c in connections:
				min_connects[c[0]] = min(min_connects.get(c[0], c[1]), c[1])
				# final_connections.append((adm3, c[0], c[1]))
			connections = {adm3: min_connects}

	return connections


def create_adj_dict(shape, exclusions, bundles, consolidations):
	"""
	adj_num (int): number of adjacent tiles
	"""

	reg_map = shape[['ADM2_PCODE', 'ADM3_PCODE', 'geometry']]

	df = apply_exclusions(reg_map, exclusions)
	df = apply_consolidations(df, consolidations)
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