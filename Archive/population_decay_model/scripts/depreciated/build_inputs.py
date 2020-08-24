import json
import pandas as pd
import geopandas as gpd
from os.path import join, abspath, dirname

"""
To do
Funciton that builds bundles for each bundle file
Function that build exclusions for each exclusion file
Functions that ensure integrity of column names (make sure entered names are in files)
"""


EXCLUSION_DICT = {
	"MW": ['MW20115', 'MW30399', 'MW30904', 'MW30299', 'MW20511',
		   'MW30199', 'MW31009', 'MW31305', 'MW30807', 'MW31110', 
		   'MW20207', 'MW10106', 'MW10206', 'MW10410', 'MW10511', 
		   'MW10411']
				  }

BUNDLED_DICT = {"MW":
				{"smaller_region_name": "ADM3_PCODE",
				"larger_region_name": "ADM2_PCODE", 
				"list": ["MW210", "MW315", "MW314", "MW107"]}}

INT_FOLDER = join(dirname(abspath(dirname(__file__))), "country_inputs", "MW", "intermediate_data")
GEO_FOLDER= join(dirname(abspath(dirname(__file__))), "country_inputs", "MW", "geography")
CI_FOLDER = join(dirname(abspath(dirname(__file__))), "country_inputs", "MW", "CI")



### for testing
SHAPE_FILENAME = "mwi_admbnda_adm3_nso_20181016.shp"
CI_FILENAME = "CurrentInfectionLocation_30April20 - Copy.csv"

def test_folders():

	for f in [DATA_FOLDER, INPUTS_FOLDER, INT_FOLDER, GEO_FOLDER]:
		print(f)

#comment
def create_geo_ints(country, shape_filename, id_geo_col, CI_geo_col):

	### in the future a different file will collect these things for us
	exclusion_list = EXCLUSION_DICT.get(country, [])
	bundled_data = BUNDLED_DICT.get(country, {})
	is_bundled = True

	### upload full dataset
	df = gpd.read_file(join(GEO_FOLDER, shape_filename))
	df.name = "full"

	### split based on exclusions
	print(exclusion_list)
	df_cut = df[df[id_geo_col].isin(exclusion_list) == False]
	df_cut.name = "excluded"

	### split based on bundles
	dfs = [df, df_cut]
	print("len df:", df.shape)
	print("len cut:", df_cut.shape)

	if is_bundled:
		r_small = bundled_data["smaller_region_name"]
		r_big = bundled_data["larger_region_name"]
		bundled_list = bundled_data["list"]

		for d in [df, df_cut]:  ### not looping through dfs so I can append to it
			new_d = d.copy(deep=True)			
			new_d.loc[d[r_big].isin(bundled_list), r_small] = \
				d.loc[d[r_big].isin(bundled_list), r_big]
			new_d.name = d.name + "_bundled"
			dfs.append(new_d)

	### get adjacency stuff
	for d in dfs:
		print("making files for...", d.name)

		### make geo_index_identities
		identities = d[[id_geo_col, CI_geo_col]].drop_duplicates(ignore_index=True) ### grabbing unique column names
		identities.rename({identities.columns[0]: 'region', identities.columns[1]: 'greater_region'}, inplace=True, axis=1)
		if all(identities['region'] == identities['greater_region']):
			identities = identities[id_geo_col]
		identities.to_json(join(INT_FOLDER, "ids_" + d.name + ".json"))
		output = find_adjacencies(d, id_geo_col)
		# print(output.columns)
		with open(join(INT_FOLDER, d.name + ".json"), 'w') as json_file:
			json.dump(output, json_file)







def main(country, shape_filename, id_geo_col, CU_geo_col):
	"""
	populates int_folder with necessary files
	"""

	create_geo_dicts(country, shape_filename, id_geo_col, CU_geo_col)

	### load CIs
	CIs = import_CIs(join(INPUTS_FOLDER, CI_FILENAME), "ADM2_PCODE", df, "ADM3_PCODE")
	print("CIs:\n{}".format(CIs))
	CIs.to_json(join(INT_FOLDER, "CI.json"))








def import_shape(shape_filename, id_geo_col, CI_geo_col=None):

	if not CI_geo_col:
		CI_geo_col = id_geo_col

	df = gpd.read_file(join(DATA_FOLDER, shape_filename))	



def import_CIs(CI_filepath, CI_geo_id_name, ids_df, geo_id_name):
	"""
	reads CI file and merges onto df
	inputs:
		CI_filepath (string): path to CI csv
		CI_geo_id_name (string):  name of column that refers to geography
		ids_df (pd.DataFrame): maps CI_geo_id_name to geographic level of interest
		geo_id_name (string):  column that uniquely identifies region 
		corresponding to CI_geo_id_name
	"""

	CI = pd.read_csv(join(DATA_FOLDER, CI_filepath))

	# tmp.loc[tmp['ADM2_PCODE'].isin(BUNDLED_CITIES), 'ADM3_PCODE'] = \
	# tmp.loc[tmp['ADM2_PCODE'].isin(BUNDLED_CITIES), 'ADM2_PCODE']

	CI = CI[[CI_geo_id_name, "Current Infections"]]
	# rv = ids_df.merge(CI, how="left", left_on=geo_id_name, right_on=CI_geo_id_name)

	return CI


def find_adjacencies(df, geo_id_name):
	"""
	inputs:
		df (pd.DataFrame):  contains geo_id_name and polygons
		geo_id_name (string):  column that uniquely identifies region
	returns:  dictionary of region: list of adjacent regions
	"""

	tmp = gpd.sjoin(df, df, how="left", op='intersects')
	tmp = tmp.loc[tmp[geo_id_name + "_left"] != tmp[geo_id_name + "_right"], 
		[geo_id_name + "_left", geo_id_name + "_right"]]
	tmp.drop_duplicates(ignore_index=True, inplace=True)
	rv = df_to_dict(tmp[[geo_id_name + "_left", geo_id_name + "_right"]])

	# print("output for df {}\n{}".format(df.name, df))

	return rv


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}
	# d_bundled = {}


	for k, v in df.itertuples(index=False, name=None):
		d[k] = d.get(k, []) + [v]

	return d