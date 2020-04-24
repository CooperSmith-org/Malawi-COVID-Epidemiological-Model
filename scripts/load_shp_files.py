## load shp files

from os.path import join, dirname, abspath
import pandas as pd
import geopandas as gpd

### fancy way of setting working directory
DATA_FOLDER = join(dirname(abspath(dirname(__file__))), "data")
OUTPUT_FOLDER = join(dirname(abspath(dirname(__file__))), "output")
SHAPE_FILES = ["mwi_admbnda_adm2_nso_20181016.shp", "mwi_admbnda_adm3_nso_20181016.shp"]
CURRENT_INFECTIONS = "CurrentInfectionLocation.csv"

### setting simulation parameters
ADJ_MULTIPLIER = .6
SECOND_ADJ_MULTIPLIER = .2

### 7 adm3s appear not to have any boarders?


class TA(object):

	def __init_(self, name, adm3_to_adm2, adm3_to_adm3, CI):
		'''
		name (string): name of TA in english
		adm3_to_adm2 (dict): dict mapping name to adjacent districts
		adm3_to_adm2 (dict): dict mapping name to adjacent TAs
		CI (int): number of current infections

		'''

		self.name = adm3
		self.CI = CI

		self.adj_adm2_list = adm3_to_adm2.get(adm3, [])
		self.adj_adm3_list = adm3_to_adm3.get(adm3, [])
		self.contributions_adj1 = None
		self.contributions_adj2 = None


	def get_adj1_edges(self, CI_series):

		contributions = []

		for district in self.adj_adm2_list:
 
			c = Contribution(self.name, district, 1, CI_series.loc[district].item())
			contributions.append(c)

		self.contributions_adj1 = contributions

		return contributions


class Contribution(object):

	def __init__(self, adm3, adm2, degree, contribution):

		self.adm3 = adm3
		self.adm2 = adm2
		self.degree = degree
		self.contribution = contribution


def go():

	adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = create_relations()
	CI = import_current_infections()

	output = adm3_homes.copy(deep=True)
	output = output.merge(CI, how='left', left_on="ADM2", right_index=True)

	TA_dict = build_TAs(df, adm3_to_adm2_dict, adm3_to_adm3_dict)




	return output



	# for row in output.itertuples(index=False, name=False):



	
	output.to_csv(join(OUTPUT_FOLDER, "test.csv"), index=False)

	return output




def build_TAs(df, adm3_to_adm2_dict, adm3_to_adm3_dict, CI):

	TA_dict = {}

	for amd2, adm3, CI in df.itertuples(index=False, name=None):

		num_infected = CI.loc[adm2].item()
		t = TA(adm3, adm3_to_adm2_dict, adm3_to_adm3_dict, num_infected)
		TA_dict[adm3] = t

	return TA_dict 
		# c.to_list()




def find_adjacencies(row, adm3_to_adm2_dict, adm3_to_adm3_dict, CI):

	adm3 = row["ADM3"]
	# print("checking adm3: {}".format(adm3))
	first_adj_score, prev_counted = find_first_adj(adm3, adm3_to_adm2_dict, CI, multiplier=.6)
	print(adm3, first_adj_score)

	second_adj_score = find_second_adj(adm3, adm3_to_adm3_dict, adm3_to_adm2_dict, CI, prev_counted)

	row["1st Adjacent Score"] = first_adj_score
	row["2nd Adjacent Score"] = second_adj_score
	row["Total Score"] = first_adj_score + second_adj_score

	return row


def find_first_adj(adm3, adm3_to_adm2_dict, CI, multiplier=1, prev_counted=set()):

	print("\nin first_adj")
	print("searching for {}...".format(adm3))
	score = 0
	print("prev_counted:", prev_counted)

	try:		
		adm2_list = adm3_to_adm2_dict[adm3]
		print("adm2_list:", adm2_list)

	except KeyError:
		# print("    no adjacencies")
		return 0, []

	for adm2 in adm2_list:
		print("    adm2: {}".format(adm2))

		if adm2 in prev_counted:
			print("    already counted")
			continue

		try:
			inf = CI.loc[adm2].item()
			print("    counted {} cases in {}".format(inf, adm2))
			prev_counted.add(adm2)
			
		except KeyError:
			inf = 0

		score += inf

	# print("    returning score: {} from {}".format(score, prev_counted))
	return score * multiplier, prev_counted


def find_second_adj(adm3, adm3_to_adm3_dict, adm3_to_adm2_dict, CI, prev_counted=set()):
 
	# print("    second adj for {}".format(adm3))

	total_score = 0

	try:		
		adm3_list = adm3_to_adm3_dict[adm3]
		print("adm3_list:", adm3_list)

	except KeyError:
		# print("    no adjacent adm3s")
		return 0

	for adj_adm3 in adm3_list:

		print("doing adjacent adm3: {}".format(adj_adm3))
		score, adm2s = find_first_adj(adj_adm3, adm3_to_adm2_dict, CI, multiplier=.2, prev_counted=prev_counted)
		total_score += score
		prev_counted.union(adm2s)
		print("\n")

	# print("    returning total score: {}".format(total_score))
	return total_score, prev_counted


def create_relations():

	### adm2 file
	adm2 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[0]))  ## load file
	adm2 = adm2[["ADM2_EN", "geometry"]].rename({"ADM2_EN": "ADM2"}, axis=1) ## subset and rename cols

	### adm3 file
	adm3 = gpd.read_file(join(DATA_FOLDER, SHAPE_FILES[1]))
	adm3 = adm3[["ADM3_EN", "ADM2_EN", "geometry"]].rename({"ADM3_EN": "ADM3", \
		"ADM2_EN": "ADM2"}, axis=1)
	adm3_homes = adm3[["ADM2", "ADM3"]]

	### connect adm3s to 2s
	tmp = gpd.sjoin(adm3, adm2, how='left', op='intersects') ## spacial join
	adm3_to_adm2 = tmp[tmp["ADM2_left"] != tmp["ADM2_right"]] ## filter out matches with home adm
	adm3_to_adm2_dict = df_to_dict(adm3_to_adm2[["ADM3", "ADM2_right"]])

	### connect adjacent adm3s
	tmp = gpd.sjoin(adm3, adm3, how="left", op='intersects')
	# print(tmp.columns)
	adm3_to_adm3 = tmp.loc[tmp["ADM3_left"] != tmp["ADM3_right"], ["ADM3_left", "ADM3_right"]]
	adm3_to_adm3_dict = df_to_dict(adm3_to_adm3[["ADM3_left", "ADM3_right"]])

	return adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict


def import_current_infections():

	tmp = pd.read_csv(join(DATA_FOLDER, CURRENT_INFECTIONS))
	CI = tmp[["ADM2_EN", "Current Infections"]].rename({"ADM2_EN": "ADM2"}, axis=1)

	return CI.set_index("ADM2")


def df_to_dict(df):
	'''
	Creates a dictionary from a df with 2 columns
	First column becomes key, second becomes value
	'''

	d = {}

	for k, v in df.itertuples(index=False, name=None):
		d[k] = d.get(k, []) + [v]

	return d


# if __name__ == "__main__":

# 	create_shps()