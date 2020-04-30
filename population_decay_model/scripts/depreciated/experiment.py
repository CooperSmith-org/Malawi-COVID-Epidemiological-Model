import data_loading
import pandas as pd

MULTIPLIER_DICT = {1: .6, 2: .2}


def go(degree=2, multiplier_dict=MULTIPLIER_DICT):

	assert len(MULTIPLIER_DICT) == degree, ("multiplier_dict length must equal degree")

	print("loading files...")
	CI, adm3_homes, adm3_to_adm2, adm3_to_adm3 = import_files()
	# print(CI.head())
	# return None

	print("building connections...")
	total_cs = pd.DataFrame(columns=["ADM2", "Degree", "ADM3", "Current Infections"])

	for adm3 in adm3_to_adm2.keys():

		c = find_connections(adm3, adm3_to_adm2, adm3_to_adm3, degree)
		c_df = pd.DataFrame(c, columns=["ADM2", "Degree"])
		min_c = c_df.groupby("ADM2").min()
		min_c["ADM3"] = adm3

		merged = min_c.merge(CI, how="left", left_index=True, right_index=True)
		total_cs = total_cs.append(merged.reset_index())

	print("calculating scores...")
	for k, v in multiplier_dict.items():
		total_cs.loc[total_cs["Degree"]==k, "Multiplier"] = v

	total_cs["Contribution"] = total_cs["Multiplier"] * total_cs["Current Infections"]

	scores = total_cs.groupby("ADM3").sum()

	return total_cs, scores
	# total_cs["Multiplier"] = .6  ### build-out variability
	# total_cs.loc[]["Multiplier"]




	


def import_files():

	adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict = data_loading.create_relations()
	CI = data_loading.import_current_infections()

	# output = adm3_homes.copy(deep=True)
	# output = output.merge(CI, how='left', left_on="ADM2", right_index=True)

	return CI, adm3_homes, adm3_to_adm2_dict, adm3_to_adm3_dict



def find_connections(adm3, adm3_to_adm2, adm3_to_adm3, degree, iteration=1):

	## base case

	adm2_list = adm3_to_adm2.get(adm3, [])

	if iteration == degree:
		return [(adm2, iteration) for adm2 in adm2_list]


	else:
		### take last
		connections = [(adm2, iteration) for adm2 in adm2_list]

		for adm3 in adm3_to_adm3.get(adm3, []):
			connections += find_connections(adm3, adm3_to_adm2, adm3_to_adm3, degree, iteration=iteration+1)

		return connections

# def get_CIs(connectsion):




