import pandas as pd


def main(ids, adj_dict, degree, multiplier_dict, CI):

	### to manage merges if CI is at greater geography
	merge_col =  "region" if ids.shape[1] == 1 else "greater_region"

	total_cs = get_connections(adj_dict, degree)

	tmp = total_cs.merge(ids, how="inner", left_on="adj_region", right_on=ids.columns[0])
	tmp = tmp.drop("region_y", axis=1).rename({"region_x": "region"}, axis=1)
	tmp = tmp.merge(CI, how='left', left_on=merge_col, right_on=CI.columns[0])
	tmp = tmp.drop("CI_region", axis=1)

	for k, v in multiplier_dict.items():
		tmp.loc[tmp["degree"] == k, "multiplier"] = v

	tmp["calculated_score"] = tmp["multiplier"] * tmp["CI_count"]
	scores = tmp.groupby("region")["calculated_score"].sum().reset_index()

	scores = ids.merge(scores, how="inner", left_on=ids.columns[0], right_on="region")

	scores = scores.merge(CI, how="left", left_on=merge_col, right_on=CI.columns[0])
	scores.drop(merge_col, axis=1, inplace=True)
	# return scores
	scores['final_score'] = scores['calculated_score']
	scores.loc[scores['CI_count'] > 0, 'final_score'] = scores.loc[scores['CI_count'] > 0, 'CI_count']

	return scores, total_cs


# def calc_scores(total_cs, CI, multiplier_dict, merge_col):
# 	"""
# 	total_cs (pd.DataFrame): 
# 	CI (pd.Series):

# 	"""
# 	for k, v in multiplier_dict.items():
# 		total_cs.loc[total_cs["degree"] == k, "multiplier"] = v

# 	total_cs = total_cs.merge(CI, how='left', left_on="adj_" + merge_col, right_on=CI.columns[0])
# 	total_cs["calculated_score"] = total_cs["multiplier"] * total_cs["CI_count"]
# 	# print(calc_scores)

# 	scores = total_cs.groupby("region")["calculated_score"].sum().reset_index()
# 	# scores = ids.merge(scores, how='left', left_on="ADM3", right_on="ADM3")
# 	return scores
# 	scores = scores.merge(CI, how="left", left_on=merge_col, right_on=CI.columns[0])
# 	return scores
# 	scores['final_score'] = scores['calculated_score']
# 	scores.loc[scores['CI_count'] > 0, 'final_score'] = scores.loc[scores['CI_count'] > 0, 'CI_count']

# 	scores.fillna()
# 	# scores["Calculated Score"] = scores.apply(lambda row: check_for_CI(row, CI, adm3_homes), axis=1)
# 	# scores.loc[scores["Current Infections"] > 0] = scores.loc[scores["Current Infections"], 

# 	return scores


def get_connections(adj_dict, degree):
# def get_connections(adm3_list, adm3_to_adm3, CI, degree):
	"""
	Calls find_connections for each TA then products a df of the union of all
	connections with CI data merged on
	Inputs:
		adm3_list (list): list of unique adm3s
		adm3_to_adm2 (dict): maps TA to list of adjacent Districts
		adm3_to_adm3 (dict): maps TA to list of adjacent TAs
		CI (pd.Series): index is District and value is number of current infections
		degree (int): maximum degree of connections desired.  For example, enter one if
		only interested in infections in adjacent TAs.  Enter 2 if interested in TAs that
		border adjacent TAs, etc.
	Returns pd.DataFrame that lists all connections and their degree
	"""

	total_cs = pd.DataFrame(columns=["adj_region", "degree", "region"])

	for unit in list(adj_dict.keys()):

		c = find_connections(unit, adj_dict, degree)
		c_df = pd.DataFrame(c, columns=["adj_region", "degree"])
		min_c = c_df.groupby("adj_region").min()
		# print("max degree connection: {}".format(min_c.max()))
		min_c["region"] = unit

		# merged = min_c.merge(CI, how="left", left_index=True, right_index=True)
		total_cs = total_cs.append(min_c.reset_index())

	# total_cs = total_cs.groupby(["region", "degree"]).agg("count").reset_index()
	# total_cs = total_cs.pivot(index="region", columns="degree")
	# total_cs.columns = total_cs.columns.droplevel()

	# for i in total_cs.columns:
	# 	total_cs.rename({i: "degree_" + str(i) + "_count"}, axis=1, inplace=True)

	return total_cs


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

	return connections

