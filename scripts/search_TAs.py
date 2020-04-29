import pandas as pd

def calc_scores(total_cs, CI, adm3_homes, multiplier_dict):
	"""
	total_cs (pd.DataFrame): 
	CI (pd.Series):

	"""
	for k, v in multiplier_dict.items():
		total_cs.loc[total_cs["Degree"] == k, "Multiplier"] = v

	total_cs["Calculated Score"] = total_cs["Multiplier"] * total_cs["Current Infections"]

	scores = total_cs.groupby("ADM3")["Calculated Score"].sum().reset_index()

	scores = adm3_homes.merge(scores, how='left', left_on="ADM3", right_on="ADM3")
	scores = scores.merge(CI, how="left", left_on="ADM2", right_index=True)
	scores['Final Score'] = scores['Calculated Score']
	scores.loc[scores['Current Infections'] > 0, 'Final Score'] = scores.loc[scores['Current Infections'] > 0, 'Current Infections']
	# scores["Calculated Score"] = scores.apply(lambda row: check_for_CI(row, CI, adm3_homes), axis=1)
	# scores.loc[scores["Current Infections"] > 0] = scores.loc[scores["Current Infections"], 

	return scores


def check_for_CI(row, CI, adm3_homes):
	"""
	Checks if there are > 0 current infections in home district.
	Returns the number number of infections if True, or the calculated
	score if False
	Inputs:
		row: row of pd.DataFrame with columns "ADM3" and "Score"
		CI (pd.Series): index is District and value is number of current infections
		adm3_homes (dict): keys are TAs and values are the associated District
	"""
	adm2 = adm3_homes.get(row["ADM3"], None)
	if adm2:
		num_infections = CI.loc[adm2].item()
		if num_infections > 0:
			return num_infections
		else:
			return row["Score"]
	else:
		return row["Score"]


def get_connections(adm3_list, adm3_to_adm2, adm3_to_adm3, CI, degree):
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

	total_cs = pd.DataFrame(columns=["ADM2", "Degree", "ADM3", "Current Infections"])

	for adm3 in adm3_list:

		c = find_connections(adm3, adm3_to_adm2, adm3_to_adm3, degree)
		c_df = pd.DataFrame(c, columns=["ADM2", "Degree"])
		min_c = c_df.groupby("ADM2").min()
		min_c["ADM3"] = adm3

		merged = min_c.merge(CI, how="left", left_index=True, right_index=True)
		total_cs = total_cs.append(merged.reset_index())

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

	if iteration == degree:
		return [(adj_adm3, iteration) for adj_adm3 in adj_adm3]

	else:

		connections = []
		for adj_adm3 in adm3_to_adm3.get(adm3, []):
			connections += [(adj_adm3, iteration)]
			connections += find_connections(adm3, adm3_to_adm2, adm3_to_adm3, degree, iteration=iteration+1)

		return connections

