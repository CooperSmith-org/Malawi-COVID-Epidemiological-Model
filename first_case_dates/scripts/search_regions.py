import pandas as pd

def calc_scores(total_connections, CI, multiplier_dict, r):
	"""
	total_cs (pd.DataFrame): 
	CI (pd.Series):
	"""

	for k, v in multiplier_dict.items():
		total_connections.loc[total_connections["Degree"] == k, "multiplier"] = v

	scores = total_connections.groupby(r)['multiplier'].sum().reset_index()
	scores = scores.merge(CI, how='left', left_on=r, right_on=r)
	scores.rename({'multiplier': 'calculated score'}, axis=1, inplace=True)
	scores['final score'] = scores['calculated score']
	scores.loc[scores['Current Infections'] > 0, 'final score'] = \
		scores.loc[scores['Current Infections'] > 0, 'Current Infections']

	return scores


def get_connections(adm3_to_adm3, CI, degree, colname):
	"""
	Calls find_connections for each TA then products a df of the union of all
	connections with CI data merged on
	Inputs:
		adm3_to_adm2 (dict): maps TA to list of adjacent Districts
		adm3_to_adm3 (dict): maps TA to list of adjacent TAs
		CI (pd.Series): index is District and value is number of current infections
		degree (int): maximum degree of connections desired.  For example, enter one if
		only interested in infections in adjacent TAs.  Enter 2 if interested in TAs that
		border adjacent TAs, etc.
	Returns pd.DataFrame that lists all connections and their degree
	"""
	# print(CI)
	total_cs = pd.DataFrame(columns=["adj_"+colname, "Degree", colname, "Current Infections"])

	for adm3 in adm3_to_adm3.keys():

		c = find_connections(adm3, adm3_to_adm3, degree)
		c_df = pd.DataFrame(c, columns=["adj_" + colname, "Degree"])
		min_c = c_df.groupby("adj_" + colname).min()
		# print("max degree connection: {}".format(min_c.max()))
		min_c[colname] = adm3
		min_c = min_c.reset_index()
		# print(min_c.head())

		merged = min_c.merge(CI, how="left", left_on="adj_" + colname, right_on=colname)
		merged.rename({colname + '_x': colname}, axis=1, inplace=True)
		merged.drop([colname + '_y'], axis=1, inplace=True)
		# print(merged.head())
		total_cs = total_cs.append(merged.reset_index())
		total_cs.drop('index', axis=1, inplace=True)

	return total_cs


