import pandas as pd


class District(object):

	def __init__(self, name, CI):
		'''
		name (string): name of TA in english
		CI (int): number of current infections
		'''

		self.name = name
		self.CI = CI
		self.TA_dict = {}  ## build-out later


	def add_TA(self, TA):
		"""
		adds TA object to self.TA_dict
		Inputs:  TA (TA object)
		"""

		self.TA_dict[TA.name] = TA



class TA(object):

	def __init__(self, name, district, adm3_to_adm2, adm3_to_adm3):
		'''
		name (string): name of TA in english
		adm3_to_adm2 (dict): dict mapping name to adjacent districts
		adm3_to_adm2 (dict): dict mapping name to adjacent TAs
		CI (int): number of current infections
		'''

		self.name = name
		self.district = district
		self.CI = district.CI  ## assume TA infections = District Infections

		self.adj_Districts = {}
		self.adj_TAs = {}
		self.contributions_adj1 = []
		self.contributions_adj2 = []


	def build_adj_Districts(self, Dist_dict, adm3_to_adm2):
		"""
		Populates self.adj_adm2_list
		"""
		adm2_list = adm3_to_adm2.get(self.name, [])

		for d in adm2_list:
			self.adj_Districts[d] = Dist_dict[d]


	def build_adj_TAs(self, TA_dict, adm3_to_adm3):
		"""
		Populates self.adj_adm3_list
		"""

		adm3_list = adm3_to_adm3.get(self.name, [])

		for t in adm3_list:
			self.adj_TAs[t] = TA_dict[t]


	def get_adj1_edges(self, Dist_dict): 

		contributions = []

		for district in self.adj_adm2_list:
 
			c = Contribution(self.name, , 1, Dist_dict[district].CI)
			contributions.append(c)

		self.contributions_adj1 = contributions

		return contributions


class Contribution(object):

	def __init__(self, adm3, adm2, degree, num_contributed):

		self.adm3 = adm3
		self.adm2 = adm2
		self.degree = degree
		self.num_contributed = num_contributed


def build_objects(df, adm3_to_adm2_dict, adm3_to_adm3_dict):
	"""
	Builds dictionary of District and TA objects
	Inputs:
	df (pd.DataFrame):  must have 3 columns in follwing order:
		District (str), TA (str), num_current_infections (int)
	adm3_to_adm2 (dict): dict mapping name to adjacent districts
	adm3_to_adm2 (dict): dict mapping name to adjacent TAs
	Returns: dictionary of Districts and dictionary of TAs
		key=name, value=corresponding object
	"""

	TA_dict = {}
	Dist_dict = {}

	for adm2, adm3, CI in df.itertuples(index=False, name=None):
	
		d = Dist_dict.get(adm2, District(adm2, CI))

		t = TA(adm3, d, adm3_to_adm2_dict, adm3_to_adm3_dict)
		TA_dict[adm3] = t

		d.add_TA(t)
		Dist_dict[adm2] = d

	return Dist_dict, TA_dict 


def build_adjacencies(Dist_dict, TA_dict, adm3_to_adm2, adm3_to_adm3):
	"""
	Populates adjancency attributes in TA objects
	Inputs
		Dist_dict (dict): dictionary mapping district name to object
		TA_dict (dict): dictionary mapping district name to object
		adm3_to_adm2 (dict): dictionary mapping adm3 name to adjacent adm3 name
		adm3_to_adm3 (dict): dictionary mapping adm3 name to adjacent adm2 name
	"""

	for t in TA_dict.values():
		t.build_adj_Districts(Dist_dict, adm3_to_adm2)
		t.build_adj_TAs(TA_dict, adm3_to_adm3)


def get_connections(TA_dict, Dist_dict):
	"""

	"""

	total_connections = []

	for ta in TA_dict.values():

		total_connections.extend(ta.get_adj1_edges(Dist_dict))

	return total_connections