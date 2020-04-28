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
		self.contributions_adj1 = set()
		self.contributions_adj2 = set()

		self.first_adj_score = 0
		self.second_adj_score = 0
		self.total_score = 0


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


	def get_adj1_edges(self):
		"""
		Populates self.contributions_adj1
		"""

		contributions = set()

		for district in self.adj_Districts.values():
 
			c = Connection(self, district, 1, district.CI)
			contributions.add(c)

		self.contributions_adj1 = contributions

		### calc total
		for c in self.contributions_adj1:
			self.first_adj_score += c.num_contributed

		return contributions


	def get_adj2_edges(self):
		"""
		Populates self.contributions_adj1
		Only includes connections to districts that are not first
		connections.
		"""

		### first find all adj Districts of neighbor TAs
		possible_districts = []

		for t in self.adj_TAs.values():
			possible_districts.extend(list(t.adj_Districts.values()))

		### subset to districts that self isn't adjacent to
		contributions = set()

		for d in possible_districts:
			if d.name not in self.adj_Districts:
				contributions.add(Connection(self, d, 2, d.CI))

		self.contributions_adj2 = contributions

		### calc total score
		for c in self.contributions_adj1:
			self.second_adj_score += c.num_contributed

		return contributions


	def calc_total_score(self, first_multiplier, second_multiplier):

		if self.CI:
			self.total_score = self.CI
		else:
			self.total_score = first_multiplier * self.first_adj_score + \
			second_multiplier * self.second_adj_score

		return self.total_score


class Connection(object):

	def __init__(self, TA, District, degree, num_contributed):

		self.TA = TA
		self.District = District
		self.degree = degree
		self.num_contributed = num_contributed


	def __repr__(self):

		s = (
			"""
			TA: {}, District: {}, Degree: {}, Number: {}
			""".format(self.TA.name, self.District.name, self.degree, self.num_contributed)
			)

		return s


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


def get_connections(Dist_dict, TA_dict):
	"""

	"""

	total_connections = []

	for ta in TA_dict.values():
		total_connections.extend(ta.get_adj1_edges())

	for ta in TA_dict.values():
		total_connections.extend(ta.get_adj2_edges())

	return total_connections


def convert_connections_to_df(connections_list, first_multiplier, second_multiplier):

	df = pd.DataFrame(columns=['TA', 'District', 'Degree', 'Num Infections', 'Multiplier'])

	for c in connections_list:
		row = {}
		row['TA'] = c.TA.name
		row['District'] = c.District.name
		row['Degree'] = c.degree
		row['Num Infections'] = c.num_contributed
		if c.degree == 1:
			row['Multiplier'] = first_multiplier
		elif c.degree == 2:
			row['Multiplier'] = second_multiplier
		# print(row)
		df = df.append(row, ignore_index=True)

	return df


def calc_total_scores(TA_dict, first_M, second_M):

	scores = []

	for t in TA_dict.values():

		score = t.calc_total_score(first_M, second_M)
		from_CI = int(t.CI > 0)
		scores.append((t.name, score, from_CI))

	return scores
