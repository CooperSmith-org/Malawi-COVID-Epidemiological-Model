

def get_CI_geo_name():
	
	while True:
	
		updates = input("Please enter the name of the column in the CI file that defines the region:")

		if updates == "yes":
			is_update = True
		elif updates == "no":
			is_update = False
		else:
			print("Sorry, please enter yes or no.  Make sure you're using lower case.")
			continue
		break

	return is_update


def is_new_shape_file():

	while True:
		updates = input("Have there been any updates to the shape files? Enter yes | no:")

		if updates == "yes":
			is_update = True
		elif updates == "no":
			is_update = False
		else:
			print("Sorry, please enter yes or no.  Make sure you're using lower case.")
			continue
		break

	return is_update

def get_initial_params(country_list):


	### getting country
	while True:
		country = input("Please enter the 2 integer country code of the country\nyou're interested in:")
		if country not in country_list:
			print("Sorry, that's not a recognized 2 digit country code, try again")
			continue
		break

	### bundled 
	while True:
		bund = input("Do you want to bundle cities? Enter yes | no:")

		if bund == "yes":
			is_bund = True
		elif bund == "no":
			is_bund = False
		else:
			print("Sorry, please enter yes or no.  Make sure you're using lower case.")
			continue
		break

	### excluded
	while True:
		exclude = input("Do you want to exclude less populated geographies? Enter yes | no:")

		if exclude == "yes":
			is_excluded = True
		elif exclude == "no":
			is_excluded = False
		else:
			print("Sorry, please enter yes or no.  Make sure you're using lower case.")
			continue
		break

	return country, is_bund, is_excluded


def get_secondary_params():

	while True:
		val = input("How many adjacent geographic units should be searched: ")

		try:
			degree = int(val)
			if degree < 1:  
				print("Sorry, input must be 1 or greater, try again")
				continue
			break
		except ValueError:
			print("Sorry, that is not 1 or greater, try again")

	i = 1
	multiplier_dict = {}

	while i < degree + 1:
		val = input("Enter the weight of adjacent TA {}: ".format(i))

		try:
			weight = float(val)
			if weight > 1 or weight < 0:
				print("Sorry, between 1 and 0, try again")
				continue
			multiplier_dict[i] = weight	
			i += 1
			
		except ValueError:
			print("That is not between 0 and 1")
	print()

	print("Executing analysis for {} degrees".format(degree))
	print("Here are the weights:")
	for k, v in multiplier_dict.items():
		print("TA {}: {}".format(k, v))
	print()
	outfile = input("Enter the name of the file you'd like to output.\n" +
		"Do not include '.csv.' or any other file extension:")

	return degree, multiplier_dict, outfile