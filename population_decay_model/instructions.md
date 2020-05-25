# Population Decay Model Instructions

## Set up country folder (first time run for country)

If you're running the Population Decay Model for a new country for the first time, you may need to complete the following easy steps.

The population model script finds all of its inputs for a given country in a specific folder named after that country.  This folder will be located in ```population_decay_model/country_inputs```.  If the country for which you'd like to run the model doesn't have a folder, you'll have to create it.  To do this:

1. Create a folder in country inputs with the name of that country.  For example BF could be a folder for Burkina Faso.  Try to limit characters in folder names to numbers, underscores, and letters.
2. Inside the folder named after the country, you must create 5 separate folders:
  * CI - this folder will hold the csv that lists current infections
  * shape - this folder will hold the country shape file and all of its friends
  * bundles - this folder will hold text files that list regions that should be bundled together
  * exclusions - this folder will list regions that should not be included
  * intermediate_data - this file is used by the script to store some intermediate components
  
Details about the contents of these folders can be found below
  
### CI

This is a ```.csv``` file that must have at minimum 2 columns.  The first row must contain the name of each column.  The name of the column that lists the number of infections must be called ```Current Infections```.  The other column must specify the region associated with that number of current infections.  The name of this region (which is in the first row of this column) **must be exactly the same in all other files**.

### shape

This file contains the ```.shp``` file and all of its friends.  The titles of all the geography in this file **must be exactly the same in all other files**.

### bundles

This is a ```.txt``` file of the following format.  The first line is the name of the larger region that smaller regions are being bundled into.  For example, in ```cities.txt``` in MW's bundles folder, the first line is ```ADM2_PCODE```.  Each line below this will contain one instance of that region.  For MW, we want all ADM3_PCODES that are in the ADM2_PCODE, MW210 to be bundled together, so we list MW210 underneath ADM2_PCODE.  **It is very important that name of the region in the first line is exactly the same as the name of this region in other files.**

### exclusions

Similar to bundles, this is a ```.txt``` file in which the first row is the name of the type of region that should be excluded and each line underneath indicates one instance of that region that should be excluded.  For example, in MW's exclusions folder, the first line is ```ADM3_PCODE```.  Each subsequence line is an ADM3_PCODE that will be excluded.  **It is very important that name of the region in the first line is exactly the same as the name of this region in other files.**

### intermediate_data

Fortunately you do not need to worry about this file.


## Writing the Config File

In ```population_decay_model/config``` create a copy of the ```config_template.txt```.  Rename that copy something that will identify the run.  Open the file and edit the parameters according to the specifications of your run.  Make your edits to the right of the colon.  See the description of each parameter below.  Delete a row containing any parameter you don't need.

* country:  Name of the country - must exactly match name of country folder.  **This parameter is required**
* num adjacent degrees:  Must be an integer.  signal the maximum number of regions away a given region that a different region can be and still contribute to its calculated infection score.  **This parameter is required**
* multiplier i:  Replace i with an integer ranging from 1 to the value of num adjacent degrees.  This parameter defines weight of a region with n infections i untils away.  For example, if region A is adjactent to region B, and region A has 5 current infections, and multiplier 1: .6, then region A will contribute 3 unites to region B's calculated infection score.  The must be one multiplier i parameter for each num adjacent degree.  **This parameter is required**
* region colname:  Name of the region for which infection scores are being calculate.  **Must exactly match the corresponding name in all files**
* CI:  The name of the current infections file in the CI folder.  See above for details.    **This parameter is required**
* CI region colname: Name of the region for which current infections are defined.  For example, for MW this would be ADM2_PCODE.  **Must exactly match the corresponding name in all files**    **This parameter is required**
* exclusions:  The name of the exclusions ```.txt``` file in the exclusions folder.  See above for details.
* bundles:  The name of the bundles ```.txt``` file in the bundles folder.  See above for details.
* shape:  The name of the  ```.shp``` file in the shape folder.  See above for details.
* outfile name:  The name of the final output.  If this is not provided a default one based on the date and country name is made.

## Running the Script

Congratulations!  You're ready to run the script!!!

1. Open your terminal and navigate to ```population_decay_model/```
2. Run ```python3 scripts/execute.py config/<config filename>```.  Note that the <> just indicate you're supposed to replace that text with the name of your config file.
3. Monitor the terminal and resolve any config file related errors.  If there is an error you cannot resolve email ```nselman@uchicago.edu```.
4. Retrieve your output in the ```population_decay_model/output``` folder.

## Pushing your output to the git repository.

In order for other team members to access your output you must push it to the git reposition.

1. ```git add <name of output file including path from current terminal location>```
2. ```git commit -m '<message describing run>'```
3. ```git push```


Please contact nselman@uchicago.edu with any questions.
