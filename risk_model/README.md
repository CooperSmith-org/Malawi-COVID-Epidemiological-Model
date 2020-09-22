# Risk Model

The goal of the risk model is go generate a risk score for each administrative region in Malawi.  Each input to the model measures a characteristic assumed to be positively related to COVID risk.  For each input, a quantity analogous the percentage of the country total is calculated for each TA.  For example, for the input Population Density, the Population Density for each TA is divided by the sum of the for all TAs, then multiplied by 100.  We refer to this quantity as a rank.  The final output is the sum of the ranks for all inputs.

### Inputs

- Population density (source)
- Percent elderly (source)
- Interpolated Current Infections (Currently from the Population Decay Model detailed below)

### Output

- The output of the risk model is a risk score which is exactly the sum of the ranks for all TAs.

## Population Decay Model

The population decay model is a naïve method of interpolating missing data of current infections.  This will eventually be removed from the model when better data becomes available.  

1. For each TA with missing current infection data, we estimate the current infections to be the sum of the current infections in all surrounding TAs multiplied by 0.6.
2. That number is added to the sum of all the current infections in all TAs two borders way not including the "first ring" of adjacent TAs multiplied by 0.2.

The following equation describes the process
$$
(.6 \sum\limits_{adjacent\_TAs} adjacent\_TAs) + (.4 \sum\limits_{2boarder\_TAs} 2boarder\_TAs)
$$


