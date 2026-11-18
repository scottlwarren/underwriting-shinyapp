# Underwriting tool in RShiny

This tool is designed to run on a synthetic dataset as a proof of concept for a interactive, self-service application for assessing changes in risk factors. 

Due to time constraints, there are a limited number of factors which are editable. 

## Dataset

The dataset is a synthetic dataset generated in Claude Opus 5.5. There are two main bordereauxs showing `policies` and `claims`, with an expectation of these being standardsed for each scheme.

## Data processing

Data is processed using DuckDB - with the idea that this could be connected directly to a database housing vast amounts of data. DuckDB also has the benefit of being adaptable for memory use - allowing this tool to be deployed on SBCs or minimal cloud infrastructure. As DuckDB can store items in storage rather than RAM, it means there would be less risk using low powered machinery. 

## Logic

Currently the logic is very simple - it multiplies gross premium in each grouping (for instance age group) using numeric inputs in the sidebar. For the two current pricing factors, this works well - however in future there could be more specific targeting of premium - for instance, age band could impact vet premiums at a different rate to items like tack premium.

## Future development

In the future, goals would be to create a way to download the new ratings and associated loss ratios as a CSV. I would also like to change the hardcoded features into a more dynamic approach to be able to use the tool for multiple scheme types. 