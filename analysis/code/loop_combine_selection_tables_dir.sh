#!/bin/bash

# Define the path to the R script
R_SCRIPT_PATH="/home/au472091/OneDrive/au/projects/pam_bats/analysis/code/aspot_combine_selection_tables.R"

# Define the base directory containing deployment folders
BASE_DIR="/media/au472091/data/new_results_aspot/land"

# Define the directory where the CSVs should exist
CSV_DIR="/home/au472091/OneDrive/au/projects/pam_bats/analysis/results/species_overview"

# Loop through each folder in the base directory
for deployment in "$BASE_DIR"/*; do
    # Extract just the folder name
    DEPLOYMENT_NAME=$(basename "$deployment")

    # Define the expected CSV path
    CSV_PATH="$CSV_DIR/$DEPLOYMENT_NAME.csv"

    # Check if the CSV file exists
    if [ ! -f "$CSV_PATH" ]; then
        # Create a temporary copy of the R script
        TEMP_R_SCRIPT="/tmp/temp_aspot_combine_selection_tables.R"
        cp "$R_SCRIPT_PATH" "$TEMP_R_SCRIPT"

        # Replace the deployment name in the temporary R script
        sed -i "s/deployment = 'Kammerslusen_0903_2024_A'/deployment = '$DEPLOYMENT_NAME'/g" "$TEMP_R_SCRIPT"

        # Run the modified R script
        Rscript "$TEMP_R_SCRIPT"

        # Wait for the R script to finish before continuing
        wait
    fi
done
