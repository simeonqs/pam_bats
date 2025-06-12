#!/bin/bash

# Define the path to the R script
R_SCRIPT_PATH="/home/au472091/OneDrive/au/projects/pam_bats/analysis/code/aspot_combine_selection_tables.R"

# Define the base directory containing deployment folders
BASE_DIR="/media/au472091/data/new_results_aspot/land"

# Loop through each folder in the base directory
for deployment in "$BASE_DIR"/*; do

    echo "$deployment"

    # Extract just the folder name
    DEPLOYMENT_NAME=$(basename "$deployment")

    # Define the expected directory path
    COMBINED_DIR="$deployment/combined_selection_tables"

    # Check if the combined_selection_tables directory exists
    if [ ! -d "$COMBINED_DIR" ]; then
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
