#!/bin/bash

# Get the directory of the script
directory=$(dirname $0)

# Compile the Prolog program
make

# Set the output file
output=test_results.out
echo "" > $output

# Iterate over the files in the input folder
for file in tests/input/*.txt; do
    # Get the base name of the file
    basename=$(basename $file)
    # Extract the file name without extension
    filename="${basename%.*}"
    echo "Testing $basename:" | tee -a $output
    # Run the Prolog program with the input file and redirect the output to a temporary file
    ./flp23-log < $file > result.tmp
    # Compare the output with the corresponding solution file using the Python script
    python3 compare_solutions.py result.tmp tests/solutions/$filename.txt | tee -a $output
done

# Check if any tests failed
(grep "FAILED." $output > /dev/null && echo -e "\n\033[0;31mSome tests failed.\033[0m" || echo -e "\n\033[92mAll tests passed.\033[0m") | tee -a $output