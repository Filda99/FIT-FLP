import sys

SUCCESS_COLOR = '\033[92m'
WARNING_COLOR = '\033[93m'
FAIL_COLOR = '\033[91m'
NO_COLOR = '\033[0m'

def main():
    print("-------------------------")
    # Get the input and solution file paths from the command line arguments
    input_file = sys.argv[1]
    solution_file = sys.argv[2]

    # Read the output from the Prolog program, skipping spaces and empty lines
    with open(input_file, 'r') as f:
        output = [line.strip() for line in f if line.strip()]

    # Read the content of the solution file, skipping spaces and empty lines
    with open(solution_file, 'r') as f:
        solution = [line.strip() for line in f if line.strip()]

    print("Output:")
    print(output)
    print("Solution:")
    print(solution)

    # Compare the output with the solution
    if output == solution:
        print(f"{SUCCESS_COLOR}PASSED{NO_COLOR}")
    else:
        print(f"{FAIL_COLOR}FAILED{NO_COLOR}")

if __name__ == "__main__":
    main()