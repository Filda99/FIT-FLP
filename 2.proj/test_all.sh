directory=$(dirname $0)

make
output=test_results.out
echo "" > $output

for file in $directory/*.txt; do
    basename=$(basename $file)
    echo "Testing $basename:" | tee -a $output
    ./flp23-log < $file > result.tmp
    python3 $directory/compare_solutions.py -ot regular $file result.tmp | tee -a $output
done

(grep "FAILED." $output > /dev/null && echo -e "\n\033[0;31mSome tests failed.\033[0m" || echo -e "\n\033[0;34mAll tests passed.\033[0m") | tee -a $output
