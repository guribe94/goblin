#!/bin/bash

eval $(opam config env)

goblinExecPath=./goblin.native
# testOption=$1 #stores the test flag since functions can't see the $1
# vFlag=$2 #stores the -v flag since functions can't see it with $2
pass=0
fail=0
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'
errorFile=errors.log
excpTestFlag=0

logFile=./compiler_tests.log
testPath=./tests
testExtension=.out
gobbler=../gobble



# Set time limit for all operations


ulimit -t 30


createGoblin(){
  echo "Creating Goblin compiler"
  make clean 2>&1 > /dev/null
  make 2>&1 > /dev/null
  echo "Compilation of Goblin executable complete"
}

runTests(){
  for testFile in *.gob; do

    filename=$(basename "$testFile")

    echo "==================================" >> session_file
    echo "Testing: $filename" >> session_file

    $gobbler $testFile &> /dev/null
    exe_name=$(basename "$testFile" | cut -d. -f1)


    if [ -f "../$exe_name" ]
    then
      ../$exe_name > temp.out && rm ../$exe_name
      #Test output differences use the diff command and neglect screen output
    else
      $gobbler $testFile 2> temp.out
    fi

    diff temp.out $filename$testExtension
    confirmation #function
  done

  rm temp.out

  #Verbose flag actuated
  if [ "$vFlag" == "-v" ]; then
    cat session_file
  fi

  #Copy session output to historical log
  cat session_file >> "$logFile"

  total=$(($pass + $fail))
  #Test status output
  echo -e
  echo -e "${GREEN}Tests Passed: $pass ${NC}"
  echo -e "${RED}Tests Failed: $fail ${NC}"
  echo -e "${CYAN}Total Tests: $total ${NC}"
  echo "View $logFile for more information"

  #Clean up temp files
  rm session_file;
}


confirmation(){
  #$? is the exit code for diff, if 0, then test output matched!
  if [ $? -eq 0 ];
  then
    echo -e "${GREEN}$filename passed!${NC}" >> session_file
    echo -e "${GREEN}$filename passed!${NC}"
    ((pass++))

  else
    echo -e "${RED}$filename FAILED${NC}" >> session_file
    echo -e "${RED}$filename FAILED${NC}"

    #print out expected output and result
    echo "Expected Output:" >> session_file

    if [ $excpTestFlag -eq 0 ];	then
      cat $filename$testExtension >> session_file
    else
      cat "$testExceptionsPath"$filename$testExtension >> session_file
    fi
    echo "" >> session_file
    echo "Generated Output:" >> session_file
    cat "$filename".out  >> session_file
    echo "" >> session_file
    ((fail++))
  fi
}


main(){
  createGoblin
  cd $testPath
  runTests

}

main
