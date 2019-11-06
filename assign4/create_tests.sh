#!/bin/bash

rm tests/*

fnames=( "binop1.lam"
         "binop2.lam"
         "bool_true.lam"
         "bool_false.lam"
         "if.lam"
         "relop1.lam"
         "relop_or.lam"
         "relop_and.lam"
         "if_relop_and_or.lam" )

tests=( "1"
        "(1 + 2) * 4"
        "true"
        "false"
        "if true then 1 else 0"
        "1 > 2"
        "(1 > 2) || (1 < 2)"
        "(1 > 2) && (1 < 2)"
        "if ((1 > 2) || (1 < 2)) && (1 == 1) then true else false")

length=${#tests[@]}

for (( i = 0; i < length; i++ )); do
    echo "${tests[i]}" > "tests/${fnames[i]}"
done

