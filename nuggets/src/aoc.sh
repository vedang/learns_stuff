#!/bin/bash
input="/tmp/input_1.txt"

TRUE=1
FALSE=0
DEBUG=0
num_inputs="$(wc -l $input | cut -d ' ' -f 1)"

debug_echo ()
{
    if [ $DEBUG -eq 1 ]
    then
        echo "$1"
    fi
}

save_input ()
{
    local idx=0
    debug_echo "Saving Input!"
    while IFS= read -r line
    do
        input_arr[idx]=$line
        idx=$((idx + 1))
    done < "$input"
}

print_array ()
{
    for element in "${input_arr[@]}"
    do
        debug_echo "$element"
    done
}

is_present ()
{
    local val=$1
    local i=$2
    debug_echo "PR Checking if $val is present in input"
    while [ "$i" -lt "$num_inputs" ]
    do
        if [ "${input_arr[$i]}" -eq "$val" ] ; then
            debug_echo "PR FOUND $val !"
            return $TRUE
        fi
        i=$((i + 1))
    done
    debug_echo "PR DID NOT FIND $val !"
    return $FALSE
}

find_numbers ()
{
    local sum=$1
    local idx=$2
    while [ "$idx" -lt "$num_inputs" ]
    do
        local n1=${input_arr[$idx]}
        local n2="$((sum - n1))"
        debug_echo "TWO Num is $n1, so we are looking for $n2 to get to $sum"
        local idx_check="$((idx + 1))"
        is_present "$n2" "$idx_check"
        if [ $? -eq  $TRUE ]
        then
            echo "$n1 + $n2 == $sum"
            return $TRUE
        fi
        idx="$((idx + 1))"
    done

    return $FALSE
}

find_three_numbers ()
{
    local sum=$1
    local idx=$2
    debug_echo "THREE DEBUG: $sum $idx $num_inputs"
    while [ "$idx" -lt "$num_inputs" ]
    do
        debug_echo "THREE DEBUG: Running loop with $idx"
        local n1=${input_arr[$idx]}
        local n2="$((sum - n1))"
        debug_echo "THREE Num is $n1, so we are looking for $n2 to get to THREE $sum"
        find_numbers "$n2" "0"
        if [ $? -eq  $TRUE ]
        then
            echo "$n1 + $n2 <breakup above> = $sum"
            return $TRUE
        fi
        idx="$((idx + 1))"
        debug_echo "THREE DEBUG: New idx: $idx"
    done
    debug_echo "THREE Woe is me, I did  not find any numbers."
    return $FALSE
}

save_input
print_array
debug_echo "Finding numbers that sum to 2020"
find_numbers "2020" "0"
find_three_numbers "2020" "0"
