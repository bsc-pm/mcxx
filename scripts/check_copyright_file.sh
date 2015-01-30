#!/bin/bash

readonly mode="$1"
readonly filename="$2"

if scripts/file_name_match.py scripts/copyright_excludes.txt "$filename";
then
    exit 0
fi

has_lgplv3_license()
{
    grep -q "GNU Lesser General Public" "$1" && grep -q "version 3 of the License" "$1"
}

get_copyright_year()
{
    grep -o "Copyright [0-9]\{4\}-[0-9]\{4\}" "$1" | sed -e 's/Copyright [0-9]\{4\}-\([0-9]\{4\}\)/\1/'
}

if has_lgplv3_license "$filename";
then
    YEAR=$(./scripts/get_year_of_file.sh "$filename")
    YEAR_IN_FILE=$(get_copyright_year "$filename")

    if [ -z "$YEAR_IN_FILE" ];
    then
        if [ "$mode" = all -o "$mode" = "missing-year" ];
        then
            echo "File '$filename' has license but does not seem to have any copyright year"
        fi
    elif [ "$YEAR" != "$YEAR_IN_FILE" ];
    then
        if [ "$mode" = all -o "$mode" = "wrong-year" ];
        then
            echo "File '$filename' does not have the proper copyright year, it is $YEAR_IN_FILE but it should be $YEAR"
        fi
    fi
else
    if [ "$mode" = all -o "$mode" = "missing-license" ];
    then
        echo "File '$filename' does not seem to have the LGPLv3 license"
    fi
fi
