files=$1
output_file="config.dk"
regex="def match_(.*) :" # get inductive names
theory="cic"
for f in $files
do
    while read line; do
	if [[ $line =~ $regex ]]
	then
	    ind=${BASH_REMATCH[1]}
	    echo "[] $ind $theory.prop --> ${ind}_prop" >> $output_file
	    echo "[] $ind $theory.type --> ${ind}_type" >> $output_file
	fi
    done < $f
done
