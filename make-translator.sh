#!/bin/bash
OUTPUT=out.scm
TRANSLATOR='guile translator.scm'
FILES=(smile.smile stdlib.smile lexer.smile parser.smile translator.smile)

rm $OUTPUT
for i in ${FILES[@]}; do
    cat $i  | grep -v '^load' | $TRANSLATOR >> $OUTPUT
done
