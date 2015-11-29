#!/bin/bash
# Copied shamelessly from mini-python

score=0
max=0

echo "Tests positifs (fichiers dans tests/good/)"

for f in tests/typing/good/*.scala tests/exec/*.scala tests/exec-fail/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    if ./pscala $f > out; then
        score=`expr $score + 1`;
    else
        echo "  ECHEC du typing pour $f"
    fi
done
echo

echo "Tests négatifs (fichiers dans tests/bad/)"

for f in tests/typing/bad/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    if ./pscala $f > out 2>&1; then
        echo "  ECHEC : le typing de $f devrait échouer"
    else
        #if grep -q "^error:" out; then
            score=`expr $score + 1`;
        #else
        #    echo "  ECHEC : devrait afficher 'error'"
        #fi
    fi
done

rm out

echo
percent=`expr 100 \* $score / $max`;
echo "Score: $score / $max tests, soit $percent%"