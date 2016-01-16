#!/bin/bash
# Copied shamelessly from mini-python

COMP=$1
if [ -z "$COMP" ]; then
  COMP=./pscala
fi

score=0
compiler_errors=0
max=0

echo "Tests positifs (fichiers dans tests/good/)"

for f in tests/syntax/good/*.scala tests/typing/good/*.scala tests/exec/*.scala tests/typing/bad/*.scala tests/exec-fail/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    $COMP $f --parse-only > out
    result=$?
    if (( $result == 0 )) ; then
        score=`expr $score + 1`;
    elif (( $result == 1 )) ; then
        echo "  ECHEC du parsing pour $f"
    else
        echo "  ERREUR du compilateur pour $f"
        compiler_errors=`expr $compiler_errors + 1`;
    fi
done
echo

echo "Tests négatifs (fichiers dans tests/bad/)"

for f in tests/syntax/bad/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    $COMP $f --parse-only > out 2>&1
    result=$?
    if (( $result == 1 )) ; then
        score=`expr $score + 1`;
    elif (( $result == 0 )) ; then
        echo "  ECHEC : le parsing de $f devrait échouer"
    else
        echo "  ERREUR du compilateur pour $f"
        compiler_errors=`expr $compiler_errors + 1`;
    fi
done

rm out

echo
percent=`expr 100 \* $score / $max`;
echo "Score: $score / $max tests, soit $percent%" 
echo "Nombre d'erreurs du compilateur : $compiler_errors / $max tests"