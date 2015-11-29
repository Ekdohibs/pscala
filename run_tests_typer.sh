#!/bin/bash
# Copied shamelessly from mini-python

score=0
max=0
compiler_errors=0

echo "Tests positifs (fichiers dans tests/good/)"

for f in tests/typing/good/*.scala tests/exec/*.scala tests/exec-fail/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    ./pscala $f > out
    result=$?
    if (( $result == 0 )) ; then
        score=`expr $score + 1`;
    elif (( $result == 1 )) ; then
        echo "  ECHEC du typing pour $f"
    else
        echo "  ERREUR du compilateur pour $f"
        compiler_errors=`expr $compiler_errors + 1`;
    fi
done
echo

echo "Tests négatifs (fichiers dans tests/bad/)"

for f in tests/typing/bad/*.scala; do
    max=`expr $max + 1`;
    echo $f
    rm -f out
    ./pscala $f > out 2>&1
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