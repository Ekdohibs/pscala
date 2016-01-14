#!/bin/bash
# Copied shamelessly from mini-python

score=0
max=0
compiler_errors=0

echo "Tests positifs (fichiers dans tests/exec/)"

for f in tests/exec/*.scala tests/exec_add/good/*.scala; do
    max=`expr $max + 1`;
    echo $f
    nm=`dirname $f`/`basename $f .scala`
    rm -f out
    ./pscala $f > out
    result=$?
    if (( $result == 0 )) ; then
        gcc $nm.s -o $nm
        if (( $? == 0 )) ; then
          timeout 1s $nm > out;
          result=$?
          if (( $result == 0 )) ; then
            if cmp --quiet out $nm.out; then
              score=`expr $score + 1`;
            else
              echo "  ECHEC : mauvaise sortie pour $f"
            fi
          elif (( $result == 124 )) ; then
            echo "  ECHEC : l'éxécution de $f a dépassé le temps imparti"
          else
            echo "  ECHEC de l'éxécution pour $f"
          fi
          rm -f $nm
        else
          echo "  ECHEC de la compilation de l'assembleur produit pour $f"
        fi
        rm -f $nm.s
    elif (( $result == 1 )) ; then
        echo "  ECHEC de la compilation pour $f"
    else
        echo "  ERREUR du compilateur pour $f"
        compiler_errors=`expr $compiler_errors + 1`;
    fi
done
echo

echo "Tests négatifs (fichiers dans tests/exec-fail/)"

for f in tests/exec-fail/*.scala; do
    max=`expr $max + 1`;
    echo $f
    nm=`dirname $f`/`basename $f .scala`
    rm -f out
    ./pscala $f > out
    result=$?
    if (( $result == 0 )) ; then
        gcc $nm.s -o $nm
        if (( $? == 0 )) ; then
          timeout 1s $nm > out;
          result=$?
          if (( $result == 0 )) ; then
            echo "  ECHEC : l'éxécution de $f devrait échouer"
          elif (( $result == 124 )) ; then
            echo "  ECHEC : l'éxécution de $f a dépassé le temps imparti"
          else
            score=`expr $score + 1`;
          fi
          rm -f $nm
        else
          echo "  ECHEC de la compilation de l'assembleur produit pour $f"
        fi
        rm -f $nm.s
    elif (( $result == 1 )) ; then
        echo "  ECHEC de la compilation pour $f"
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