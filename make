#! /bin/bash
DIRECTORIES="bin build test test/build test/bin test/html test/shell"
DO_CLEAN=0
DO_MAKE=1
DO_TESTS=0

case $1 in
    "clean") DO_CLEAN=1
             DO_MAKE=0  ;;
    "test" ) DO_TESTS=1
             DO_MAKE=0  ;;
    "all"  ) DO_CLEAN=1
             DO_TESTS=1 ;;
esac

if [[ $DO_CLEAN = 1 ]]; then
    for DIR in $DIRECTORIES; do
        if [[ -d $DIR ]]; then
            rm -r $DIR
        fi;
    done
fi

if [[ $DO_MAKE = 1 || $DO_TESTS = 1 ]]; then
    for DIR in $DIRECTORIES; do
        if [[ ! -d $DIR ]]; then
            mkdir $DIR
        fi;
    done
fi

if [[ $DO_MAKE = 1 ]]; then
    ghc -o bin/renamer -outputdir bin -odir build -hidir build --make RenCli.hs
fi

if [[ $DO_TESTS = 1 ]]; then
    ghc -fhpc RunTests.hs -o "test/bin/RunTests" -outputdir "test/bin" -odir "test/build" -hidir "test/build" -hpcdir "test/build" --make
    CURR=$PWD
    cd ./test/build
    ../bin/RunTests
    RC=$?
    hpc markup RunTests --exclude=Main --exclude=QC --exclude=Tests --srcdir $CURR  --hpcdir=test/build --destdir="../html"
    cd $CURR
    if [[ $RC == 0 && $DO_MAKE == 1 ]]; then
        ./testshell
    fi
fi
