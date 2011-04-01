#! /bin/bash

if [ -d master ];
then 
    cd master
fi
pycco src/plugins/*.lisp src/*.lisp
