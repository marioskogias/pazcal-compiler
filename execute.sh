#!/bin/bash

#Change to your execution directory
exec_dir="exec_env"

echo $1
cp $1 $exec_dir/a.asm
pushd $exec_dir
make run
popd
