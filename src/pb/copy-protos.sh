#!/bin/bash

GO_IPFS_DIR=$GO_ROOT/src/github.com/ipfs/go-ipfs
COPY_DIR=$PWD

for protoFile in $(find $GO_IPFS_DIR -wholename *pb/*.proto) ; do
    cp $protoFile $COPY_DIR 
    sed -i '1isyntax = "proto2";' $COPY_DIR/$(basename $protoFile)
    echo $protoFile
done
