#!/usr/bin/env bash

set -e

artifacts=("out")

cleanup() {
    rm -rf ${artifacts[@]}
}

for file in $(find examples -name *_expected); do
    name=$(basename $file)
    name=${name%_expected}

    artifacts+=($name)

    echo "============= $name ============="

    ./compile.sh examples/$name.star 2> /dev/null && ./$name > out && diff out $file && echo -e "+ TEST PASSED\n" ||
        (echo "- TEST FAILED" && cleanup && exit 1)
done

echo "+++ ALL OK"
cleanup
