#!/bin/bash
# NOTE: mustache templates need \ because they are not awesome.

cd `dirname $0`

MNESIA_DIR="database"
IF_MNESIA_INIT=""

if [ ! -r $MNESIA_DIR ]; then
    IF_MNESIA_INIT="db_mnesia:init()";
else
    IF_MNESIA_INIT="db_mnesia:start()";
fi


exec erl -pa ebin deps/*/ebin -boot start_sasl -mnesia dir $MNESIA_DIR -eval "$IF_MNESIA_INIT" -eval "ibrowse:start()" -sname pichub_dev -s codrel -eval "pichub_web:start()"
