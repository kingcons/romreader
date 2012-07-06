#!/bin/sh
sbcl --eval "(ql:quickload '(romreader sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :romreader \"docs/romreader.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
