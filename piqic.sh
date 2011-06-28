#!/bin/bash

(cd deps/smk_api_common/ \
  && piqic erlang -I ../eto_common/ seto.piqi \
  && mkdir -p ../../{src,include} \
  && mv *.hrl ../../include/ \
  && mv *.erl ../../src/ \
)
