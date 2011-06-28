#!/bin/bash

SMK_API_COMMON=deps/smk_api_common/
if [ ! -d "$SMK_API_COMMON" ]; then
  SMK_API_COMMON=../smk_api_common/
fi

API_HOME="$PWD"
(cd "$SMK_API_COMMON" \
  && piqic erlang -I ../eto_common/ seto.piqi \
  && mkdir -p "$API_HOME"/{src,include} \
  && mv *.hrl "$API_HOME"/include/ \
  && mv *.erl "$API_HOME"/src/ \
)
