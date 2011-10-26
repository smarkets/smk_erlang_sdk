.PHONY: test

compile:
	./rebar compile

test:
	./rebar skip_deps=true eunit

github:
	git push origin github-master
	git push github github-master:master
	git push github HEAD:refs/tags/v$(VSN)

delvsn:
	git push github :refs/tags/v$(VSN)
