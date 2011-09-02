github:
	git push origin github-master
	git push github github-master:master
	git tag v$(VSN)
	git push origin refs/tags/v$(VSN):refs/tags/v$(VSN)
	git push github refs/tags/v$(VSN):refs/tags/v$(VSN)

delvsn:
	git tag -d v$(VSN)
	git push origin :refs/tags/v$(VSN)
	git push github :refs/tags/v$(VSN)
