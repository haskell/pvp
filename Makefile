PANDOC=pandoc

index.html: README.md Makefile html/pre-body.inc.html html/post-body.inc.html html/post-head.inc.html
	$(PANDOC) -T 'Package Versioning Policy' -t html5 -B html/pre-body.inc.html -A html/post-body.inc.html -H html/post-head.inc.html -o $@ README.md 
