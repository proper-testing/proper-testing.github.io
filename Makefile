include username.mk

.PHONY: default update_docs clean distclean build test upload

default: build

update_docs:
	./update_docs.sh

clean:
	rm -f `find . -name '*~' -or -name '\#*\#'`

distclean: clean
	rm -rf build/*

build: distclean
	mkdir -p build
	cp -r resources/* build
	python compile_pages.py

test:
	python start_webserver.py

upload: clean
	rsync -apO --delete --chmod=Dg+s,uga=rx,ug+w \
		build/* $(USERNAME)@greedy.softlab.ntua.gr:/home/proper
