include username.mk

.PHONY: default update_docs clean distclean build test upload

default: build

update_docs:
	if ! test -d proper; then git clone https://github.com/manopapad/proper.git; fi
	cd proper; git pull origin master
	$(MAKE) -C proper distclean doc
	rm -rf resources/doc
	cp -r proper/doc resources

clean:
	rm -f `find . -name '*~' -or -name '\#*\#'`

distclean: clean
	rm -rf build/*

build: distclean
	mkdir -p build
	cp -r resources/* build
	#wget --no-check-certificate -O pages_src/Support/User_Guide.md \
	#	https://github.com/manopapad/proper/raw/master/README.md
	python compile_pages.py

test:
	python start_webserver.py

upload: clean
	rsync --archive --delete --group=proper --chmod=ug+rw,a+r \
		build/* $(USERNAME)@greedy.softlab.ntua.gr:/home/proper
