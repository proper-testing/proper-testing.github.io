include username.mk

.PHONY: default update_proper_docs clean distclean build test upload

default: build

update_proper_docs:
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
	ifdef USERNAME
	cp -r resources/* build
	wget --no-check-certificate -O pages_src/Support/User_Guide.md \
		https://github.com/manopapad/proper/raw/master/README.md
	python build_pages.py
	else
	@echo "Please read the instructions first."
	endif

test:
	python webserver.py

upload: clean
	rsync --archive --delete build/* $(USERNAME)@greedy.softlab.ntua.gr:/home/proper
