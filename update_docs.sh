#!/bin/sh

MAIN_PAGE_LINK='<p style="position: absolute; bottom: 0px"><a href="/" target="_top">Back to Main Page</a></p>'

if ! test -d proper; then
    git clone https://github.com/manopapad/proper.git
fi
cd proper
git pull origin master
make distclean doc
cd ..
rm -f resources/doc/*.html
cp proper/doc/*.html resources/doc
for file in resources/doc/*.html; do
    sed --in-place -e 's|http://www.erlang.org/"|/" target="_top"|' \
		   -e 's|erlang.png|/images/proper_tiny.png|' \
		   -e 's|erlang logo|PropEr logo|' $file
done
sed --in-place -e "s|</body>|$MAIN_PAGE_LINK</body>|" resources/doc/modules-frame.html
