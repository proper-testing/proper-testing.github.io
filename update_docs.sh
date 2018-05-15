#!/bin/sh

MAIN_PAGE_LINK='<p style="position: absolute; bottom: 0px"><a href="/" target="_top">Back to Main Page</a></p>'
USER_GUIDE_META_DATA="---\nlayout: page\ntitle: User Guide\n---"
if ! test -d proper; then
    git clone https://github.com/proper-testing/proper.git
fi
cd proper
git pull -q origin master
make compile doc
cd ..
rm -f apidocs/*.html
cp proper/doc/*.html apidocs
for file in apidocs/*.html; do
    sed --in-place -e 's|http://www.erlang.org/"|/" target="_top"|' \
		   -e 's|erlang.png|/img/proper_tiny.png|' \
		   -e 's|erlang logo|PropEr logo|' $file
done
# sed --in-place -e "s|</body>|$MAIN_PAGE_LINK</body>|" resources/doc/modules-frame.html
echo $USER_GUIDE_META_DATA > userguide.md
cat proper/README.md >> userguide.md
rm -rf proper/
