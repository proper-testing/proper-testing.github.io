#!/usr/bin/env python
# -*- coding: utf-8 -*-
# kate: replace-tabs on; replace-tabs-save on;

import codecs
import markdown
import pygments.styles

"""
To make all.css, run the commands created by the following code:
print "touch all.css"
for style in pygments.styles.get_all_styles():
    print "./pygmentize.py -f html -S " + s + " -a .syntax_" + s + " >> all.css"
"""

code_file = codecs.open('test_code.md', 'r', 'utf8')
code_text = code_file.read()
code_file.close()
md = markdown.Markdown(extensions = ['codehilite'])
code_html = md.convert(code_text)

header_html = """
<html>
<head>
<title>Code Highlighting Test</title>
<link rel="stylesheet" type="text/css" href="resources/styles/main.css" />
<link rel="stylesheet" type="text/css" href="all.css" />
<style type="text/css">
    #footer, #nav ul{
        background-image: url('resources/images/gradient.jpg');
    }
</style>
</head>
<body>
<div id="header">
<a href="#" title="Back to main page"><img src="resources/images/header.png" alt="PropEr" /></a>
</div>
<div id="nav">
<ul id="pri_navbar"><li><a href="#">Main</a></li><li><a href="#">About</a></li><li><a href="#">Download</a></li><li class="current"><a href="#">Support</a></li></ul>
<ul id="sec_navbar"><li><a href="#">Main</a></li><li><a href="#">Documentation</a></li><li class="current"><a href="#">FAQ</a></li><li><a href="#">Publications</a></li><li><a href="#">Tips</a></li><li><a href="#">Tutorials</a></li></ul>
</div>
<div id="content">
"""

footer_html = """
</div>
<div id="footer">
<ul>
<li>Last edited on 2011-05-23.</li>
</ul>
</div>
</body>
</html>
"""

html_file = codecs.open('test_code.html', 'w', 'utf8')
html_file.write(header_html)

# for style in pygments.styles.get_all_styles():
for style in ['manni', 'colorful', 'default', 'trac', 'emacs', 'pastie']:
    html_file.write('<h2>' + style + '</h2>\n')
    html_file.write(code_html.replace('codehilite', 'syntax_' + style))

html_file.write(footer_html)
html_file.close()
