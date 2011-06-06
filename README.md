Working on the site
===================

Preparation
-----------

*   Only members of the 'proper' group on greedy have commit and upload access.
*   To clone the PropEr site sources, run:
	git clone ssh://me@greedy.softlab.ntua.gr/home/nonlocal/manopapad/proper_site.git
    where 'me' is your user name on greedy.
*   Add a file named 'username.mk' to the project's main directory. This file
    should contain a single line of the form 'USERNAME=me', where 'me' is your
    user name on greedy. git will ignore this file.
*   You are going to need a working python interpreter.


Editing the site
----------------

*   Add or edit pages in the 'pages_src' directory.
*   Pages are written in Markdown. For help on Markdown, see [the official
    syntax guide][1] and [a list of available extensions][2] (extensions must be
    enabled separately, inside compile_pages.py). This README is actually
    written in Markdown.
*   Page names must be alphanumeric, in mixed case, with underscores instead of
    spaces, and must have the extension '.md'. A page name may optionally begin
    with an index of the form '42#', in order to override the default alphabetic
    ordering of pages. This prefix will be removed when building the site.
*   The site's structure is reflected in the directory hierarchy.
*   Support/User_Guide.md is copied from PropEr's README file. Do not edit this
    file directly: any changes you make will be overwritten.
*   Headers and footers will be added to each page automatically. The template
    for these can be found in the 'template.html' file.
*   A title and heading will be added to each page, according to its file name.
*   Every directory must contain an 'index.md' file. The contents of that file
    will be automatically appended with a list of the pages and sub-directories
    inside that directory.
*   The lines at the top of source files are meta-information:
    'Summary' (mandatory)
    :   used to construct the link lists inside index files (the description of
        a page will be the value of its 'Summary' attribute, the description of
        a sub-directory will be the value of the 'Summary' attribute of its
        index page)
    'Author' (optional)
    :   if present, author information will be added under the page's header
    'kate'
    :   must have the value:
        'replace-tabs-save on; replace-tabs on; tab-width 8;'
        to avoid complications when editing the file in the editor Kate
*   EDoc-generated documentation must be copied manually to 'resources/doc'
    periodically, by running 'make update_docs' on a system that has a recent
    version of EDoc.
*   Other resources (images, style sheets, pdfs etc.) can be found in the
    'resources' directory.
*   To specify the width and/or height of included images, import them using
    the HTML tag directly:
        <img src="..." alt="..." title="..." width="..." height="..." />

[1]: http://daringfireball.net/projects/markdown/
[2]: http://www.freewisdom.org/projects/python-markdown/Available_Extensions


Workflow
--------

1.  Make your changes.
2.  Run 'make' to compile the pages.
3.  Run 'make test' to launch a minimal web server that will serve the site. The
    site's pages will be available in any browser at [http://localhost:8080].
    When you're done, give Ctrl+C to stop the server.
4.  If you're not happy with the result, go back to step 1.
5.  Commit and push your changes.
6.  Run 'make upload' to upload the changes to the web.


Stuff to add
============

Front Page
----------

*   some description of what PropEr is
*   Where to go now (download, read the user guide, see the docs, contact etc.)
*   "Current version is ..."
*   links to other tools (dialyzer etc.)


Header
------

*   icon and (fancy) "PropEr" logo that link to the main page
*   Erlang icon must be lower (and bigger)
*   shirt icon: "PropEr" on the shirt tag, size = "42", Erlang logo smaller and
    a little lower, watch the perspective
*   more modern (and austere) font for logo (e.g. SansSerif)
*   Horizontal navigation bar (have extra "Home" link?)
*   Second level of structure, for the current sub-directory
*   The documentation / FAQ, etc. could be more visible, not hidden under
    support: flat navbar layout (for now)


Footer
------

*   "Copyright ..."
*   license
*   "Powered by ..."
*   "Valid HTML ..."
*   "This page was last edited on ..." (make this more fine-grained?)
*   page author information (read from meta-information)


Help/Support
------------

*   link to external resources on PropEr (interviews, podcasts, tutorials,
    chapter in Learn You Some Erlang, QuickCheck stuff, ...)
*   link to issues page on github
*   link to contact information, in case of problems
*   Quickstart, User Guide,  Common Problems (same as PropEr's README)
*   EDoc-powered documentation
*   FAQ, How-To
*   Tips, Common mistakes/blunders
*   Tutorials (e.g. simple lists properties, re2, proc_dict statem, combining
    PropEr and EUnit: go from EUnit to PropEr or integrate PropEr in EUnit ...)
*   Demos, Sample Properties, showcase of QC-mode and PropEr-exclusive features
    (various demos from presentations, Kresten's properties, ec_dictionary, from
    open-source projects, ...)
*   re2 extended demo (full process of specification, re vs re2, generators from
    regexps, generators or even native types as string,regexp,options)
*   links to (preferably locally saved) talks and papers (+bibliography info)
*   separate "Talks" tab (not only academic talks, but also presentations,
    podcasts etc.)
*   humorous version of presentation with "times said 'proper'" counter
*   EDoc pages color theme that resembles the main page


Code highlighting
-----------------

*   consistency in code examples, correct indentation, blank lines between
    functions
*   pick a theme and include the corresponding .css file
*   add language annotation to code blocks (#!erlang for code and #!erl for
    shell output)
*   no line numbers
*   best themes: manni, colorful, default, trac (nice, but users might confuse
    red keywords for links), emacs, pastie (nice)
*   a light blue/gray background is good for differentiating code from text
    (could add it explicitly if theme doesn't have one)
*   alternatively, left-indent code blocks (via the margin-left or border-left
    CSS property?) (also add corresponding right-indentation?)
*   fix python-markdown fenced-code & codehilite incompatibility


Download
--------

*   Packaged versions
*   link to project page on github (bleeding edge version)


About
-----

*   Group photo
*   Per author:
    - name
    - photo
    - bio (funny version?)
    - part written
    - e-mail
    - personal webpage
*   PropEr's users
*   (Humorous) reviews
*   "Icon by Gordon Irving"
*   link to QuviQ's site
*   Image map and separate page for each dev


Humor
-----

*   Change the page descriptions to something funnier:
    About
    :   The PropEr developers
    API
    :   Browse our PropEr API
    Download
    :    PropEr instructions how to do this
    FAQ
    :    Frequently Asked Questions with PropEr Answers
    Publications
    :    PropEr papers and talks
    Tips
    :    PropEr suggestions for effective use of the tool
    Tutorials
    :    PropEr tutorials
*   What people have said about PropEr:
    -   Stavros Aronis: "PropEr is a QuickCheck parody."
    -   Kostis Sagonas: "Good job, now make it produce properties from specs."


Infrastructure
--------------

*   Add a title to each navbar link (the corresponding summary for normal links,
    or simply "Back to main page" for home links).
*   Test for broken links, set up automatic check.
*   Setup code highlighting with pygments, also other extensions.
*   Choose a code highlighting style
*   Restore the copying of README.md from proper.
*   Work on the stylesheet (bigger text on navbars?).
*   make header.png non-transparent?
*   have a set of alternatives for each photo and make them auto-cycle
*   some way to specify the order of pages: e.g. can name pages as:
    1.Something.md, to make them first in the list, then remove the 1. when
    converting, so that we get Something.html
*   make the width 900px?
*   fully justified text?
*   group on rsync is wrong?
*   fixes on CSS to allow for floating images
*   favicon = resized PropEr logo / new logo
*   titles on all pages should contain "PropEr"
*   more/fewer categories

<!-- kate: replace-tabs-save on; replace-tabs on; tab-width 8; -->
