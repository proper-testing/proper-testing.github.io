Working on the site
===================

Preparation
-----------

*   Only members of the 'proper' group on greedy have commit and upload access.
*   To clone the PropEr site sources, run:
	git clone ssh://me@greedy.softlab.ntua.gr/home/nonlocal/manopapad/proper_site.git
    where 'me' is your username on greedy.
*   Add a file named 'username.mk' to the project's main directory. This file
    should contain a single line of the form 'USERNAME=me', where 'me' is your
    username on greedy. git will ignore this file.
*   You are going to need a working python interpreter.


Editing the site
----------------

*   Add or edit pages in the 'pages_src' directory.
*   Pages are written in Markdown. For help on Markdown, see [the official
    syntax guide][1] and [a list of available extensions][2].
*   Page names must be alphanumeric, in mixed case, with underscores instead of
    spaces, and must have the extension '.md'.
*   The site's structure is reflected in the directory hierarchy.
*   Support/User_Guide.md is copied from PropEr's README file. Do not edit this
    file directly: any changes you make will be overwritten.
*   Headers and footers will be added to each page automatically. The template
    for these can be found in the 'page_base.src' file.
*   A title will be added to each page, according to its file name.
*   Every directory must contain an 'index.md' file. The contents of that file
    will be automatically appended with a list of the pages and sub-directories
    inside that directory.
*   The meta-information on the top of source files is used to construct the
    link lists inside index files. The description of each page in that list
    will be the value of its 'Summary' attribute. The description of each
    sub-directory will be the value of the 'Summary' attribute of that
    directory's index page.
*   EDoc-generated documentation must be copied manually to 'resources/doc'
    periodically, by running 'make update_proper_docs' on a system that has a
    recent version of EDoc.
*   Other resources (images, stylesheets, pdfs etc.) can be found in the
    'resources' directory.

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

*   Where to go now (download, read the user guide, see the docs, contact etc.)
*   "Current version is ..."
*   links to other tools (dialyzer etc.)


Header
------

*   shirt icon (links to front page)
*   "PropEr" in fancy font (links to front page)
*   Horizontal navigation bar (have extra "Home" link?)
*   Second level of structure, for the current sub-directory


Footer
------

*   "Copyright ..."
*   "Powered by ..."
*   "Valid HTML ..."
*   "This page was last edited on ..."
*   "Icon by Gordon Irving"


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
*   Tutorials (e.g. simple lists properties, re2, proc_dict statem, comining
    PropEr and EUnit ...)
*   Demos, Sample Properties, showcase of QC-mode and PropEr-exclusive features
    (various demos from presentations, Kresten's properties, ec_dictionary, from
    open-source projects, ...)
*   links to (preferably locally saved) talks and papers


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
    - bio
    - part written
    - e-mail
    - personal webpage
*   PropEr's users
*   (Humorous) reviews
