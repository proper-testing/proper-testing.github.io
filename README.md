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
*   pages_src/User_Guide.md is copied from PropEr's README.md file. Do not edit
    this file directly: any changes you make will be overwritten.
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
*   The User Guide and the EDoc-generated API documentation must be copied
    manually from the PropEr source tree periodically, by running
    `make update_docs` on a system that has a recent version of EDoc.
*   Other resources (images, style sheets, pdfs etc.) can be found in the
    'resources' directory.
*   To specify the width and/or height of included images, import them using
    the HTML tag directly:
        <img src="..." alt="..." title="..." width="..." height="..." />
*   Anything inside the `private` folder will not be copied to the site. You can
    use this folder to track things that we don't want to make public (like .ppt
    sources of slides).

[1]: http://daringfireball.net/projects/markdown/
[2]: http://www.freewisdom.org/projects/python-markdown/Available_Extensions


Workflow
--------

1.  Make your changes.
2.  Run `make` to compile the pages.
3.  Run `make test` to launch a minimal web server that will serve the site. The
    site's pages will be available in any browser at [http://localhost:8080].
    When you're done, give Ctrl+C to stop the server.
4.  If you're not happy with the result, go back to step 1.
5.  Commit and push your changes.
6.  Compile the pages once more (using `make`) to get the last edit dates right.
7.  Run `make upload` to upload the changes to the website.


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

*   more breathing room for logo
*   Second level of structure, for the current sub-directory


Footer
------

*   "Copyright ..."
*   license
*   "Powered by ..."
*   "Valid HTML ..."
*   "This page was last edited on ..." (make this more fine-grained?)
*   page author information here?


Help/Support
------------

*   what to do when you need assistance (contact information, issues page on
    github)
*   Demos, Sample Properties, showcase of QC-mode and PropEr-exclusive features
    (various demos from presentations, Kresten's properties, ec_dictionary, from
    open-source projects, ...)


Tutorials
---------

*   tutorial(s) on the simple use of PropEr (writing properties for testing
    purely functional code), e.g. simple lists properties
*   tutorial(s) on the use of PropEr-specific extensions (types as generators,
    spec testing, Auto-ADT)
*   tutorial on combining PropEr and EUnit: combine their use, go from EUnit to
    PropEr, use PropEr counterexamples as unit tests, ...
*   re2 extended demo (full process of specification, re vs re2, generators from
    regexps, generators or even native types as string,regexp,options)
*   quickstart (head-first) tutorial: include the header file, you only need
    ?FORALL and some basic types ...


Tips
----

*   tips on using PropEr, generators, Erlang types, ...
*   where to put properties: in separate files, or in the same file, but
    surrounded by ifdef(TEST)
*   stuff from the paper's "Practical Experiences" chapter (both printed and
    commented out) should be added to "Tips"
*   Quickstart, Common problems, How-Tos, Common mistakes/blunders, Common user
    errors, Comments on received tests, User misunderstandings


FAQ
---

*   GPL/license question(s) (see issue, mailing list, FSF correspondance)


API
---

*   no-frames version of API?
*   what is the main proper header?
*   what is a raw_type?
*   fix variable names (RawType vs. Type etc.)
*   background in code listings?
*   copy function descriptions from the paper (and other sources...)


Publications
------------

*   talks and papers: bibliography info, bibtex, full text/slides (locally
    saved), abstract, link to video
*   diploma theses: could also just remove Greek pages
*   ACM Papers: Must include the following notice both embedded within the full
    text file and in the accompanying citation display as well:
    "© ACM, (YEAR). This is the author’s version of the work. It is posted here
    by permission of ACM for your personal use. Not for redistribution. The
    definitive version was published in PUBLICATION, {VOL#, ISS#, (DATE)}
    http://doi.acm.org/10.1145/{nnnnnn.nnnnnn}".
    The nnnnnn.nnnnnn number for the article's DOIs can be found on its citation
    page in the ACM Digital Library.
*   fill in bibliography information for types paper
*   add h2 "Papers" header? (and make "Talks" and "Diploma Theses" h2)?


Links
-----

*   link to external resources on PropEr (interviews, podcasts, tutorials,
    chapter in Learn You Some Erlang, QuickCheck stuff, non-academic talks --
    Kostis' too, presentations, podcasts etc.
*   Kostis Sagonas: "Cool Tools for Modern Erlang Program Development" talk:
        http://erlang-factory.com/conference/SFBay2011/speakers/KostisSagonas
    Trapexit: QuickCheck tutorials:
        http://www.trapexit.org/Category:QuickCheck
    Minute With Riak: Property Based Testing podcast:
        http://riak.minutewith.com/pages/20110128
    Torben Hoffmann: "Property Based Testing For Unit Testers" tutorial:
        https://docs.google.com/document/pub?id=1kTLvLKDpYBBKgo0S0W4sXUI5-BzAIDYFFfqqklEawao
    Cretors of QuickCheck: main QC site: tutorials:
        http://quviq.com/
*   link to QuickCheck talks?
*   link to PropEr announcement?


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
*   patch pygments to parse specs, records etc.


Download
--------

*   Packaged versions (for each versions, or automatically produced using a
    post-hook)


About
-----

*   Group photo
*   Per author:
    - name
    - photo
    - bio (funny version?)
    - CV?
    - part written
    - e-mail?
    - personal webpage
*   PropEr's users
*   Acknowledgments:
    -   "Icon by Gordon Irving, edited by Thanos Tintinidis"
    -   "Tutorial illustrations by ..., creator of Learn You Some Erlang (link)"
*   link to QuviQ's site


Humor
-----

*   Change the page descriptions to something funnier:
    About
    :   "The PropEr developers"
    API
    :   "Browse our PropEr API", or "The PropEr API and its documentation"
    Download
    :    "PropEr instructions on how to do this"
    FAQ
    :   Frequently Asked Questions with PropEr Answers
    Publications
    :   PropEr papers and talks
    Tips
    :   "PropEr suggestions for effective use of the tool", or
        For the effective use of PropEr
    Tutorials
    :   "PropEr tutorials", or "Showing the tool's PropEr use"
*   About page: humorous reviews:
    What people have said about PropEr:
    -   Stavros Aronis: "PropEr is a QuickCheck parody."
    -   Kostis Sagonas: "Good job, now make it produce properties from specs."
*   About page: tweets (good and bad)
*   Talks: humorous version of EF'11 London presentation with "times said
    'proper'" counter
*   history of PropEr (with comics from the EF2011 London comics)


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
*   Support for floating images: img without extra information is floating, if
    it is of some specific class, it's block-level. Then, images given using
    Markdown notation are floating, while images given with an HTML tag can have
    the class declaration and be block-level.
*   give some breathing room to the logo
*   add a post-commit hook on greedy, to automatically build and upload the site
    on each commit (pull?)

<!-- kate: replace-tabs-save on; replace-tabs on; tab-width 8; -->
