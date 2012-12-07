#!/usr/bin/env python2.7
"""
Generates the HTML commenting site for the Real World OCaml book.

Contributors:
    Dave Hall <dave@etianen.com>

Requirements for this script can be found in requirements.txt, and installed
using `pip install -r requirements.txt`.

If you want to run this generator inside a virtual environment, then run the
following commands from within the `commenting` dir:

    $ virtualenv venv --distribute
    $ source venv/bin/activate
    $ pip install -r requirements.txt
    
Then, whenever you want to run this generator, use the following command:

    $ python bin/generate_commenting_site.py
"""

import argparse, os.path, logging, sys, shutil, glob, traceback

from bs4 import BeautifulSoup

from django.template.loader import render_to_string
from django.conf import settings


def panic(msg, code=1):
    """Logs the given error, then exits."""
    logging.error(msg)
    logging.debug("".join(traceback.format_stack()[:-1]))
    sys.exit(code)
    
    
def parse_args():
    """Parses the arguments from sys.argv"""
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument("--verbose", "-v",
        action = "count",
        help = "Make the output more verbose, can be given up to two times",
        default = 0,
    )
    parser.add_argument("--prefix",
        dest = "prefix",
        default = None,
        help = "The filesystem prefix of the build (defaults to repo root)",
    )
    parser.add_argument("--src-dir",
        dest = "src_dir",
        default = "build",
        help = "The source HTML directory, relative to PREFIX (defaults to 'build')",
    )
    parser.add_argument("--dst-dir",
        dest = "dst_dir",
        default = "commenting-build",
        help = "The destination HTML directory, relative to PREFIX (defaults to 'commenting-build')",
    )
    parser.add_argument("--template-dir",
        dest = "template_dir",
        default = os.path.join("commenting", "templates"),
        help = "The Django template directory, relative to PREFIX (defaults to 'commenting/templates')",
    )
    parser.add_argument("--build-template-dir",
        dest = "build_template_dir",
        default = os.path.join("commenting", "build_template"),
        help = "The folder containing all default items to be copied into the DST_DIR, relative to PREFIX (defaults to 'commenting/build_template')",
    )
    parser.add_argument("--debug", "-d",
        dest = "debug",
        default = False,
        action = "store_true",
        help = "Builds the site in debug mode, with unminified assets.",
    )
    parser.add_argument("--github-user",
        dest = "github_user",
        default = "etianen",  # TODO: Make the public repo owner default.
        help = "The user that owns the GitHub repo to use for issues.",
    )
    parser.add_argument("--github-repo",
        dest = "github_repo",
        default = "OCaml-Book",
        help = "The name of the GitHub repo to use for issues.",
    )
    parser.add_argument("--github-milestone",
        dest = "github_milestone",
        required = True,
        help = "The GitHub milestone to use for issues.",
    )
    return parser.parse_args()


def configure_logging(verbosity):
    """Sets up logging based on the given verbosity."""
    logging.basicConfig(format="%(levelname)s: %(message)s")
    logging.captureWarnings(True)
    logging_level = (3 - verbosity) * 10
    logging.getLogger().setLevel(logging_level)
    logging.debug("Logging level set to {}".format(logging_level))
    
    
def configure_django(prefix, template_dir):
    """Configures the Django components used by this script."""
    # Parse the template dir.
    template_dir = os.path.join(prefix, template_dir)
    logging.debug("Django template directory set to {}".format(template_dir))
    # Configure Django.
    settings.configure(
        TEMPLATE_DIRS = (
            template_dir,
        ),
    )


def configure_src_dir(prefix, src_dir):
    """Parses the src dir, and checks that it exists."""
    # Parse the src dir.
    src_dir = os.path.join(prefix, src_dir)
    logging.debug("Source directory set to {}".format(src_dir))
    # Check that the src dir exists.
    if not os.path.exists(src_dir):
        panic("Source directory {} does not exist".format(src_dir))
    # Return parsed src dir.
    return src_dir


def configure_dst_dir(prefix, dst_dir):
    """Parses the dst dir, clears it if necessary, and then creates it."""
    # Parse the dst dir.
    dst_dir = os.path.join(prefix, dst_dir)
    logging.debug("Destination directory set to {}".format(dst_dir))
    # Delete existing destination directory.
    if os.path.exists(dst_dir):
        logging.info("Deleting existing destination directory")
        shutil.rmtree(dst_dir)
    # Return parsed dst dir.
    return dst_dir


def configure_build_template_dir(prefix, dst_dir, build_template_dir):
    """Parses the build template dir, and checks that it exists."""
    # Parse the build template dir.
    build_template_dir = os.path.join(prefix, build_template_dir)
    logging.debug("Build template directory set to {}".format(build_template_dir))
    # Check that the build template dir exists.
    if not os.path.exists(build_template_dir):
        panic("Build template directory {} does not exist".format(build_template_dir))
    # Copy over the template dir.
    shutil.copytree(build_template_dir, dst_dir)
    # Return parsed build template dir.
    return build_template_dir


def copy_locale_dir(locale_src_dir, locale_dst_dir, dirname):
    """Copies the named directory from the src to the dst."""
    logging.debug("Copying {} from {} to {}".format(dirname, locale_src_dir, locale_dst_dir))
    shutil.copytree(
        os.path.join(locale_src_dir, dirname),
        os.path.join(locale_dst_dir, dirname),
    )


def find_required(html_name, soup, *args, **kwargs):
    """Finds the named element in the given soup, or panics."""
    element = soup.find(*args, **kwargs)
    if element:
        return element
    panic("Could not find {!r} in {}".format(args, html_name))
    
    
def render_html_template(template_name, soup, args, context):
    """Renders the given html template, returning the unicode result."""
    context.setdefault("debug", args.debug)
    context.setdefault("github_user", args.github_user)
    context.setdefault("github_repo", args.github_repo)
    context.setdefault("github_milestone", args.github_milestone)
    # Find the next and previous links.
    prev_page_link = soup.find("link", attrs={"rel": "prev"})
    prev_page = prev_page_link and prev_page_link["href"] or None
    context.setdefault("prev_page", prev_page)
    next_page_link = soup.find("link", attrs={"rel": "next"})
    next_page = next_page_link and next_page_link["href"] or None
    context.setdefault("next_page", next_page)
    # Render the template already!
    return render_to_string(template_name, context)


def render_locale_part_html(html_name, soup, navigation_list, args):
    """Process an index HTML page, returning a string of processed HTML."""
    logging.debug("Processing {} as a part".format(html_name))
    # Get the title.
    title = soup.find("title").get_text()
    # Render the template.
    return render_html_template("index.html", soup, args, {
        "title": title,
        "navigation_list": navigation_list,
        "page_navigation_list": parse_locale_toc_element(html_name, soup),
        "html_name": html_name,
    })
    
    
def render_locale_index_html(html_name, soup, navigation_list, args):
    """Process an index HTML page, returning a string of processed HTML."""
    logging.debug("Processing {} as a table of contents".format(html_name))
    # Render the template.
    return render_html_template("index.html", soup, args, {
        "title": "Table of Contents",
        "navigation_list": navigation_list,
        "page_navigation_list": navigation_list,
        "html_name": html_name,
    })
    
    
def process_locale_chapter_page_section(html_name, section):
    """Sanitises the given section from a chapter page."""
    logging.debug("Processing section '{}' in {}".format(section["title"], html_name))
    # Make into HTML5 section.
    section.name = "section"
    del section["class"]
    del section["title"]
    # Replace titlepage div with HTML5 section h1.
    titlepage = find_required(html_name, section, "div", "titlepage", recursive=False)
    heading = find_required(html_name, titlepage, ["h1", "h2", "h3", "h4", "h5", "h6", "h7", "p"])
    heading.name = "h1"
    section.insert(0, heading)
    titlepage.extract()
    
    
def render_locale_chapter_page(html_name, soup, navigation_list, args):
    """Processes a chaper page, returning a string of processed HTML."""
    logging.debug("Processing {} as a chapter page".format(html_name))
    # Get the title.
    title = soup.find("title").get_text()
    # Get the part.
    part_html_name = find_required(html_name, soup, "link", attrs={"rel": "up"})["href"]
    # Find the chapter root element.
    chapter_root = soup.find("div", "chapter")
    # Strip out some unneccesary elements.
    [e.extract() for e in chapter_root.find_all("div", "titlepage", recursive=False)]
    [e.extract() for e in chapter_root.find_all("div", "toc", recursive=False)]
    # Process sections.
    for n in xrange(1, 10):
        for section in chapter_root.find_all("div", "sect{}".format(n)):
            process_locale_chapter_page_section(html_name, section)
    for section in chapter_root.find_all("div", "sidebar"):
        process_locale_chapter_page_section(html_name, section)
    # Remove wrappers around lists.
    for element in chapter_root.find_all("div", "itemizedlist"):
        element.replaceWith(element.find("ul", recursive=False))
    # Remove wrappers around tables.
    for element in chapter_root.find_all("div", "informaltable"):
        element.replaceWith(element.find("table", recursive=False))
    # Remove wrappers around asides.
    for section_class in ("note", "important", "warning", "tip"):
        for element in chapter_root.find_all("div", section_class):
            aside = soup.new_tag("aside")
            aside["class"] = section_class
            element.replaceWith(aside)
            header = soup.new_tag("h1")
            header.append(find_required(html_name, element, "th").get_text())
            aside.append(header)
            rows = element.find_all("tr")
            if len(rows) != 2:
                panic("Expect two rows in special section from {}".format(html_name))
            for child in find_required(html_name, rows[1], "td").find_all(True, recursive=False):
                aside.append(child)
    # Clean up table colgroups.
    for element in chapter_root.find_all("colgroup"):
        element.extract()
    # Remove all styles and classes.
    for element in chapter_root.find_all(True):
        if element.name not in ("section", "aside"):
            del element["class"]
            del element["style"]
            del element["title"]
            del element["type"]
            del element["align"]
            del element["valign"]
            del element["border"]
            del element["cellspacing"]
            del element["cellpadding"]
    # Convert chapter root to string.
    content_html = u"".join(unicode(e) for e in chapter_root.find_all(True, recursive=False))
    # Render the template.
    return render_html_template("chapter.html", soup, args, {
        "title": title,
        "content_html": content_html,
        "navigation_list": navigation_list,
        "html_name": html_name,
        "part_html_name": part_html_name,
    })
    
    
def load_locale_html_as_soup(locale_src_dir, html_name):
    """Loads the named HTML file as a soup object."""
    locale_src_path = os.path.join(locale_src_dir, html_name)
    # Read the source HTML file.
    logging.debug("Reading source HTML for {}".format(html_name))
    with open(locale_src_path, "rb") as locale_src_handle:
        locale_src_html = locale_src_handle.read()
    # Parse the source HTML file.
    logging.debug("Parsing HTML for {}".format(html_name))
    return BeautifulSoup(locale_src_html)
    
    
def process_locale_html(locale_src_dir, locale_dst_dir, html_name, navigation_list, args):
    """Processes the given HTML file and writes it to the destination dir."""
    logging.debug("Processing HTML file {}".format(html_name))
    # Generate paths.
    locale_dst_path = os.path.join(locale_dst_dir, html_name)
    soup = load_locale_html_as_soup(locale_src_dir, html_name)
    # Is this a table of contents?
    if soup.find("div", "book"):
        locale_dst_html = render_locale_index_html(html_name, soup, navigation_list, args)
    elif soup.find("div", "part"):
        locale_dst_html = render_locale_part_html(html_name, soup, navigation_list, args)
    elif soup.find("div", "chapter"):
        locale_dst_html = render_locale_chapter_page(html_name, soup, navigation_list, args)
    else:
        panic("Unknown page type: {}".format(html_name))
    # Write the destination HTML to disc.
    logging.debug("Writing processed HTML for {}".format(html_name))
    with open(locale_dst_path, "wb") as locale_dst_handle:
        locale_dst_handle.write(locale_dst_html.encode("utf-8"))


def parse_locale_toc_element(html_name, soup):
    """Parse the table of contents from a soup."""
    # Find the root table of contents.
    toc = find_required(html_name, soup, "div", "toc")
    root_dl = find_required(html_name, toc, "dl", recursive=False)
    # Recursively parse the table of contents.
    def parse_toc(parent_dl):
        children = []
        for element in parent_dl.find_all(("dt", "dd"), recursive=False):
            if element.name == "dt":
                children.append({
                    "title": element.find("a").get_text(),
                    "href": element.find("a")["href"],
                    "children": [],
                })
            if element.name == "dd":
                children[-1]["children"].extend(parse_toc(element.find("dl", recursive=False)))
        return children
    return parse_toc(root_dl)

        
def parse_locale_toc(locale_src_dir):
    """Parses the TOC from the given local dir."""
    html_name = "index.html"
    soup = load_locale_html_as_soup(locale_src_dir, html_name)
    return parse_locale_toc_element(html_name, soup)


def process_locale(src_dir, dst_dir, locale, args):
    """Processes all files for the given locale."""
    logging.info("Processing HTML for locale {}".format(locale))
    # Process the src dir.
    locale_src_dir = os.path.join(src_dir, locale, "html")
    logging.debug("Locale source directory is {}".format(locale_src_dir))
    # Process the dst dir.
    locale_dst_dir = os.path.join(dst_dir, "www", locale, "html")
    logging.debug("Locale destination directory is {}".format(locale_dst_dir))
    # Create the dst dir.
    logging.debug("Creating locale destination directory")
    os.makedirs(locale_dst_dir)
    # Copy over figures.
    copy_locale_dir(locale_src_dir, locale_dst_dir, "figures")
    # Process the index.
    logging.debug("Parsing table of contents")
    navigation_list = parse_locale_toc(locale_src_dir)
    # Process HTML files.
    html_paths = glob.glob(os.path.join(locale_src_dir, "*.html"))
    logging.debug("Detected {} HTML file(s)".format(len(html_paths)))
    for html_path in html_paths:
        html_name = html_path[len(locale_src_dir)+1:]
        process_locale_html(locale_src_dir, locale_dst_dir, html_name, navigation_list, args)


def main():
    """Generates the commenting site from the chunked HTML build."""
    # Parse the arguments.
    args = parse_args()
    configure_logging(args.verbose)
    # Set up directory paths.
    prefix = os.path.abspath(args.prefix or os.path.join(os.path.dirname(__file__), "..", ".."))
    logging.debug("Prefix set to {}".format(prefix))
    src_dir = configure_src_dir(prefix, args.src_dir)
    dst_dir = configure_dst_dir(prefix, args.dst_dir)
    configure_build_template_dir(prefix, dst_dir, args.build_template_dir)
    # Configure Django.
    configure_django(prefix, args.template_dir)
    # Search for locales.
    locale_dirs = glob.glob(os.path.join(src_dir, "*", "html"))
    logging.debug("Detected {} locale(s): {}".format(len(locale_dirs), ", ".join(locale_dirs)))
    # Process each locale dir.
    for locale_dir in locale_dirs:
        locale = locale_dir[len(src_dir)+1:-5]
        process_locale(src_dir, dst_dir, locale, args)


if __name__ == "__main__":
    main()