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

import argparse, os.path, logging, sys, shutil, glob
from itertools import izip

from bs4 import BeautifulSoup

from django.template.loader import render_to_string
from django.conf import settings


def panic(msg, code=1):
    """Logs the given error, then exits."""
    logging.error(msg)
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
    parser.add_argument("--media-dir",
        dest = "media_dir",
        default = os.path.join("commenting", "media"),
        help = "The folder containing all media items, relative to PREFIX (defaults to 'commenting/media')",
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
    # Create new destination directory.
    logging.info("Creating destination directory")
    os.mkdir(dst_dir)
    # Return parsed dst dir.
    return dst_dir


def configure_media_dir(prefix, media_dir):
    """Parses the media dir, and checks that it exists."""
    # Parse the media dir.
    media_dir = os.path.join(prefix, media_dir)
    logging.debug("Media directory set to {}".format(media_dir))
    # Check that the media dir exists.
    if not os.path.exists(media_dir):
        panic("Media directory {} does not exist".format(media_dir))
    # Return parsed media dir.
    return media_dir


def copy_local_dir(locale_src_dir, locale_dst_dir, dirname):
    """Copies the named directory from the src to the dst."""
    logging.debug("Copying {} from {} to {}".format(dirname, locale_src_dir, locale_dst_dir))
    shutil.copytree(
        os.path.join(locale_src_dir, dirname),
        os.path.join(locale_dst_dir, dirname),
    )
    
    
# TODO: Use r.js to optimize the media.
def copy_locale_media_dir(media_dir, locale_dst_dir):
    """Copies the media dir to the locale destination dir."""
    locale_dst_media_dir = os.path.join(locale_dst_dir, "media")
    logging.debug("Copying media from {} to {}".format(media_dir, locale_dst_media_dir))
    shutil.copytree(media_dir, locale_dst_media_dir)
    
    
def process_locale_index_html(html_name, soup):
    """Process an index html page returning a string of processed data."""
    logging.debug("Processing {} as a table of contents".format(html_name))
    # Get the title.
    title = soup.find("title").get_text()
    # Find the root table of contents.
    toc = soup.find("div", "toc")
    if not toc:
        panic("Could not find `div.toc` in {}".format(html_name))
    root_dl = toc.find("dl", recursive=False)
    if not root_dl:
        panic("Could not find `div.toc > dl` in {}".format(html_name))
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
    navigation_list = parse_toc(root_dl)
    # Render the template.
    return render_to_string("index.html", {
        "title": title,
        "navigation_list": navigation_list,
    })
    
    
def process_locale_html(locale_src_dir, locale_dst_dir, html_name):
    """Processes the given HTML file and writes it to the destination dir."""
    logging.debug("Processing HTML file {}".format(html_name))
    # Generate paths.
    locale_src_path = os.path.join(locale_src_dir, html_name)
    locale_dst_path = os.path.join(locale_dst_dir, html_name)
    # Read the source HTML file.
    logging.debug("Reading source HTML for {}".format(html_name))
    with open(locale_src_path, "rb") as locale_src_handle:
        locale_src_html = locale_src_handle.read()
    # Parse the source HTML file.
    logging.debug("Parsing HTML for {}".format(html_name))
    soup = BeautifulSoup(locale_src_html)
    # Is this a table of contents?
    if soup.find("div", "book"):
        locale_dst_html = process_locale_index_html(html_name, soup)
    else:
        # TODO: Error if unknown page type.
        locale_dst_html = render_to_string("base.html", {
            "title": soup.find("title").get_text(),
        })
    # Write the destination HTML to disc.
    logging.debug("Writing processed HTML for {}".format(html_name))
    with open(locale_dst_path, "wb") as locale_dst_handle:
        locale_dst_handle.write(locale_dst_html.encode("utf-8"))


def process_locale(src_dir, dst_dir, media_dir, locale):
    """Processes all files for the given locale."""
    logging.info("Processing HTML for locale {}".format(locale))
    # Process the src dir.
    locale_src_dir = os.path.join(src_dir, locale, "html")
    logging.debug("Locale source directory is {}".format(locale_src_dir))
    # Process the dst dir.
    locale_dst_dir = os.path.join(dst_dir, locale, "html")
    logging.debug("Locale destination directory is {}".format(locale_dst_dir))
    # Create the dst dir.
    logging.debug("Creating locale destination directory")
    os.makedirs(locale_dst_dir)
    # Copy over figures.
    copy_local_dir(locale_src_dir, locale_dst_dir, "figures")
    # Copy over media items.
    copy_locale_media_dir(media_dir, locale_dst_dir)
    # Process HTML files.
    html_paths = glob.glob(os.path.join(locale_src_dir, "*.html"))
    logging.debug("Detected {} HTML file(s)".format(len(html_paths)))
    for html_path in html_paths:
        html_name = html_path[len(locale_src_dir)+1:]
        process_locale_html(locale_src_dir, locale_dst_dir, html_name)


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
    media_dir = configure_media_dir(prefix, args.media_dir)
    # Configure Django.
    configure_django(prefix, args.template_dir)
    # Search for locales.
    locale_dirs = glob.glob(os.path.join(src_dir, "*", "html"))
    logging.debug("Detected {} locale(s): {}".format(len(locale_dirs), ", ".join(locale_dirs)))
    # Process each locale dir.
    for locale_dir in locale_dirs:
        locale = locale_dir[len(src_dir)+1:-5]
        process_locale(src_dir, dst_dir, media_dir, locale)


if __name__ == "__main__":
    main()