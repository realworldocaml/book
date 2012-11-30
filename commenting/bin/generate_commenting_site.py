#!/usr/bin/env python2.7

import argparse, os.path, logging, sys, shutil


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
        default = None,
        help = "The source HTML directory, relative to PREFIX (defaults to 'build')",
    )
    parser.add_argument("--dst-dir",
        dest = "dst_dir",
        default = None,
        help = "The destination HTML directory, relative to PREFIX (defaults to 'commenting-build')",
    )
    return parser.parse_args()


def configure_logging(verbosity):
    """Sets up logging based on the given verbosity."""
    logging.basicConfig(format="%(levelname)s: %(message)s")
    logging.captureWarnings(True)
    logging_level = (3 - verbosity) * 10
    logging.getLogger().setLevel(logging_level)
    logging.debug("Logging level set to {}".format(logging_level))


def configure_src_dir(prefix, src_dir=None):
    """Parses the src dir, and checks that it exists."""
    # Parse the src dir.
    src_dir = os.path.join(prefix, src_dir or "build")
    logging.debug("Source directory set to {}".format(src_dir))
    # Check that the src dir exists.
    if not os.path.exists(src_dir):
        panic("Source directory {} does not exist".format(src_dir))
    # Return parsed src dir.
    return src_dir


def configure_dst_dir(prefix, dst_dir=None):
    """Parses the dst dir, clears it if necessary, and then creates it."""
    # Parse the dst dir.
    dst_dir = os.path.join(prefix, dst_dir or "commenting-build")
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


def main():
    """Generates the commenting site from the chunked HTML build."""
    # Parse the arguments.
    args = parse_args()
    configure_logging(args.verbose)
    # Set up src and dst dirs.
    prefix = os.path.abspath(args.prefix or os.path.join(os.path.dirname(__file__), "..", ".."))
    logging.debug("Prefix set to {}".format(prefix))
    src_dir = configure_src_dir(prefix, args.src_dir)
    dst_dir = configure_dst_dir(prefix, args.dst_dir)


if __name__ == "__main__":
    main()