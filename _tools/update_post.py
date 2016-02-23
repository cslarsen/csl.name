#! /usr/bin/env python

"""
Updates the "updated: " field in Jekyll posts, using the most recent git commit
date.

Usage:
    python update_post.py file1.md file2.md
"""

import datetime
import dateutil.parser
import os
import shutil
import subprocess
import sys
import tempfile

def gitlog(filename):
    "Yields every line of a git log."
    lines = subprocess.check_output(["git", "log", filename])
    for line in lines.split("\n"):
        yield line

def newest_commit_date(filename):
    "Returns date-string of most recent commit."
    prefix = "Date:   "
    for line in gitlog(filename):
        if line.startswith(prefix):
            return line[len(prefix):]

def parse_date(s):
    """Converts string to datetime instance."""
    return dateutil.parser.parse(s)

def updated(filename):
    "Returns date and time of latest git commit of file."
    return parse_date(newest_commit_date(filename))

def format_datetime(dt):
    """Formats datetime object as 'YYYY-MM-DD HH:MM:SS +/-ZZ:ZZ'."""
    secs = dt.utcoffset().seconds
    hours = secs / 3600
    minutes = secs % 3600
    sign = "+" if (hours > 0 or minutes > 0) else "-"
    return "%04d-%02d-%02d %02d:%02d:%02d %c%02d:%02d" % ( dt.year, dt.month,
            dt.day, dt.hour, dt.minute, dt.second, sign, hours, minutes)

def update_post(filename):
    """Replaces the "Updated:" field in a file with its latest commit date."""
    if not (filename.endswith(".md") or filename.endswith(".markdown")):
        raise ValueError("Will only work on markdown files: %s" % filename)

    date = updated(filename)
    updated_line = "updated: %s\n" % format_datetime(date)
    written = False
    dividers = 0

    with tempfile.TemporaryFile() as temp:
        # Copy post to tempfile, replacing or inserting the updated line
        with open(filename, "rt") as post:
            for line in post.readlines():
                if line.rstrip() == "---":
                    dividers += 1
                    if dividers == 2:
                        # The updated field was not found, make one
                        if not written:
                            temp.write(updated_line)
                            written = True
                        temp.write("---\n")
                    else:
                        temp.write("---\n")
                elif line.startswith("updated:"):
                    if not written:
                        temp.write(updated_line)
                        written = True
                else:
                    temp.write(line)

        # Copy contents of tempfile over post
        backup = "%s.backup" % filename
        shutil.copyfile(filename, backup) # in case of exceptions

        try:
            temp.seek(0)
            with open(filename, "wt") as post:
                print("%s %s" % (format_datetime(date), filename))
                shutil.copyfileobj(temp, post)
            # all ok, remove backup
            os.unlink(backup)
        except:
            # restore backup
            shutil.move(backup, filename)
            raise

if __name__ == "__main__":
    for filename in sys.argv[1:]:
        update_post(filename)
