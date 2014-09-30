#!/usr/bin/env python
# This script is originally from the YouCompleteMe project.

from bs4 import BeautifulSoup
from markdown import markdown
import fileinput

markdown_lines = list(fileinput.input())

# Delete the header
del markdown_lines[:2]

markdown_source = ''.join(markdown_lines)

with open('index.html', 'r+') as content_file:
    content = content_file.read()

    new_contents = markdown(unicode(markdown_source, 'utf-8'),
                            extensions=['fenced_code'])
    new_tags = BeautifulSoup(new_contents, 'html5lib')

    # soup = BeautifulSoup(content, "html5lib")
    # Use the normal html parser so it doesn't add html/body tags
    # around our fragment
    soup = BeautifulSoup(content, "html.parser")
    elem = soup.find(id="markdown-output")
    elem.clear()
    for new_elem in new_tags.body.contents:
        elem.append(new_elem)

    content_file.seek(0)
    content_file.truncate()
    content_file.write(str(soup))
