#!/usr/bin/env python
# -*- coding: utf-8 -*-
# kate: replace-tabs on; replace-tabs-save on;

import os
import os.path
import commands
import codecs
import copy
from datetime import date
import markdown

#===============================================================================

def build(fs_path, title, extra, template):
    if os.path.isdir(fs_path):
        try: os.mkdir(switch_base_dir(fs_path))
        except OSError: pass
        files = os.listdir(fs_path)
        files.remove('index.md')
        files.sort()
        files.insert(0, 'index.md')
        fs_paths = [os.path.join(fs_path, f) for f in files]
        site_paths = [to_site_path(p) for p in fs_paths]
        titles = [get_title(f) for f in files]
        titles[0] = 'Main'
        extras = [add_navbar(extra, site_paths, titles, p) for p in site_paths]
        summaries = [build(p, t, e, template)
                     for (p,(t,e))
                     in zip(fs_paths[1:],zip(titles[1:],extras[1:]))]
        link_list = make_link_list(site_paths[1:], titles[1:], summaries)
        index_extra = add_content(extras[0], link_list)
        return build(fs_paths[0], title, index_extra, template)
    else:
        (summary, author, content) = parse_file(fs_path)
        write_html(fs_path, title, author, content, extra, template)
        return summary

#===============================================================================

def empty_extra():
    return {'content':'', 'pri_navbar':''}

def add_content(extra, content):
    new_extra = copy.copy(extra)
    new_extra['content'] = content
    return new_extra

def add_navbar(extra, site_paths, titles, curr_path):
    if extra['pri_navbar'] == '':
        navbar = make_navbar(site_paths, titles, curr_path, 'pri_navbar')
        new_extra = copy.copy(extra)
        new_extra['pri_navbar'] = navbar
        return new_extra
    else:
        return extra

def make_navbar(site_paths, titles, curr_path, navbar_id):
    list_items = [make_navbar_link(p, t, curr_path)
                  for (p,t) in zip(site_paths,titles)]
    list_block = ['<ul id="' + navbar_id + '">'] + list_items + ['</ul>']
    return ''.join(list_block)

def make_navbar_link(target, title, curr_path):
    if target == curr_path:
        extra_info = ' class="current"'
    else:
        extra_info = ''
    return ('<li' + extra_info + '>' +
            '<a href="' + target + '">' + title + '</a>' +
            '</li>')

def make_link_list(site_paths, titles, summaries):
    list_items = ['<li>' + '<a href="' + p + '">' + t + '</a>: ' + s + '</li>'
                  for (p,(t,s)) in zip(site_paths,zip(titles,summaries))]
    list_block = ['<h2>Contents</h2>','<ul>'] + list_items + ['</ul>']
    return '\n'.join(list_block)

#===============================================================================

def switch_base_dir(fs_path):
    path_components = split_path(fs_path)
    path_components = [remove_index(c) for c in path_components]
    path_components[0] = 'build'
    new_fs_path = path_components.pop()
    while path_components <> []:
        new_fs_path = os.path.join(path_components.pop(), new_fs_path)
    return switch_extension(new_fs_path)

def to_site_path(fs_path):
    path_components = split_path(fs_path)[1:]
    path_components = [remove_index(c) for c in path_components]
    path_components[-1] = switch_extension(path_components[-1])
    return '/' + '/'.join(path_components)

def get_title(filename):
    basename = remove_index(os.path.splitext(filename)[0])
    return basename.replace('_', ' ')

def remove_index(filename):
    return filename.split('#')[-1]

def split_path(fs_path):
    path_components = []
    while fs_path <> '':
        (head, tail) = os.path.split(fs_path)
        fs_path = head
        path_components.append(tail)
    path_components.reverse()
    return path_components

def switch_extension(path):
    (basepath, extension) = os.path.splitext(path)
    if extension == '':
        return basepath
    else:
        return basepath + '.html'

#===============================================================================

def parse_file(fs_path):
    in_file = codecs.open(fs_path, 'r', 'utf8')
    text = in_file.read()
    in_file.close()
    md = markdown.Markdown(extensions = ['extra','meta','codehilite','toc'])
    content = md.convert(text)
    summary = ' '.join(md.Meta['summary'])
    author = ' '.join(md.Meta.get('author', ['']))
    return (summary, author, content)

def write_html(fs_path, title, author, content, extra, template):
    out_fs_path = switch_base_dir(fs_path)
    if author <> '':
        author = '<p class="author_info">by ' + author + '</p>'
    timestamp_command = 'git log -1 --format=format:"%at%n" "' + fs_path + '"'
    timestamp = int(commands.getoutput(timestamp_command))
    edit_date = date.fromtimestamp(timestamp).isoformat()
    html = template.replace(
               '#TITLE#', title
           ).replace(
               '#AUTHOR_INFO#', author
           ).replace(
               '#PRI_NAVBAR#', extra['pri_navbar']
           ).replace(
               '#CONTENT#', content + extra['content']
           ).replace(
               '#EDIT_DATE#', edit_date
           )
    out_file = codecs.open(out_fs_path, 'w', 'utf8')
    out_file.write(html)
    out_file.close()

#===============================================================================

template_file = codecs.open('template.html', 'r', 'utf8')
template = template_file.read()
template_file.close()
build('pages_src', 'PropEr', empty_extra(), template)
