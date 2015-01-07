import sys, os

sys.path.append(os.path.abspath('../sphinx/exts'))
extensions = ['includecode']

# General configuration

project = 'Basis'
copyright = '2012-2015 Chris Sachs'

master_doc = 'index'
highlight_language = 'scala'
add_function_parentheses = False

# HTML output options

html_title = 'Basis Documentation'

html_theme = 'basis'
html_theme_path = ['../sphinx/themes']

html_domain_indices = False
html_last_updated_fmt = '%b %d, %Y'
html_show_sourcelink = False
html_show_sphinx = False
html_use_index = False
html_use_smartypants = False
htmlhelp_basename = 'basisdoc'
