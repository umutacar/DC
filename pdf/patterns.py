## BEGIN Latex patterns
TEX_CHAPTER_HEADING = r'\chapter{%s}' + '\n'
TEX_CHAPTER_LABEL = r'\label{ch:%s}' + '\n'
TEX_CHAPTER_PREAMBLE = r'''
\begin{preamble}
%s
\end{preamble}
'''

TEX_ATOM_SLIDE = r'''
\begin{slide}
\includegraphics[width=\textwidth]{%s}
\end{slide}
    '''

### END Latex patterns
# Usage: XML_INCLUDE_PDF % filename
XML_INCLUDE_PDF = r'''<embed src='%s' width='500px'/>'''

## BEGIN XML patterns
# Usage XML_CHAPTER % (title, title_src, label)
XML_CHAPTER_HEADING = r'''
<block name='chapter'>
<field name='title'>
<![CDATA[
%s
]]>
</field> <!-- title -->
<field name='title_src'>
<![CDATA[
%s
]]>
</field> <!-- title_src -->
<field name='label'>
%s
</field> <!-- label -->
'''
XML_CHAPTER_ENDING = r'''
</block> <!-- chapter -->
'''
# usage XML_PREAMBLE % (body, body_src)
XML_PREAMBLE = r'''
<atom name='preamble'>
<field name='title'>
<![CDATA[
...NOT.PROVIDED.TITLE...
]]>
</field> <!-- title -->
<field name='title_src'>
<![CDATA[
...NOT.PROVIDED.TITLE...
]]>
</field> <!-- title_src -->
<field name='label'>
...NOT.PROVIDED.LABEL...
</field> <!-- label -->
<field name='body'>
<![CDATA[
%s
]]>
</field> <!-- body -->
<field name='body_src'>
<![CDATA[
%s
]]>
</field> <!-- body_src -->
</atom> <!-- preamble -->
'''

# Usage: XML_SLIDE % (body, body_src)
XML_SLIDE = r'''
<atom name='slide'>
<field name='title'>
<![CDATA[
...NOT.PROVIDED.TITLE...
]]>
</field> <!-- title -->
<field name='title_src'>
<![CDATA[
...NOT.PROVIDED.TITLE...
]]>
</field> <!-- title_src -->
<field name='label'>
...NOT.PROVIDED.LABEL...
</field> <!-- label -->
<field name='body'>
<![CDATA[
%s
]]>
</field> <!-- body -->
<field name='body_src'>
<![CDATA[
%s
]]>
</field> <!-- body_src -->
</atom> <!-- slide -->
'''
