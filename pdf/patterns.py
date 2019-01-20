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

## BEGIN XML patterns
# Usage CHAPTER % (title, title_src, label)
CHAPTER = r'''
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

# usage PREAMBLE % (body, body_src)
PREAMBLE = r'''
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

# Usage: SLIDE % (body, body_src)
SLIDE = r'''
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
