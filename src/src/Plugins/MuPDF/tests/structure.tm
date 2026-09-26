<TeXmacs|2.1.5>

<style|article>

<\body>
  <doc-data|<doc-title|Structure test>|<doc-author|<author-data|<author-name|André Tëster>>>>

  <table-of-contents|toc|>

  <section|First section><label|sec-first>

  This section points at <reference|sec-deep> and at <reference|sec-second>,
  and out of the document at <hlink|the TeXmacs
  site|https://www.texmacs.org>.

  <subsection|A subsection>

  <subsubsection|A deeper one><label|sec-deep>

  Text under the deepest heading.

  <subsection|Another subsection>

  <section|Second section><label|sec-second>

  Back to <reference|sec-first>.

  <subsection|Last subsection>

  <subsection|Größe à Paris><label|sec-été>

  Accents everywhere: back to <reference|sec-été>, and to <hlink|a page on
  summer|https://fr.wikipedia.org/wiki/Été>.

  <new-page>

  <section|Third section, on another page>

  An anchor on the second page: <label|on-page-two>see
  <reference|on-page-two>.
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.1.1|?>>
    <associate|auto-4|<tuple|1.2|?>>
    <associate|auto-5|<tuple|2|?>>
    <associate|auto-6|<tuple|2.1|?>>
    <associate|auto-7|<tuple|3|?>>
    <associate|on-page-two|<tuple|3|?>>
    <associate|sec-deep|<tuple|1.1.1|?>>
    <associate|sec-first|<tuple|1|?>>
    <associate|sec-second|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>First
      section> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>A subsection
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <with|par-left|<quote|2tab>|1.1.1<space|2spc>A deeper one
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|1tab>|1.2<space|2spc>Another subsection
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Second
      section> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.5fn>

      <with|par-left|<quote|1tab>|2.1<space|2spc>Last subsection
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Third
      section, on another page> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>