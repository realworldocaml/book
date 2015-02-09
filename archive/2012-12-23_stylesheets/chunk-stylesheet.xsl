<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>

  <xsl:import href="system-xsl/html/chunk.xsl"/>
  <xsl:include href="base-html-stylesheet.xsl"/>

  <!-- PARAMETER REFERENCE:                                         -->
  <!-- http://docbook.sourceforge.net/release/xsl/current/doc/html/ -->

  <!-- Uncomment this to enable auto-numbering of sections -->
  <!-- xsl:param name="section.autolabel" select="1" / -->
  <xsl:param name="chunker.output.encoding">UTF-8</xsl:param>
  <xsl:param name="use.id.as.filename" select="1"/>
  <xsl:param name="chunk.first.sections" select="0"/>
  <xsl:param name="chunk.section.depth" select="0"/>
  <xsl:param name="chunk.quietly" select="0"/>

</xsl:stylesheet>
