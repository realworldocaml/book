<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>
  <xsl:import href="system-xsl/profiling/profile.xsl"></xsl:import>

  <!-- For some reason, xsltproc omits the DTD from the file it
       outputs. Add a sensible one back in, because otherwise xmllint
       won't validate profiled documents. -->

  <xsl:template match="/">
    <xsl:text disable-output-escaping="yes"><![CDATA[
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.4//EN"
 "http://www.oasis-open.org/docbook/xml/4.4/docbookx.dtd">
  ]]></xsl:text>
    <xsl:apply-templates select="." mode="profile"/>
  </xsl:template> 
</xsl:stylesheet>
