<?xml version="1.0" encoding="utf-8"?>

<!-- Prepare an ASCII dump file of all IDs, and the pages in which
     they live, for loading into a database. Assumes one-level chunked
     HTML output, with each chunk containing either a chapter or
     sect1. -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text"/>
  <xsl:strip-space elements="title"/>

  <xsl:template match="/">
    <xsl:for-each select="//preface|//chapter|//appendix|//bibliography|//sect1">
      <xsl:variable name="id">
        <xsl:choose>
          <xsl:when test="local-name(.)='sect1'">
            <xsl:value-of select="../@id"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@id"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="sectitle">
        <xsl:value-of select="normalize-space(./title)"/>
      </xsl:variable>
      <xsl:for-each select=".//para[@id]|.//programlisting[@id]|.//screen[@id]">
        <xsl:value-of select="@id"/>
        <xsl:text>|</xsl:text>
        <xsl:copy-of select="$id"/>
        <xsl:text>|</xsl:text>
        <xsl:copy-of select="$sectitle"/>
        <xsl:text>&#x0a;</xsl:text>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
