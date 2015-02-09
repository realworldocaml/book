<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>
                
  <xsl:import href="../chunk-stylesheet.xsl"/>
  <xsl:param name="l10n.gentext.language" select="'en'"/>

  <xsl:template match="programlisting">
    <xsl:variable name="id">
     <xsl:call-template name="object.id"/>
    </xsl:variable>
    <xsl:call-template name="anchor"/>
    <pre>
      <xsl:apply-templates select="." mode="common.html.attributes"/>
        <xsl:if test="@width != ''">
          <xsl:attribute name="width">
            <xsl:value-of select="@width"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:if test="@language != ''">
          <xsl:attribute name="language">
            <xsl:value-of select="@language"/>
          </xsl:attribute>
        </xsl:if>
         <xsl:attribute name="id">
            <xsl:value-of select="$id"/>
         </xsl:attribute>
        <xsl:apply-templates/>
    </pre>
  </xsl:template>
  
</xsl:stylesheet>
