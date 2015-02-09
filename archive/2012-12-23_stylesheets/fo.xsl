<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
      xmlns:fo="http://www.w3.org/1999/XSL/Format"
      version='1.0'>

  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>

  <xsl:param name="l10n.gentext.language" select="'en'"/>
  <xsl:param name="paper.type" select="'A4'"></xsl:param>
  <xsl:param name="draft.mode" select="no"/>

  <!-- These extensions are required for table printing and other stuff -->
  <xsl:param name="use.extensions">1</xsl:param>
  <xsl:param name="callouts.extension">1</xsl:param>
  <xsl:param name="linenumbering.extension">1</xsl:param>
  <xsl:param name="tablecolumns.extension">1</xsl:param>
  <xsl:param name="textinsert.extension">1</xsl:param>

  <xsl:param name="admon.graphics" select="1" />
  <xsl:param name="admon.graphics.extension">.png</xsl:param>
  <xsl:param name="admon.graphics.path">figs/</xsl:param>
  <xsl:param name="callout.graphics" select="1" />
  <xsl:param name="callout.graphics.extension">.png</xsl:param>
  <xsl:param name="callout.graphics.path">images/callouts/</xsl:param>

  <xsl:param name="section.autolabel" select="1" />
  <xsl:param name="section.label.includes.component.label">1</xsl:param>

  <xsl:param name="variablelist.as.blocks" select="1" />
  <xsl:param name="hyphenate">false</xsl:param>

  <!-- Font settings, we use characters out of base14 even for english -->
  <xsl:param name="title.font.family">sans-serif,Arial</xsl:param>
  <xsl:param name="body.font.family">serif,Times New Roman</xsl:param>
  <xsl:param name="sans.font.family">sans-serif,Arial</xsl:param>
  <xsl:param name="dingbat.font.family">serif,Times New Roman</xsl:param>
  <xsl:param name="monospace.font.family">monospace,Courier New</xsl:param>
  <xsl:param name="symbol.font.family">Symbol,ZapfDingbats</xsl:param>

  <!-- Page related settings -->
  <xsl:param name="page.margin.inner">1.5cm</xsl:param>
  <xsl:param name="page.margin.outer">1.5cm</xsl:param>
  <xsl:param name="title.margin.left">0pt</xsl:param>
  <xsl:param name="body.start.indent">24pt</xsl:param>
  <xsl:param name="body.end.indent">0pt</xsl:param>

  <!-- Prevent blank pages in output -->
  <xsl:template name="book.titlepage.before.verso">
  </xsl:template>
  <xsl:template name="book.titlepage.verso">
  </xsl:template>
  <xsl:template name="book.titlepage.separator">
  </xsl:template>

  <!-- titlepage settings -->
  <xsl:template name="book.titlepage">
    <fo:block>
        <fo:table table-layout="fixed" space-after.optimum="10pt" width="100%">
            <fo:table-body>
                <fo:table-row>
                    <fo:table-cell>
                        <fo:block text-align="center">
                          <!--fo:external-graphic src="url(figs/cover-logo.png)"
                              width="90%"  height="auto" content-width="scale-to-fit" content-height="scale-to-fit" /-->
                        </fo:block>
                    </fo:table-cell>
                </fo:table-row>
            </fo:table-body>
        </fo:table>
    </fo:block>

    <fo:block text-align="center" color="#000000" margin-left="1cm" margin-right="1cm"
        space-before.optimum="3cm" space-after.optimum="5.0cm"
        font-weight="900" font-size="32pt">
      <xsl:attribute name="font-family"><xsl:value-of select="$title.font.family" /></xsl:attribute>

      <xsl:value-of select="/book/title"/>
    </fo:block>

    <fo:block text-align="center" color="#000080" margin-left="1cm" margin-right="1cm"
        space-before.optimum="2cm" space-after.optimum="8.0cm"
        font-weight="900" font-size="16pt">
      <xsl:attribute name="font-family"><xsl:value-of select="$title.font.family" /></xsl:attribute>

      <xsl:value-of select="/book/subtitle"/>
    </fo:block>

    <fo:block text-align="center" color="#000000" margin-left="1cm" margin-right="1cm"
        space-before.optimum="8cm" space-after.optimum="10cm"
        font-weight="600" font-size="24pt">
      <xsl:attribute name="font-family"><xsl:value-of select="$title.font.family" /></xsl:attribute>

      <xsl:call-template name="person.name.list">
        <xsl:with-param name="person.list" select="bookinfo/authorgroup/author"/>
      </xsl:call-template>
    </fo:block>

    <!--fo:block text-align="end"    color="#666D70" margin-left="1cm" margin-right="1cm"
        font-family="sans-serif" font-weight="normal" font-size="10pt" >
      <xsl:value-of select="/book/subtitle"/>
    </fo:block-->
  </xsl:template>

  <!-- title settings -->
  <xsl:attribute-set name="preface.titlepage.recto.style">
    <xsl:attribute name="color">#7C1C51</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="chapter.titlepage.recto.style">
    <xsl:attribute name="color">#7C1C51</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="section.titlepage.recto.style">
    <xsl:attribute name="color">#7C1C51</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="appendix.titlepage.recto.style">
    <xsl:attribute name="color">#7C1C51</xsl:attribute>
  </xsl:attribute-set>

  <!-- Verbatim related settings -->
  <xsl:param name="hyphenate.verbatim">0</xsl:param>

  <xsl:attribute-set name="monospace.properties">
    <xsl:attribute name="font-family">
      <xsl:value-of select="$monospace.font.family"/>
    </xsl:attribute>
      <xsl:attribute name="color">#000080</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="monospace.verbatim.properties"
                     use-attribute-sets="verbatim.properties monospace.properties">
    <xsl:attribute name="border-color">blue</xsl:attribute>
    <xsl:attribute name="border-width">thin</xsl:attribute>
    <xsl:attribute name="border-style">solid</xsl:attribute>
    <xsl:attribute name="font-size">8pt</xsl:attribute>
    <xsl:attribute name="wrap-option">wrap</xsl:attribute>
    <xsl:attribute name="hyphenation-character">&#x002D;</xsl:attribute>
    <!--xsl:attribute name="hyphenation-character">&#x002D;</xsl:attribute-->
    <!--xsl:attribute name="hyphenation-character">&#x2011;</xsl:attribute-->
    <!--xsl:attribute name="hyphenation-character">&#x25BA;</xsl:attribute-->
    <!--xsl:attribute name="hyphenation-character">&#x27A4;</xsl:attribute-->
  </xsl:attribute-set>

  <!-- emphasis settings -->
  <xsl:template match="emphasis">
    <xsl:param name="content">
      <xsl:call-template name="simple.xlink">
        <xsl:with-param name="content">
          <xsl:apply-templates/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:param>

    <fo:inline color="#7C1C51" font-weight="bold">
      <xsl:copy-of select="$content"/>
    </fo:inline>
  </xsl:template>

  <!-- Colourize links in output -->
  <xsl:attribute-set name="xref.properties">
    <xsl:attribute name="color">
      <xsl:choose>
        <xsl:when test="self::ulink">blue</xsl:when>
        <xsl:when test="self::xref">blue</xsl:when>
        <xsl:when test="self::uri">blue</xsl:when>
        <xsl:otherwise>red</xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
  </xsl:attribute-set>

</xsl:stylesheet>
