<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/TR/REC-html40">
<xsl:output encoding="UTF-8" method="html"/>


  <xsl:template match="/">
      <xsl:apply-templates select="COMMUNICATIONCENTERCONTENT"/>
  </xsl:template>

  <xsl:template match="COMMUNICATIONCENTERCONTENT">
      <xsl:apply-templates select="CHANNEL[@TYPE='2']"/>
  </xsl:template>

  <xsl:template match="CHANNEL[@TYPE='2']">
    <table border="0" cellpadding="0" cellspacing="0" width="516">
      <xsl:apply-templates select="EXTENSION[@STATUSCODE='1']"/>
    </table>
  </xsl:template>

  <xsl:template match="EXTENSION[@STATUSCODE='1']">
	<tr><td>
		<table border="0" cellpadding="0" cellspacing="0" width="516">
		<tr>
		    <td class="pageBodyText" width="400"><a class="pageTextLinkRegular"><xsl:attribute name="href"><xsl:value-of select="@TARGETURL"/></xsl:attribute><xsl:value-of select="@NAME"/></a></td>
		    <td class="pageBodyText" width="140"><div id="launchDate"><xsl:value-of select="@LAUNCHDATE"/></div></td>
		</tr>
		</table>
	</td></tr>
  </xsl:template>


</xsl:stylesheet>