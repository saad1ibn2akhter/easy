<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xdt="http://www.w3.org/2005/02/xpath-datatypes">

<xsl:template match="/">
<xsl:if test="count(//Registry[@Category = 'Error'] | //File[@Category = 'Error']) > 0">
<h1>Errors</h1>
	<xsl:apply-templates select="//Registry[@Category = 'Error']" />
	<xsl:apply-templates select="//File[@Category = 'Error']" />
<hr />
</xsl:if>
<h1>General Information</h1>
	<xsl:apply-templates select="//Message[@Category='GeneralInfo']"/>
<hr />
<h1>Migration Details</h1>
	<xsl:apply-templates select="//Section[@Name != 'Profile']">
		<xsl:sort select="@Name" />
	</xsl:apply-templates>
	<!--<xsl:apply-templates select="//Section[@Name = 'Profile']" />-->
	<p><b>General</b><xsl:text>: </xsl:text></p>
	<xsl:apply-templates select="//Message[@Category='DetailInfo']"/>
	<xsl:apply-templates select="//Registry[@Category='DetailInfo']"/>
<hr />
<h1>Summary</h1>
	<xsl:apply-templates select="//Message[@Category='SummaryInfo']"/>
</xsl:template>

<xsl:template match="//Registry[@Category = 'Error']">
	<p><xsl:value-of select="parent::*/@Name" /><xsl:text>: </xsl:text><xsl:copy-of select="* | text()" />
	</p>
</xsl:template>
<xsl:template match="//File[@Category = 'Error']">
	<p><xsl:value-of select="parent::*/@Name" /><xsl:text>: </xsl:text><xsl:copy-of select="* | text()" />
	</p>
</xsl:template>

<xsl:template match="Section[@Name != 'Profile']">
	<xsl:if test="child::*">
		<p><b><xsl:value-of select="@Name" /></b><xsl:text>: </xsl:text></p>
		<xsl:apply-templates />
		<hr />
	</xsl:if>
</xsl:template>

<xsl:template match="Section[@Name = 'Profile']">
	<xsl:if test="child::*">
		<p><b>General</b><xsl:text>: </xsl:text></p>
		<xsl:apply-templates />		
	</xsl:if>
</xsl:template>

<xsl:template match="File">
	<p><xsl:text> </xsl:text><xsl:copy-of select="* | text()" /></p>
</xsl:template>

<xsl:template match="Message">
	<p><xsl:text> </xsl:text><xsl:copy-of select="* | text()" /></p>
</xsl:template>

<xsl:template match="//Registry[@Category = 'DetailInfo']">
	<p><xsl:text> </xsl:text><xsl:copy-of select="* | text()" /></p>
</xsl:template>

<xsl:template match="//Message[@Category = 'DetailInfo']">
	<p><xsl:text> </xsl:text><xsl:copy-of select="* | text()" /></p>
</xsl:template>

<xsl:template match="//Message[@Category = 'SummaryInfo']">
	<p><xsl:text> </xsl:text><xsl:copy-of select="* | text()" /></p>
</xsl:template>
</xsl:stylesheet>
