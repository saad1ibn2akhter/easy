
<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/TR/REC-html40">
<xsl:output encoding="UTF-8" method="html"/>
<!-- 	This XSL is used to format the Live Update Landing page together with the WSCommCntrData.xml.
	It provides the entire landing page HTML template. If loading from server side, the XSL content will be
	generated and can be updated dynamically. The client (local) copy of this XSL also exists if choosing loading
	from local install. The local copy might be out of sync with the server side and needs to be synced 
	with the server side copy every release. Also, the local copy of this XSL file needs to be localized to
	different languages while if loading from server side, it is already in local langauges determined by the language code.
-->
  <xsl:template match="/">
        <table border="0" cellpadding="3" cellspacing="3" width="775">
            <tr>
                <td><img src="ext_adsklogo.gif"/></td>
            </tr>
            <tr>
                <td valign="top">
                    <table border="0" cellpadding="0" cellspacing="0" width="775">
                        <tr>
                            <td valign="top">
                                <table border="0" cellpadding="0" cellspacing="0" width="256">
                                    <tr>
                                        <td bgcolor="#cccccc"><img src="ext_clear.gif" width="10" height="1"/></td>
                                    </tr>
                                </table>
                            </td>
                            <td bgcolor="#ffffff">
                                <img src="ext_clear.gif" width="3" height="1"/>
                            </td>
                            <td valign="top">
                                <table border="0" cellpadding="0" cellspacing="0" width="516" ID="Table1">
                                    <tr>
                                        <td bgcolor="#cccccc"><img src="ext_clear.gif" width="10" height="1"/></td>
                                    </tr>
                                </table>
                            </td>
                        </tr>
                    </table>
                </td>
            </tr>
            <tr>
                <td>
                    <table border="0" cellpadding="0" cellspacing="0" width="775">
                        <tr>
                            <td>
                                <table border="0" cellpadding="0" cellspacing="0" width="256" ID="Table2">
                                    <tr>
                                        <td>&#160;</td>
                                    </tr>
                                </table>
                            </td>
                            <td bgcolor="#ffffff">
                                <img src="ext_clear.gif" width="3" height="1"/>
                            </td>
                            <td>
                                <table border="0" cellpadding="0" cellspacing="0" width="516" ID="Table3">
                                    <tr>
                                        <td>
							<table>
                                    	<tr>
                                        		<td align="left" class="pageSubHeader">Autodesk Live Update<br/><br/></td>
                                        		<td></td>
                                    	</tr>
                                    	<tr>
                                        		<td class="pageBodyText">In an effort to continuously improve the quality and stability of its products, Autodesk has released Service Packs that address a number of customer requests. We believe that these updates will improve your experience with our products. Please read the information associated with each release to take full advantage of the Service Packs.<br/><br/></td>
                                        		<td></td>
                                    	</tr>
                                    	<tr>
                                        		<td class="pageBodyText">The use of this software is subject to the terms and conditions contained in the software license agreement that accompanied this product.&#160;YOUR USE OF THE SOFTWARE INDICATES YOUR ASSENT TO BE BOUND BY THE TERMS AND CONDITIONS OF SUCH LICENSE AGREEMENT. COPYING OR USE OF THIS SOFTWARE OR ITS DOCUMENTATION EXCEPT AS PERMITTED BY THE SOFTWARE LICENSE AGREEMENT IS UNAUTHORIZED AND IS COPYRIGHT INFRINGEMENT UNDER THE LAWS OF YOUR COUNTRY. IF YOU COPY OR USE THIS SOFTWARE OR ITS DOCUMENTATION WITHOUT PERMISSION OF AUTODESK, YOU ARE VIOLATING THE LAW. YOU MAY BE LIABLE TO AUTODESK FOR DAMAGES, AND YOU MAY BE SUBJECT TO CRIMINAL PENALTIES.<br/><br/></td>
                                        		<td></td>
                                    	</tr>
                                    	<tr>
                                        		<td align="left" class="pageSubHeader">Instructions for updating your product:<br/><br/></td>
                                        		<td></td>
                                    	</tr>
                                    	<tr>
                                        		<td class="pageBodyText">
                                            		<ol>
                                                		<li>Insert the original product CD or make sure that the installation files are available to you computer.</li>
                                                		<li>Choose an update button to download the update.</li>
					              		</ol>
                                            		<p/>When the download is complete, you will be prompted to manually close your application to finish the update. At this point, it is strongly recommended that you exit all Windows programs (including any virus scanning software).
                                            		<p/>The following free disk space is required during the installation of the update:
                                            		<p/>
                                                	<ul>
                                                		<li>5 times the size of the Service Pack on the drive that contains the system TEMP folder. (For example, if the download is 5 MB, you should have 25 MB of free disk space.)</li>
                                                		<li>200 MB on the drive on which your product is installed.</li>
                                                	</ul>
                                        		</td>
                                    	</tr>
							</table>
                                        </td>
                                    </tr>
                                    <!-- Service pack to download: generated by parsing thru the WSCommCntrdata.xml file... -->
      					<xsl:apply-templates select="COMMUNICATIONCENTERCONTENT"/>
                                </table>
                            </td>
                        </tr>
                    </table>
                </td>
            </tr>
        </table>
  </xsl:template>

  <xsl:template match="COMMUNICATIONCENTERCONTENT">
      <xsl:apply-templates select="CHANNEL[@TYPE = '1']"/>
  </xsl:template>

  <xsl:template match="CHANNEL[@TYPE = '1']">
	<xsl:apply-templates select="MAINTENANCE_PATCH"/>
  </xsl:template>

  <xsl:template match="MAINTENANCE_PATCH">
      <xsl:apply-templates select="STATUSSTRING"/>
  </xsl:template>

  <xsl:template match="STATUSSTRING">
      <xsl:apply-templates select="AVAILABLEPATCHES"/>
  </xsl:template>

  <xsl:template match="AVAILABLEPATCHES">
    <table border="0" cellpadding="0" cellspacing="0" width="516">
      <xsl:apply-templates select="PATCH"/>
    </table>
  </xsl:template>

  <!-- The term "Description:" and "Download size:" need to be localized -->

  <xsl:template match="PATCH">
	<tr><td>
		<table border="0" cellpadding="0" cellspacing="0" width="516">
			<tr>
				<td colspan="2" align="left" class="pageSubHeader"><xsl:value-of select="@PATCHTITLE" disable-output-escaping="yes"/><br/><br/></td>
			</tr>
			<tr>
				<td class="pageBodyText" width="100" valign="top"><input class="updateButton" type="button" name="SPK_i" value="Update"><xsl:attribute name="onClick">javascript:CallerDoApplyPatch('<xsl:value-of select="@PRODUCTCODE" disable-output-escaping="yes" />','<xsl:value-of select="@PATCHFILELOCATION" disable-output-escaping="yes"/>')</xsl:attribute></input></td>			
				<td class="pageBodyText" width="416">Description:&#160;<xsl:value-of select="@PATCHDESC" disable-output-escaping="yes" /><xsl:if test="@PATCHDESCLOCATION!=''"><br/><a class="pageTextLinkRegular" target="_blank"><xsl:attribute name="href"><xsl:value-of select="@PATCHDESCLOCATION" disable-output-escaping="yes" /></xsl:attribute>Readme...</a></xsl:if><br/>Download Size:&#160;<xsl:value-of select="@PATCHSIZE" disable-output-escaping="yes" />&#160;KB</td>
			</tr>
		</table>
	</td></tr>
	<tr><td colspan="2"><br/></td></tr>
  </xsl:template>
 
</xsl:stylesheet>