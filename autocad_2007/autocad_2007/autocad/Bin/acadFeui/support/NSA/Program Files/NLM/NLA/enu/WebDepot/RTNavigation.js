document.writeln('<br>');

//	if this is one of the error pages, there won't be any navigation information.
noNavigationPages = new Array (xmsgRTAuthEntryError, xmsgRTContactAutodesk, xmsgRT010203071213CannotCompErrA, 
								xmsgRTPurchaseError, xmsgRT09AlreadyReg, xmsgRT10ExceededAuths, 
								xmsgRT0015TempSysErr, xmsgRT18OldSNInvalid, xmsgRTSUNOldSNInvalid,
								xmsgRT20DataError, xmsgRT21CustIDTelErr, xmsgRT22NetworkInstallation,
								xmsgRT900SystemNotResponding, xmsgRTAuthEntryError, xmsgLicErrorHdr,
								xmsgRTLicFileSaveError, xmsgRTAuthError, xmsgRTPurchAuthFailed,
								xmsgRTPurchConnectionFailed, xmsgRTConnect, xmsgRT12StolenSN);
for (i = 0; i < noNavigationPages.length; i++) {
	if (document.title == noNavigationPages[i]) {
		navigationType = "0";
		break;
	}
}

switch (navigationType) {
	case "1" :	//	Slim package
//		SlimNavigation();
	case "2" :	//	Full package
		FullNavigation();
		break;
	case "3" :	//	Enter Authcode
		EnterAuthCodeNavigation();
		break;
	case "4" :	//	NLA Slim package
	case "5" :	//	NLA Full package
	case "6" :	//	NLA Disable Internet 
		NLANavigation();
		break;
	case "7" :	//	Buy online
		BuyNavigation();
		break;
	case "8" :	//	Volunteer registration
		VolNavigation();
		break;
	case "9" :	//	Multi-Seat Single Master Install
		MSSMNavigation();
		break;
	case "10" :	//	Multi-Seat Client install
		MSSCNavigation();
		break;
	case "11" :	//	Beta-Mode
		BetaNavigation();
		break;
}

//	End of the Navigation bar 
document.writeln('</td>');
document.writeln('<td>');
document.writeln('<table>');
document.writeln('<tr>');

//	Starting of the real page
document.writeln('<td>');

//	Starting of the "Before", "Next", and "Cancel" button section
function StartNavigationButtons() {
	document.writeln('<table width="600" border="0" cellpadding="0" cellspacing="0" ID="Table1">');
	document.writeln('<tr>');
	document.writeln('<td valign="top" align="center">');
}

//	End of the "Before", "Next", and "Cancel" button section
function EndNavigationButtons() {
	document.writeln('</td>');
	document.writeln('</tr>');
	document.writeln('</table>');
}

function SlimNavigation() {
	bar = new Array ( xmsgRTBeginReg, xmsgRTConfirm, xmsgRTAuthConfirmed);
	contents = new Array ( xmsgRegAuthInfo, xmsgConfirmInfo, xmsgRegAuthInfo1);
	
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function FullNavigation() {
	bar = new Array ( xmsgRTBeginReg, xmsgRTCountryPage, xmsgRTUserInfo, xmsgRTConfirm, xmsgRTAuthConfirmed);
	contents = new Array ( xmsgRegAuthInfo, xmsgCountryHdr, xmsgRegInfo, xmsgConfirmInfo, xmsgRegAuthInfo1);
	
	//	Change User Info to Customer ID if it's necessary
	if (document.title == xmsgRTCustID ||
		document.title == xmsgRTCustIDConfirm) {
		bar[2] = xmsgRTCustID;
		contents[2] = xmsgCustIDMainTitle;
		bar[3] = xmsgRTCustIDConfirm;
		contents[3] = xmsgRTCustIDConfirm;
	} 

	otherMethodItems = new Array (xmsgRTEmail, xmsgRTFaxUS, xmsgRTMailUS);
	otherMethodContents = new Array (xmsgRegmethod9, xmsgRegmethod7, xmsgRegmethod5);
	//	switch "Confirmation" to "Email", "Fax", or "Mail"
	var confirmIndex = bar.length - 1;
	for (i = 0; i < 3; i++) {
		if (document.title == otherMethodItems[i]) {
			bar[confirmIndex] = otherMethodItems[i];
			contents[confirmIndex] = otherMethodContents[i];
			break;
		}
	}
		
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function EnterAuthCodeNavigation() {
	bar = new Array ( xmsgRTBeginReg, xmsgRTAuthNow, xmsgRTAuthConfirmed);
	contents = new Array ( xmsgRegAuthInfo, xmsgRegAuthInfo3, xmsgRegAuthInfo1);
	
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function BetaNavigation() {
	bar = new Array ( xmsgRTBeginReg, xmsgRTAuthNow, xmsgRTConfirm);
	contents = new Array ( xmsgRegAuthInfo, xmsgRegAuthInfo3, xmsgConfirmInfo);
	
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function MSSMNavigation() {
	bar = new Array ( xmsgRTSUNServer, xmsgRTCountryPage, xmsgRTUserInfo, xmsgRTSUNDataConfirm);
	contents = new Array ( xmsgRegSunData5, xmsgCountryHdr, xmsgRegInfo, xmsgRegSunData1);
	
	//	Change User Info to Customer ID if it's necessary
	if (document.title == xmsgRTCustID ||
		document.title == xmsgRTCustIDConfirm) {
		bar[2] = xmsgRTCustID;
		contents[2] = xmsgCustIDMainTitle;
		bar[3] = xmsgRTCustIDConfirm;
		contents[3] = xmsgRTCustIDConfirm;
	} 

	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function MSSCNavigation() {
	bar = new Array ( xmsgRTBeginReg, xmsgRTSUNSubmit, xmsgRTAuthConfirmed);
	contents = new Array ( xmsgRegAuthInfo, xmsgInformation, xmsgRegAuthInfo1);
	
	otherMethodItems = new Array (xmsgRTEmail, xmsgRTFaxUS, xmsgRTMailUS);
	otherMethodContents = new Array (xmsgRegmethod9, xmsgRegmethod7, xmsgRegmethod5);
	//	switch "Confirmation" to "Email", "Fax", or "Mail"
	var confirmIndex = bar.length - 1;
	for (i = 0; i < 3; i++) {
		if (document.title == otherMethodItems[i]) {
			bar[confirmIndex] = otherMethodItems[i];
			contents[confirmIndex] = otherMethodContents[i];
			break;
		}
	}
	
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function BuyNavigation() {
	bar = new Array ( xmsgRTBuyInformation, xmsgRTCountryPage, xmsgRTUserInfo, xmsgRTConfirm, xmsgRTPurchConfirmOther);
	contents = new Array ( xmsgPuchaseInfo1, xmsgCountryHdr, xmsgRegInfo, xmsgConfirmInfo, " ");
	
	//	Change User Info to Customer ID if it's necessary
	if (document.title == xmsgRTCustID ||
		document.title == xmsgRTCustIDConfirm) {
		bar[2] = xmsgRTCustID;
		contents[2] = xmsgCustIDMainTitle;
		bar[3] = xmsgRTCustIDConfirm;
		contents[3] = xmsgRTCustIDConfirm;
	} 

	otherMethodItems = new Array (xmsgRTPurchaseOffline, xmsgRTEmail, xmsgRTFaxUS, xmsgRTMailUS);
	otherMethodContents = new Array (xmsgFormPurchHdr, xmsgRegmethod9, xmsgRegmethod7, xmsgRegmethod5);
	//	switch "Confirmation" to "Email", "Fax", or "Mail"
	var confirmIndex = bar.length - 1;
	for (i = 0; i < 3; i++) {
		if (document.title == otherMethodItems[i]) {
			bar[confirmIndex] = otherMethodItems[i];
			contents[confirmIndex] = otherMethodContents[i];
			break;
		}
	}
	
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function VolNavigation() {
	bar = new Array ( xmsgRTRegVol, xmsgRTCountryPage, xmsgRTUserInfo, xmsgRTConfirm, xmsgRTRegConfirmed);
	contents = new Array ( xmsgRegVol1, xmsgCountryHdr, xmsgRegInfo, xmsgConfirmInfo, xmsgRegAuthInfo1);

	//	Change User Info to Customer ID if it's necessary
	if (document.title == xmsgRTCustID ||
		document.title == xmsgRTCustIDConfirm) {
		bar[2] = xmsgRTCustID;
		contents[2] = xmsgCustIDMainTitle;
		bar[3] = xmsgRTCustIDConfirm;
		contents[3] = xmsgRTCustIDConfirm;
	} 

	otherMethodItems = new Array (xmsgRTEmail, xmsgRTFaxUS, xmsgRTMailUS);
	otherMethodContents = new Array (xmsgRegmethod9, xmsgRegmethod7, xmsgRegmethod5);
	//	switch "Confirmation" to "Email", "Fax", or "Mail"
	var confirmIndex = bar.length - 1;
	for (i = 0; i < 3; i++) {
		if (document.title == otherMethodItems[i]) {
			bar[confirmIndex] = otherMethodItems[i];
			contents[confirmIndex] = otherMethodContents[i];
			break;
		}
	}
		
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}

function NLANavigation() {
	bar = new Array ( xmsgNSARTBegin, xmsgNSARTLocateLicense, xmsgNSARTConfirmServerAndSeat, xmsgRTCountryPage, xmsgRTUserInfo, xmsgRTConfirm, xmsgSaveLicenseHdr, xmsgRTAuthConfirmed);
	contents = new Array ( xmsgNSARTBeginTitle, xmsgEditServerHdrMsg1, xmsgNSARTConfirmServerAndSeat, xmsgNSACountryHdr, xmsgRegInfo, xmsgConfirmInfo, xmsgSaveLicenseHdr, xmsgLicAuthConfirmHdr1);

	//	Change User Info to Customer ID if it's necessary
	if (document.title == xmsgRTCustID ||
		document.title == xmsgRTCustIDConfirm) {
		bar[4] = xmsgRTCustID;
		contents[4] = xmsgCustIDMainTitle;
		bar[5] = xmsgRTCustIDConfirm;
		contents[5] = xmsgRTCustIDConfirm;
	} 

	otherMethodItems = new Array (xmsgRTEmail, xmsgRTFaxUS, xmsgRTMailUS);
	otherMethodContents = new Array (xmsgRegmethod9, xmsgRegmethod7, xmsgRegmethod5);
	
	//	switch "Confirmation" to "Email", "Fax", or "Mail" 
	//	and remove "license recive" too.
	var confirmIndex = bar.length - 2;
	for (i = 0; i < 3; i++) {
		if (document.title == otherMethodItems[i]) {
			bar[confirmIndex] = otherMethodItems[i];
			contents[confirmIndex] = otherMethodContents[i];
			contents[confirmIndex+1] = " ";
			break;
		}
	}
		
	for (i=0; i < bar.length; i++) {
		if (bar[i] == document.title) {
			document.writeln(">"+contents[i]+"<br>");
		} else {
			document.writeln("<font color=\""+ xmsgNavigationDisableColor +"\">"+contents[i]+"</font><br>");
		}
	}
}








// SIG // Begin signature block
// SIG // MIIWVgYJKoZIhvcNAQcCoIIWRzCCFkMCAQExDjAMBggq
// SIG // hkiG9w0CBQUAMGYGCisGAQQBgjcCAQSgWDBWMDIGCisG
// SIG // AQQBgjcCAR4wJAIBAQQQEODJBs441BGiowAQS9NQkAIB
// SIG // AAIBAAIBAAIBAAIBADAgMAwGCCqGSIb3DQIFBQAEEO12
// SIG // KtjOk5PitGoSL4vlQ8qgghGXMIIDxDCCAy2gAwIBAgIQ
// SIG // R78Zld+NUkZD99ttSA0xpDANBgkqhkiG9w0BAQUFADCB
// SIG // izELMAkGA1UEBhMCWkExFTATBgNVBAgTDFdlc3Rlcm4g
// SIG // Q2FwZTEUMBIGA1UEBxMLRHVyYmFudmlsbGUxDzANBgNV
// SIG // BAoTBlRoYXd0ZTEdMBsGA1UECxMUVGhhd3RlIENlcnRp
// SIG // ZmljYXRpb24xHzAdBgNVBAMTFlRoYXd0ZSBUaW1lc3Rh
// SIG // bXBpbmcgQ0EwHhcNMDMxMjA0MDAwMDAwWhcNMTMxMjAz
// SIG // MjM1OTU5WjBTMQswCQYDVQQGEwJVUzEXMBUGA1UEChMO
// SIG // VmVyaVNpZ24sIEluYy4xKzApBgNVBAMTIlZlcmlTaWdu
// SIG // IFRpbWUgU3RhbXBpbmcgU2VydmljZXMgQ0EwggEiMA0G
// SIG // CSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCpyrKkzM0g
// SIG // rwp9iayHdfC0TvHfwQ+/Z2G9o2Qc2rv5yjOrhDCJWH6M
// SIG // 22vdNp4Pv9HsePJ3pn5vPL+Trw26aPRslMq9Ui2rSD31
// SIG // ttVdXxsCn/ovax6k96OaphrIAuF/TFLjDmDsQBx+uQ3e
// SIG // P8e034e9X3pqMS4DmYETqEcgzjFzDVctzXg0M5USmRK5
// SIG // 3mgvqubjwoqMKsOLIYdmvYNYV291vzyqJoddyhAVPJ+E
// SIG // 6lTBCm7E/sVK3bkHEZcifNs+J9EeeOyfMcnx5iIZ28Sz
// SIG // R0OaGl+gHpDkXvXufPF9q2IBj/VNC97QIlaolc2uiHau
// SIG // 7roN8+RN2aD7aKCuFDuzh8G7AgMBAAGjgdswgdgwNAYI
// SIG // KwYBBQUHAQEEKDAmMCQGCCsGAQUFBzABhhhodHRwOi8v
// SIG // b2NzcC52ZXJpc2lnbi5jb20wEgYDVR0TAQH/BAgwBgEB
// SIG // /wIBADBBBgNVHR8EOjA4MDagNKAyhjBodHRwOi8vY3Js
// SIG // LnZlcmlzaWduLmNvbS9UaGF3dGVUaW1lc3RhbXBpbmdD
// SIG // QS5jcmwwEwYDVR0lBAwwCgYIKwYBBQUHAwgwDgYDVR0P
// SIG // AQH/BAQDAgEGMCQGA1UdEQQdMBukGTAXMRUwEwYDVQQD
// SIG // EwxUU0EyMDQ4LTEtNTMwDQYJKoZIhvcNAQEFBQADgYEA
// SIG // Smv56ljCRBwxiXmZK5a/gqwB1hxMzbCKWG7fCCmjXsjK
// SIG // kxPnBFIN70cnLwA4sOTJk06a1CJiFfc/NyFPcDGA8Ys4
// SIG // h7Po6JcA/s9Vlk4k0qknTnqut2FB8yrO58nZXt27K4U+
// SIG // tZ212eFX/760xX71zwye8Jf+K9M7UhsbOCf3P0owggP/
// SIG // MIIC56ADAgECAhAN6Svw1NgpiBgyBQlemnaIMA0GCSqG
// SIG // SIb3DQEBBQUAMFMxCzAJBgNVBAYTAlVTMRcwFQYDVQQK
// SIG // Ew5WZXJpU2lnbiwgSW5jLjErMCkGA1UEAxMiVmVyaVNp
// SIG // Z24gVGltZSBTdGFtcGluZyBTZXJ2aWNlcyBDQTAeFw0w
// SIG // MzEyMDQwMDAwMDBaFw0wODEyMDMyMzU5NTlaMFcxCzAJ
// SIG // BgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5j
// SIG // LjEvMC0GA1UEAxMmVmVyaVNpZ24gVGltZSBTdGFtcGlu
// SIG // ZyBTZXJ2aWNlcyBTaWduZXIwggEiMA0GCSqGSIb3DQEB
// SIG // AQUAA4IBDwAwggEKAoIBAQCyUChI3dNoeoQYRGZ1XX7E
// SIG // uJ9jJv89Q5x8ETgQJVVz2XUnaf1OuSBc0wr5oBsq7VVW
// SIG // IWHYHtvkvDNrx+/dozdljhuTDLZTHlx8ZjVfBYpF/nZO
// SIG // 31OAooEgna6IXKII9+Uw+e4iN0xCCs7fxh/E1lXpgT+1
// SIG // UqMsqgF68qKqjTX+n+ZdagWfPWvjv5bA/sxg+UDnB6BE
// SIG // 64FRbqUq8raKECjtj9wGoIZQmntKCA0wHcoQnmv36Viu
// SIG // BKlAmbIo6I8WrDzjU29L0zWdtW9kHbOWLLs953nrbXr5
// SIG // FuYmra/vmVO3QCyVuHmq/tRSqyl0fkLsOR6iahbmWbsk
// SIG // aNgAgEMQh4BrAgMBAAGjgcowgccwNAYIKwYBBQUHAQEE
// SIG // KDAmMCQGCCsGAQUFBzABhhhodHRwOi8vb2NzcC52ZXJp
// SIG // c2lnbi5jb20wDAYDVR0TAQH/BAIwADAzBgNVHR8ELDAq
// SIG // MCigJqAkhiJodHRwOi8vY3JsLnZlcmlzaWduLmNvbS90
// SIG // c3MtY2EuY3JsMBYGA1UdJQEB/wQMMAoGCCsGAQUFBwMI
// SIG // MA4GA1UdDwEB/wQEAwIGwDAkBgNVHREEHTAbpBkwFzEV
// SIG // MBMGA1UEAxMMVFNBMjA0OC0xLTU0MA0GCSqGSIb3DQEB
// SIG // BQUAA4IBAQCHeHDaTlIBIFvgecmCMMT9uRmWvZEAw73N
// SIG // zcb0Dtj/+U3AM2IwEcX1dBvUkt5fnCATsXxFvlDNg+eA
// SIG // F4OnJ5NnE0b7yriYQQPMm1FbBYt/qG/zG1AbJC7yaY1s
// SIG // Ive7yhaV7Qx0wGh32euZYofBc5D4iXR6I6ujmHuXsfeP
// SIG // KXFNLnUbSEHa8LUNIFTWd6CXgmNp/QnPivB1uwmb2fkR
// SIG // VSaaYTK+egKwe4a+osOLIix40TV2vJJzXPm55kwVCiPM
// SIG // 5NLUNC5JQBU8D2B6JMalZu+Wz3DrPuf0DX7c0XyjdnFp
// SIG // wZxPRzA1IbGirxpiPCvZjqoqB3vYGLNce+KdpW/+PImt
// SIG // MIIEvzCCBCigAwIBAgIQV2RuK1UAI9SQU0pVPqsNCjAN
// SIG // BgkqhkiG9w0BAQUFADBfMQswCQYDVQQGEwJVUzEXMBUG
// SIG // A1UEChMOVmVyaVNpZ24sIEluYy4xNzA1BgNVBAsTLkNs
// SIG // YXNzIDMgUHVibGljIFByaW1hcnkgQ2VydGlmaWNhdGlv
// SIG // biBBdXRob3JpdHkwHhcNMDQwNzE2MDAwMDAwWhcNMDkw
// SIG // NzE1MjM1OTU5WjCBtDELMAkGA1UEBhMCVVMxFzAVBgNV
// SIG // BAoTDlZlcmlTaWduLCBJbmMuMR8wHQYDVQQLExZWZXJp
// SIG // U2lnbiBUcnVzdCBOZXR3b3JrMTswOQYDVQQLEzJUZXJt
// SIG // cyBvZiB1c2UgYXQgaHR0cHM6Ly93d3cudmVyaXNpZ24u
// SIG // Y29tL3JwYSAoYykwNDEuMCwGA1UEAxMlVmVyaVNpZ24g
// SIG // Q2xhc3MgMyBDb2RlIFNpZ25pbmcgMjAwNCBDQTCCASIw
// SIG // DQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL687rx+
// SIG // 74Pr4DdP+wMQOL4I0ox9nfqSfxkMwmvuQlKM3tMcSBMl
// SIG // 6sFjevlRZe7Tqjv18JScK/vyZtQk2vf1n24ZOTa80KN2
// SIG // CB4iJyRsOJEn4oRJrhuKof0lgiwQMOhxqyjod0pR8ezN
// SIG // +PBU1G/A420Kj9nYZI1jsi1OJ/aFDv5t4ymZ4oVHfC2G
// SIG // f+hXj61nwjMykRMg/KkjFJptwoRLdmgE1XEsXSH6iA0m
// SIG // /R8tkSvnAVVN8m01KILf2WtcttbZqoH9X82DumOd0CL8
// SIG // qTtCabKOOrW8tJ4PXsTqLIKLKP1TCJbdtQEg0fmlGOfA
// SIG // 7lFwN+G2BUhSSG846sPobHtEhLsCAwEAAaOCAaAwggGc
// SIG // MBIGA1UdEwEB/wQIMAYBAf8CAQAwRAYDVR0gBD0wOzA5
// SIG // BgtghkgBhvhFAQcXAzAqMCgGCCsGAQUFBwIBFhxodHRw
// SIG // czovL3d3dy52ZXJpc2lnbi5jb20vcnBhMDEGA1UdHwQq
// SIG // MCgwJqAkoCKGIGh0dHA6Ly9jcmwudmVyaXNpZ24uY29t
// SIG // L3BjYTMuY3JsMB0GA1UdJQQWMBQGCCsGAQUFBwMCBggr
// SIG // BgEFBQcDAzAOBgNVHQ8BAf8EBAMCAQYwEQYJYIZIAYb4
// SIG // QgEBBAQDAgABMCkGA1UdEQQiMCCkHjAcMRowGAYDVQQD
// SIG // ExFDbGFzczNDQTIwNDgtMS00MzAdBgNVHQ4EFgQUCPVR
// SIG // 6Pv+PT1kNnxoz1t4qN+5xTcwgYAGA1UdIwR5MHehY6Rh
// SIG // MF8xCzAJBgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2ln
// SIG // biwgSW5jLjE3MDUGA1UECxMuQ2xhc3MgMyBQdWJsaWMg
// SIG // UHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eYIQ
// SIG // cLrkHRDZKTS2OMp7A8y6vzANBgkqhkiG9w0BAQUFAAOB
// SIG // gQCaZfXY1+Gk0F3e2H17w+7ECMJW0Izc7awijedQBg0H
// SIG // LKCkaZXMmd/MYzHPsMHklss4ziH7fOdYCiMhByyQl6vY
// SIG // lgSTVFO6OhBIcg2F7BsKQSXMfWysewPx93g88qhA0FVy
// SIG // 274LKLXIxwX+0+C1Idy8QLe+vGD1uOPYXjtl3WZWXzCC
// SIG // BQUwggPtoAMCAQICEAOMRKqgd1dBnLORB5kqOnMwDQYJ
// SIG // KoZIhvcNAQEFBQAwgbQxCzAJBgNVBAYTAlVTMRcwFQYD
// SIG // VQQKEw5WZXJpU2lnbiwgSW5jLjEfMB0GA1UECxMWVmVy
// SIG // aVNpZ24gVHJ1c3QgTmV0d29yazE7MDkGA1UECxMyVGVy
// SIG // bXMgb2YgdXNlIGF0IGh0dHBzOi8vd3d3LnZlcmlzaWdu
// SIG // LmNvbS9ycGEgKGMpMDQxLjAsBgNVBAMTJVZlcmlTaWdu
// SIG // IENsYXNzIDMgQ29kZSBTaWduaW5nIDIwMDQgQ0EwHhcN
// SIG // MDUwOTIxMDAwMDAwWhcNMDYwOTIxMjM1OTU5WjCByDEL
// SIG // MAkGA1UEBhMCVVMxEzARBgNVBAgTCkNhbGlmb3JuaWEx
// SIG // EzARBgNVBAcTClNhbiBSYWZhZWwxFjAUBgNVBAoUDUF1
// SIG // dG9kZXNrLCBJbmMxPjA8BgNVBAsTNURpZ2l0YWwgSUQg
// SIG // Q2xhc3MgMyAtIE1pY3Jvc29mdCBTb2Z0d2FyZSBWYWxp
// SIG // ZGF0aW9uIHYyMR8wHQYDVQQLFBZEZXNpZ24gU29sdXRp
// SIG // b25zIEdyb3VwMRYwFAYDVQQDFA1BdXRvZGVzaywgSW5j
// SIG // MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDI0TrL
// SIG // 4AhG8WET7KW1/Gs6jEcGtDydIrpDTNUL8qH6o6FbEoY1
// SIG // QImhZGKQJw2k/l2HhM+sGqUq4RiwMIEx4z0E9vfdPmmH
// SIG // zx3RxKiRQfEwdl1U7AfqAa3M8TvmAM+J45euBSkPFV2K
// SIG // Gr0rrOJPOkDA/InCg/F2CYCCjhMAuMhEhwIDAQABo4IB
// SIG // fzCCAXswCQYDVR0TBAIwADAOBgNVHQ8BAf8EBAMCB4Aw
// SIG // QAYDVR0fBDkwNzA1oDOgMYYvaHR0cDovL0NTQzMtMjAw
// SIG // NC1jcmwudmVyaXNpZ24uY29tL0NTQzMtMjAwNC5jcmww
// SIG // RAYDVR0gBD0wOzA5BgtghkgBhvhFAQcXAzAqMCgGCCsG
// SIG // AQUFBwIBFhxodHRwczovL3d3dy52ZXJpc2lnbi5jb20v
// SIG // cnBhMBMGA1UdJQQMMAoGCCsGAQUFBwMDMHUGCCsGAQUF
// SIG // BwEBBGkwZzAkBggrBgEFBQcwAYYYaHR0cDovL29jc3Au
// SIG // dmVyaXNpZ24uY29tMD8GCCsGAQUFBzAChjNodHRwOi8v
// SIG // Q1NDMy0yMDA0LWFpYS52ZXJpc2lnbi5jb20vQ1NDMy0y
// SIG // MDA0LWFpYS5jZXIwHwYDVR0jBBgwFoAUCPVR6Pv+PT1k
// SIG // Nnxoz1t4qN+5xTcwEQYJYIZIAYb4QgEBBAQDAgQQMBYG
// SIG // CisGAQQBgjcCARsECDAGAQEAAQH/MA0GCSqGSIb3DQEB
// SIG // BQUAA4IBAQBiZMWBvUi2jTxsv1c2YQPm/xZPJWY9Md8D
// SIG // K0LvR9Z1+ox/d40QLAFgnWEyBf5VoqDgPXtClP6kNjac
// SIG // cJMVN8g9MQUoU9AoUmnEJnmX9fZsofGq+LlfZjHvV02/
// SIG // A4wkEKcjY0tgknQwFXwGpwNpiEGVSfSlUNPkNGlOV2M9
// SIG // 6/XUXV08ByhSq8k0FPp1OpsMhZkArxyLFfWMOpLHWTEn
// SIG // WHo0DLqRe2ya6yM8jpDQs4of+YulbXwXIwCXLeS2ZKS4
// SIG // PpNTudybsVbpDZCsYpfTImiGwC5LYJLhyTV7WkEF31b2
// SIG // 2nzHMYp8e0M25QGC+jhjPWutEYZ32LOIQNRUa6apN5MU
// SIG // MYIEKTCCBCUCAQEwgckwgbQxCzAJBgNVBAYTAlVTMRcw
// SIG // FQYDVQQKEw5WZXJpU2lnbiwgSW5jLjEfMB0GA1UECxMW
// SIG // VmVyaVNpZ24gVHJ1c3QgTmV0d29yazE7MDkGA1UECxMy
// SIG // VGVybXMgb2YgdXNlIGF0IGh0dHBzOi8vd3d3LnZlcmlz
// SIG // aWduLmNvbS9ycGEgKGMpMDQxLjAsBgNVBAMTJVZlcmlT
// SIG // aWduIENsYXNzIDMgQ29kZSBTaWduaW5nIDIwMDQgQ0EC
// SIG // EAOMRKqgd1dBnLORB5kqOnMwDAYIKoZIhvcNAgUFAKCB
// SIG // sDAZBgkqhkiG9w0BCQMxDAYKKwYBBAGCNwIBBDAcBgor
// SIG // BgEEAYI3AgELMQ4wDAYKKwYBBAGCNwIBFTAfBgkqhkiG
// SIG // 9w0BCQQxEgQQs8Jpj+6kmD25Hw1O97OIhDBUBgorBgEE
// SIG // AYI3AgEMMUYwRKAmgCQAQQB1AHQAbwBkAGUAcwBrACAA
// SIG // QwBvAG0AcABvAG4AZQBuAHShGoAYaHR0cDovL3d3dy5h
// SIG // dXRvZGVzay5jb20gMA0GCSqGSIb3DQEBAQUABIGARrDd
// SIG // qzrqXKj9y6Fgs44JJImNwTyCImLAS7M+qZBj5mOlN7a5
// SIG // 7pcgnennZRvd1bpP/eS4lmSgXCF8Q6S2o6hVuxz4T5ZN
// SIG // HDgvfajtiM4jZDwxRVeCKhR41/2h36K7toZr7QUnvML1
// SIG // ZBcCltI7oWCqdXi1xsK+x2lJx5cdD1phjxOhggH/MIIB
// SIG // +wYJKoZIhvcNAQkGMYIB7DCCAegCAQEwZzBTMQswCQYD
// SIG // VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4x
// SIG // KzApBgNVBAMTIlZlcmlTaWduIFRpbWUgU3RhbXBpbmcg
// SIG // U2VydmljZXMgQ0ECEA3pK/DU2CmIGDIFCV6adogwDAYI
// SIG // KoZIhvcNAgUFAKBZMBgGCSqGSIb3DQEJAzELBgkqhkiG
// SIG // 9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTA1MTAzMDE3NTgw
// SIG // MFowHwYJKoZIhvcNAQkEMRIEEPEbZPTGNku+iT98EjjI
// SIG // j+0wDQYJKoZIhvcNAQEBBQAEggEAXHLoMZLIi+0yYxYF
// SIG // tEN3nY+GKTD9DGwo+KkKPKxHRKFAnlbka/NBlmiWVKHr
// SIG // xo0+fwZ2Ku2ovvNSP02k3kpKqbAyItG2dW9B9/Uu2Vja
// SIG // v7szcPVAzwgznJyx9KOk7g/1kkUe3cFBLfg3TrfW8VFT
// SIG // EhRCuIWoCfEmujUfHWwIjo2/sfoNMzrcElxmg/+826Bh
// SIG // QLlu2/c+OUMAs+s4jaEeqj8Up1xjKoFEvWWQmYDxnv7T
// SIG // EevK/yG1IKRJHqPf8nnYbiMARforJY84iL8JYvo6B2PS
// SIG // HhS/dA2sRTGpVjACGWn2fd61yMMMZh7KIbz5aERMFTNd
// SIG // UyGxE6sCOEQbdggkpw==
// SIG // End signature block