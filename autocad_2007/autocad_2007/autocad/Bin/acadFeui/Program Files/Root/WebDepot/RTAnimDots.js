// Functions to let users see movement while waiting
// for backend connections.
function StartAni() {
    document.body.style.cursor = "wait";
    sequence = new Array (wait0, wait1, wait2, wait3, wait4, wait5);
    currentSequence=0;
    waitdiv.style.display = "block";
    sequence[currentSequence].style.visibility="visible";
    iTimerID = window.setInterval("Animate()", 1500);
}

function Animate() {
    currentSequence++;
    if (currentSequence >= 6) {
        do {
            currentSequence -= 1;
            sequence[currentSequence].style.visibility = "hidden";
        } while (currentSequence > 0);
    }
    sequence[currentSequence].style.visibility = "visible";
}

// Division "waitdiv" controls the whole animation block.
// display:block opens space for the text in the doc window.
// Each Span gets visibility changed from hidden to visible
// for on screen movement.
document.writeln("<DIV id=\"waitdiv\" style=\"position:relative; display:none\">");
document.writeln("<center>");
if (skipdialog == "y") {
	document.writeln("<font size=\"2\">");
	document.writeln(xmsgNoDialogHdrMsg);
	document.writeln("</font><font size=\"2\"><b>");
} else {
	document.writeln("<font size=\"2\"><b>");
	document.writeln(xmsgConnectHdr1);
	document.writeln("&nbsp;</b></font> <font size=\"4\" color=\"0067cc\"><b>");
}
document.writeln("<span id=\"wait0\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("<span id=\"wait1\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("<span id=\"wait2\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("<span id=\"wait3\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("<span id=\"wait4\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("<span id=\"wait5\" style=\"position:relative; visibility:hidden;\">.&nbsp;</span>");
document.writeln("</b></font>");
document.writeln("</center>");
if (skipdialog == "y") {
	document.writeln("<br>");
} else {
	//	display warming for sending out full package
	if (processstage != "0") {
		document.writeln("<br><center>");
		document.writeln("<font size=\"2\">");
		document.writeln(xmsgConnectWarming);
		document.writeln("</font>");
		document.writeln("</center>");
	}
	document.writeln("<p>");
}
document.writeln("</DIV>");









// SIG // Begin signature block
// SIG // MIIWVgYJKoZIhvcNAQcCoIIWRzCCFkMCAQExDjAMBggq
// SIG // hkiG9w0CBQUAMGYGCisGAQQBgjcCAQSgWDBWMDIGCisG
// SIG // AQQBgjcCAR4wJAIBAQQQEODJBs441BGiowAQS9NQkAIB
// SIG // AAIBAAIBAAIBAAIBADAgMAwGCCqGSIb3DQIFBQAEELkJ
// SIG // 0ZTAryjKmraDHhWPn6agghGXMIIDxDCCAy2gAwIBAgIQ
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
// SIG // 9w0BCQQxEgQQ6twcTZWPc9IChflCZZ7mUTBUBgorBgEE
// SIG // AYI3AgEMMUYwRKAmgCQAQQB1AHQAbwBkAGUAcwBrACAA
// SIG // QwBvAG0AcABvAG4AZQBuAHShGoAYaHR0cDovL3d3dy5h
// SIG // dXRvZGVzay5jb20gMA0GCSqGSIb3DQEBAQUABIGAVeip
// SIG // D3jy0elEOd6iIgbvWnIaMKXlnQ1rCQPvlllQEwh51hNQ
// SIG // eoqVbrGUDKvcEx6HroJ5L81bffej4ajGm9kYu953zNEZ
// SIG // i4I6z3oUT3A/7kOelaTB+CZ0jMhlmclAWibS95IMvgjX
// SIG // 2U4cCQT7CHvv4ezTeR37oittaOduEo5xTOahggH/MIIB
// SIG // +wYJKoZIhvcNAQkGMYIB7DCCAegCAQEwZzBTMQswCQYD
// SIG // VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4x
// SIG // KzApBgNVBAMTIlZlcmlTaWduIFRpbWUgU3RhbXBpbmcg
// SIG // U2VydmljZXMgQ0ECEA3pK/DU2CmIGDIFCV6adogwDAYI
// SIG // KoZIhvcNAgUFAKBZMBgGCSqGSIb3DQEJAzELBgkqhkiG
// SIG // 9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTA1MTAzMDE3NTc1
// SIG // NlowHwYJKoZIhvcNAQkEMRIEEJ+7p2eso8Wdgb8bYW0y
// SIG // 0CAwDQYJKoZIhvcNAQEBBQAEggEAQJj+IOiKWOBWJFfL
// SIG // CvwhBgoZeflRYX2mmxhnWWa8kVZ6egJkWKucnZapf2tS
// SIG // abvty9TfHFCDwZSuPwbWy5A7wViVBLn2HXrXSOkUwqq1
// SIG // W4Ke7vjSDpeSdQRwn/5ZC0ClAPHVZmpobuvRn2s3YePJ
// SIG // Ol63sIkeKhfZCAmT941HfWiqVuftqf5cOUmtwFs5PGgt
// SIG // MVPv0rHIXQRG7+2nRVdO54ijh9K07nh5N0493bRnko2+
// SIG // 1y1zasAICUc8uiwtHjB0LWjMefXJh4K61Zf+LtQ7/wZb
// SIG // /+Wd32ijCT3DkDuzn9Qb19Y26qsObihizQydKHheTLXx
// SIG // pcC0pQcIsg0jaWtV7Q==
// SIG // End signature block
