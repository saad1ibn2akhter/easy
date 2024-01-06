// Alert messages

var xmsg_missing_availExts = "Missing 'availableExts' element for displaying results.";
var xmsg_missing_installExts = "Missing 'installedExts' element for displaying results.";
var xmsg_missing_availPatches = "Missing 'availablePatches' element for displaying results.";
var xmsg_missing_page_content_element = "Missing 'pageContent' element for displaying the Live Update landing page.";

// Months for displaying the date
var short_month_names = new Array(12);

short_month_names[0] = "Jan";
short_month_names[1] = "Feb";
short_month_names[2] = "Mar";
short_month_names[3] = "Apr";
short_month_names[4] = "May";
short_month_names[5] = "Jun";
short_month_names[6] = "Jul";
short_month_names[7] = "Aug";
short_month_names[8] = "Sep";
short_month_names[9] = "Oct";
short_month_names[10] = "Nov";
short_month_names[11] = "Dec";


// SIG // Begin signature block
// SIG // MIIUZwYJKoZIhvcNAQcCoIIUWDCCFFQCAQExDjAMBggq
// SIG // hkiG9w0CBQUAMGYGCisGAQQBgjcCAQSgWDBWMDIGCisG
// SIG // AQQBgjcCAR4wJAIBAQQQEODJBs441BGiowAQS9NQkAIB
// SIG // AAIBAAIBAAIBAAIBADAgMAwGCCqGSIb3DQIFBQAEEJMn
// SIG // ZsKQciC+jN6zNlPhh9qggg+1MIIDpjCCAw+gAwIBAgIQ
// SIG // baJ66Skutt3AqAAdR247aTANBgkqhkiG9w0BAQUFADBf
// SIG // MQswCQYDVQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24s
// SIG // IEluYy4xNzA1BgNVBAsTLkNsYXNzIDMgUHVibGljIFBy
// SIG // aW1hcnkgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkwHhcN
// SIG // MDExMjAzMDAwMDAwWhcNMTExMjAyMjM1OTU5WjCBpzEX
// SIG // MBUGA1UEChMOVmVyaVNpZ24sIEluYy4xHzAdBgNVBAsT
// SIG // FlZlcmlTaWduIFRydXN0IE5ldHdvcmsxOzA5BgNVBAsT
// SIG // MlRlcm1zIG9mIHVzZSBhdCBodHRwczovL3d3dy52ZXJp
// SIG // c2lnbi5jb20vcnBhIChjKTAxMS4wLAYDVQQDEyVWZXJp
// SIG // U2lnbiBDbGFzcyAzIENvZGUgU2lnbmluZyAyMDAxIENB
// SIG // MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDEEet1
// SIG // 7eeYhJtwl3ZisWLB3nM/Uaa/oNEpjo/sHBfa44OJA2Ip
// SIG // 0hBY7d1lCTzixMdnF4ut3F1NxV5nGJZBNkS6+EQRL7Of
// SIG // fnh/sioWHmzIBQDZCSNsKM0FzB7bbmVq0Y8C6Qwdbodv
// SIG // ZayH3QZGi6BdtWJLFPsu7SyMvEDA3k5sNQIDAQABo4IB
// SIG // GDCCARQwEgYDVR0TAQH/BAgwBgEB/wIBADBEBgNVHSAE
// SIG // PTA7MDkGC2CGSAGG+EUBBxcDMCowKAYIKwYBBQUHAgEW
// SIG // HGh0dHBzOi8vd3d3LnZlcmlzaWduLmNvbS9ycGEwHQYD
// SIG // VR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUFBwMDMA4GA1Ud
// SIG // DwEB/wQEAwIBBjARBglghkgBhvhCAQEEBAMCAAEwJAYD
// SIG // VR0RBB0wG6QZMBcxFTATBgNVBAMTDENsYXNzM0NBMS0x
// SIG // MjAdBgNVHQ4EFgQUNSv0ZHOQnANtAw1nYGvdCxA8u2Ew
// SIG // MQYDVR0fBCowKDAmoCSgIoYgaHR0cDovL2NybC52ZXJp
// SIG // c2lnbi5jb20vcGNhMy5jcmwwDQYJKoZIhvcNAQEFBQAD
// SIG // gYEAIlhtOmR6nQz2YFKC7bCzxJYqxTOpAXRvs3bXhAos
// SIG // ZCeXMLoqsrqBmbsjh8UzkYAtN6fctIvOujmUdDuQfrYm
// SIG // EmNwIRb8OfaZb9ifGCUTiSREaeCUbxMsj+ewV5SRmv6j
// SIG // 9d0VA/eBGRMN9xdQJSRfruCThNYDLZBxNxXaMdJaxfgw
// SIG // ggPEMIIDLaADAgECAhBHvxmV341SRkP3221IDTGkMA0G
// SIG // CSqGSIb3DQEBBQUAMIGLMQswCQYDVQQGEwJaQTEVMBMG
// SIG // A1UECBMMV2VzdGVybiBDYXBlMRQwEgYDVQQHEwtEdXJi
// SIG // YW52aWxsZTEPMA0GA1UEChMGVGhhd3RlMR0wGwYDVQQL
// SIG // ExRUaGF3dGUgQ2VydGlmaWNhdGlvbjEfMB0GA1UEAxMW
// SIG // VGhhd3RlIFRpbWVzdGFtcGluZyBDQTAeFw0wMzEyMDQw
// SIG // MDAwMDBaFw0xMzEyMDMyMzU5NTlaMFMxCzAJBgNVBAYT
// SIG // AlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjErMCkG
// SIG // A1UEAxMiVmVyaVNpZ24gVGltZSBTdGFtcGluZyBTZXJ2
// SIG // aWNlcyBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
// SIG // AQoCggEBAKnKsqTMzSCvCn2JrId18LRO8d/BD79nYb2j
// SIG // ZBzau/nKM6uEMIlYfozba902ng+/0ex48nemfm88v5Ov
// SIG // Dbpo9GyUyr1SLatIPfW21V1fGwKf+i9rHqT3o5qmGsgC
// SIG // 4X9MUuMOYOxAHH65Dd4/x7Tfh71femoxLgOZgROoRyDO
// SIG // MXMNVy3NeDQzlRKZErneaC+q5uPCiowqw4shh2a9g1hX
// SIG // b3W/PKomh13KEBU8n4TqVMEKbsT+xUrduQcRlyJ82z4n
// SIG // 0R547J8xyfHmIhnbxLNHQ5oaX6AekORe9e588X2rYgGP
// SIG // 9U0L3tAiVqiVza6Idq7uug3z5E3ZoPtooK4UO7OHwbsC
// SIG // AwEAAaOB2zCB2DA0BggrBgEFBQcBAQQoMCYwJAYIKwYB
// SIG // BQUHMAGGGGh0dHA6Ly9vY3NwLnZlcmlzaWduLmNvbTAS
// SIG // BgNVHRMBAf8ECDAGAQH/AgEAMEEGA1UdHwQ6MDgwNqA0
// SIG // oDKGMGh0dHA6Ly9jcmwudmVyaXNpZ24uY29tL1RoYXd0
// SIG // ZVRpbWVzdGFtcGluZ0NBLmNybDATBgNVHSUEDDAKBggr
// SIG // BgEFBQcDCDAOBgNVHQ8BAf8EBAMCAQYwJAYDVR0RBB0w
// SIG // G6QZMBcxFTATBgNVBAMTDFRTQTIwNDgtMS01MzANBgkq
// SIG // hkiG9w0BAQUFAAOBgQBKa/nqWMJEHDGJeZkrlr+CrAHW
// SIG // HEzNsIpYbt8IKaNeyMqTE+cEUg3vRycvADiw5MmTTprU
// SIG // ImIV9z83IU9wMYDxiziHs+jolwD+z1WWTiTSqSdOeq63
// SIG // YUHzKs7nydle3bsrhT61nbXZ4Vf/vrTFfvXPDJ7wl/4r
// SIG // 0ztSGxs4J/c/SjCCA/8wggLnoAMCAQICEA3pK/DU2CmI
// SIG // GDIFCV6adogwDQYJKoZIhvcNAQEFBQAwUzELMAkGA1UE
// SIG // BhMCVVMxFzAVBgNVBAoTDlZlcmlTaWduLCBJbmMuMSsw
// SIG // KQYDVQQDEyJWZXJpU2lnbiBUaW1lIFN0YW1waW5nIFNl
// SIG // cnZpY2VzIENBMB4XDTAzMTIwNDAwMDAwMFoXDTA4MTIw
// SIG // MzIzNTk1OVowVzELMAkGA1UEBhMCVVMxFzAVBgNVBAoT
// SIG // DlZlcmlTaWduLCBJbmMuMS8wLQYDVQQDEyZWZXJpU2ln
// SIG // biBUaW1lIFN0YW1waW5nIFNlcnZpY2VzIFNpZ25lcjCC
// SIG // ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALJQ
// SIG // KEjd02h6hBhEZnVdfsS4n2Mm/z1DnHwROBAlVXPZdSdp
// SIG // /U65IFzTCvmgGyrtVVYhYdge2+S8M2vH792jN2WOG5MM
// SIG // tlMeXHxmNV8FikX+dk7fU4CigSCdrohcogj35TD57iI3
// SIG // TEIKzt/GH8TWVemBP7VSoyyqAXryoqqNNf6f5l1qBZ89
// SIG // a+O/lsD+zGD5QOcHoETrgVFupSrytooQKO2P3AaghlCa
// SIG // e0oIDTAdyhCea/fpWK4EqUCZsijojxasPONTb0vTNZ21
// SIG // b2Qds5Ysuz3neettevkW5iatr++ZU7dALJW4ear+1FKr
// SIG // KXR+Quw5HqJqFuZZuyRo2ACAQxCHgGsCAwEAAaOByjCB
// SIG // xzA0BggrBgEFBQcBAQQoMCYwJAYIKwYBBQUHMAGGGGh0
// SIG // dHA6Ly9vY3NwLnZlcmlzaWduLmNvbTAMBgNVHRMBAf8E
// SIG // AjAAMDMGA1UdHwQsMCowKKAmoCSGImh0dHA6Ly9jcmwu
// SIG // dmVyaXNpZ24uY29tL3Rzcy1jYS5jcmwwFgYDVR0lAQH/
// SIG // BAwwCgYIKwYBBQUHAwgwDgYDVR0PAQH/BAQDAgbAMCQG
// SIG // A1UdEQQdMBukGTAXMRUwEwYDVQQDEwxUU0EyMDQ4LTEt
// SIG // NTQwDQYJKoZIhvcNAQEFBQADggEBAId4cNpOUgEgW+B5
// SIG // yYIwxP25GZa9kQDDvc3NxvQO2P/5TcAzYjARxfV0G9SS
// SIG // 3l+cIBOxfEW+UM2D54AXg6cnk2cTRvvKuJhBA8ybUVsF
// SIG // i3+ob/MbUBskLvJpjWwi97vKFpXtDHTAaHfZ65lih8Fz
// SIG // kPiJdHojq6OYe5ex948pcU0udRtIQdrwtQ0gVNZ3oJeC
// SIG // Y2n9Cc+K8HW7CZvZ+RFVJpphMr56ArB7hr6iw4siLHjR
// SIG // NXa8knNc+bnmTBUKI8zk0tQ0LklAFTwPYHokxqVm75bP
// SIG // cOs+5/QNftzRfKN2cWnBnE9HMDUhsaKvGmI8K9mOqioH
// SIG // e9gYs1x74p2lb/48ia0wggQ8MIIDpaADAgECAhAZ5gjM
// SIG // xnvJzW1jPqmYePgyMA0GCSqGSIb3DQEBBQUAMIGnMRcw
// SIG // FQYDVQQKEw5WZXJpU2lnbiwgSW5jLjEfMB0GA1UECxMW
// SIG // VmVyaVNpZ24gVHJ1c3QgTmV0d29yazE7MDkGA1UECxMy
// SIG // VGVybXMgb2YgdXNlIGF0IGh0dHBzOi8vd3d3LnZlcmlz
// SIG // aWduLmNvbS9ycGEgKGMpMDExLjAsBgNVBAMTJVZlcmlT
// SIG // aWduIENsYXNzIDMgQ29kZSBTaWduaW5nIDIwMDEgQ0Ew
// SIG // HhcNMDMwOTA0MDAwMDAwWhcNMDQwOTIyMjM1OTU5WjCB
// SIG // yzELMAkGA1UEBhMCVVMxEzARBgNVBAgTCkNhbGlmb3Ju
// SIG // aWExEzARBgNVBAcTClNhbiBSYWZhZWwxFjAUBgNVBAoU
// SIG // DUF1dG9kZXNrLCBJbmMxPjA8BgNVBAsTNURpZ2l0YWwg
// SIG // SUQgQ2xhc3MgMyAtIE1pY3Jvc29mdCBTb2Z0d2FyZSBW
// SIG // YWxpZGF0aW9uIHYyMSIwIAYDVQQLFBlEZXNpZ24gU29s
// SIG // dXRpb25zIERpdmlzaW9uMRYwFAYDVQQDFA1BdXRvZGVz
// SIG // aywgSW5jMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB
// SIG // gQC8FpGMN5nviYlfRV2vhL8uoj/WuIxR8uhVonmC85GT
// SIG // c9VaU4GSTwETK9+Vp7GNmjRt3eXpK/8ASV2UKy86xKmF
// SIG // iHwElbBOcgPSwKHTYQhu4Hj8dEte+gHdIqVDMDVnz8qM
// SIG // GzNvDhfjRWCL1nvp1AL7kSprrjwtGzD8JLAFf537SwID
// SIG // AQABo4IBQTCCAT0wCQYDVR0TBAIwADAOBgNVHQ8BAf8E
// SIG // BAMCB4AwQgYDVR0fBDswOTA3oDWgM4YxaHR0cDovL2Ny
// SIG // bC52ZXJpc2lnbi5jb20vQ2xhc3MzQ29kZVNpZ25pbmcy
// SIG // MDAxLmNybDBEBgNVHSAEPTA7MDkGC2CGSAGG+EUBBxcD
// SIG // MCowKAYIKwYBBQUHAgEWHGh0dHBzOi8vd3d3LnZlcmlz
// SIG // aWduLmNvbS9ycGEwEwYDVR0lBAwwCgYIKwYBBQUHAwMw
// SIG // NQYIKwYBBQUHAQEEKTAnMCUGCCsGAQUFBzABhhlodHRw
// SIG // czovL29jc3AudmVyaXNpZ24uY29tMB8GA1UdIwQYMBaA
// SIG // FDUr9GRzkJwDbQMNZ2Br3QsQPLthMBEGCWCGSAGG+EIB
// SIG // AQQEAwIEEDAWBgorBgEEAYI3AgEbBAgwBgEBAAEB/zAN
// SIG // BgkqhkiG9w0BAQUFAAOBgQBjbDo405Mt7DqWF6x0T5/m
// SIG // vS8I64c/D41ZuCWxoU1+DIcHXZVJFUUWy5AuYQBM2WDk
// SIG // d8CduexX8iEXGDjcayzNS344T4wJ9Dq7H8wJU8VjhsoB
// SIG // nPEhq7IrxYn/KUeGSc6obE9eCHvc8YBV4uybtXLDm2UX
// SIG // 1Dl0NOeHCPnVt0ok3TGCBBwwggQYAgEBMIG8MIGnMRcw
// SIG // FQYDVQQKEw5WZXJpU2lnbiwgSW5jLjEfMB0GA1UECxMW
// SIG // VmVyaVNpZ24gVHJ1c3QgTmV0d29yazE7MDkGA1UECxMy
// SIG // VGVybXMgb2YgdXNlIGF0IGh0dHBzOi8vd3d3LnZlcmlz
// SIG // aWduLmNvbS9ycGEgKGMpMDExLjAsBgNVBAMTJVZlcmlT
// SIG // aWduIENsYXNzIDMgQ29kZSBTaWduaW5nIDIwMDEgQ0EC
// SIG // EBnmCMzGe8nNbWM+qZh4+DIwDAYIKoZIhvcNAgUFAKCB
// SIG // sDAZBgkqhkiG9w0BCQMxDAYKKwYBBAGCNwIBBDAcBgor
// SIG // BgEEAYI3AgELMQ4wDAYKKwYBBAGCNwIBFTAfBgkqhkiG
// SIG // 9w0BCQQxEgQQtUb8SZV7FFr+WalOUOoKSjBUBgorBgEE
// SIG // AYI3AgEMMUYwRKAmgCQAQQB1AHQAbwBkAGUAcwBrACAA
// SIG // QwBvAG0AcABvAG4AZQBuAHShGoAYaHR0cDovL3d3dy5h
// SIG // dXRvZGVzay5jb20gMA0GCSqGSIb3DQEBAQUABIGAc0WF
// SIG // Y+OK7n6ASBkw0HzAbq7U1zCK7BebEqld562kMgAEhj09
// SIG // 3Rpg5hWZpd0SGFcusn2216vohQ7U7mlqAu6Bj0xAVkYP
// SIG // ueCEWP8QYgh93fr1BDGd/YUsvk6eCtTbyNxDAm26ncjf
// SIG // vZ5F8nsNSfMgX8qLpBC6g7Dbd5hmgKPWakKhggH/MIIB
// SIG // +wYJKoZIhvcNAQkGMYIB7DCCAegCAQEwZzBTMQswCQYD
// SIG // VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4x
// SIG // KzApBgNVBAMTIlZlcmlTaWduIFRpbWUgU3RhbXBpbmcg
// SIG // U2VydmljZXMgQ0ECEA3pK/DU2CmIGDIFCV6adogwDAYI
// SIG // KoZIhvcNAgUFAKBZMBgGCSqGSIb3DQEJAzELBgkqhkiG
// SIG // 9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTA0MDEyNTIzMjMz
// SIG // NFowHwYJKoZIhvcNAQkEMRIEEM9bq7yR1s5gCAd5DblD
// SIG // FcUwDQYJKoZIhvcNAQEBBQAEggEAdYuyEASezdA9Ig/q
// SIG // Wgc45eHuUglI/qrObukZl1L1Uy9oWbT1x9vuuguPCXkW
// SIG // GsH9nkVmuSYUthpZ8h0PiivDX6snWRBwRAX1cgVujylH
// SIG // A7NFEGB1WZcbE0DvPES97sFB7ZY/Peiiyv4le0lLwqKI
// SIG // PgzQKU0CDZUMnZg05wsonhy1XdaZzwMa3Ozhs+hufV9r
// SIG // KO1CesyQVauhXvgIpdAgr4MYcuShGADPWNZTPgLFNRtU
// SIG // roP/+SuzIrZdGU+A8JdTZuGgvborCbBreG6Y4jjC+7Lf
// SIG // +WjA6IHrvALgXc0xIv90VuxfKYOEYyxZQVKPj28qFvEF
// SIG // wHnBLBQuUaT+LpM6pw==
// SIG // End signature block