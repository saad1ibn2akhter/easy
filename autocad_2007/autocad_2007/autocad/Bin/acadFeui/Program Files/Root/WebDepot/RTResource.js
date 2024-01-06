/*==============================================================================================
   HTML Parsing Blocks. The following vars contain HTML code to be inserted in the Register
   Today HTML pages based on the business logic, e.g., whether the registration is for a company
   or individual, whether an old serial number is needed, whether State/Region/Province is 
   required, etc.

   (C) Copyright 2004 by Autodesk, Inc.  All rights reserved.
  ==============================================================================================*/

/////////////////////////////////////////////
// LOCALIZATION RESOURCES HERE:
/////////////////////////////////////////////

/////////////////////////////////////////////
// Product Language: 
//
// NOTE TO LOCALIZATION: The list below was inserted on 29 Jan 2001 and it was up to date at that point.
// You may use the codes detailed below if you are sure that these have not changed (they generally don’t change), otherwise, 
// please check these before inserting the language code.
/////////////////////////////////////////////
//
//English - US and International:  (en-us)
//German:  (de)
//French:   (fr)
//Italian:   (it)
//Spanish:   (es)
//Czech:   (cs)             
//Hungarian:   (hu)
//Polish:   (pl)
//Russian:   (ru)
//Japanese:   (ja)
//Korean:   (ko)
//Traditional Chinese (Taiwan) :   (zh-tw)
//Simplified Chinese (PRC) :   (zh-cn)
//Brazilian Portuguese (pt-br)
//
/////////////////////////////////////////////

var xmsgProductLanguage=	"en-us";

/////////////////////////////////////////////
// Disable field color
/////////////////////////////////////////////
var xmsgDisableColor=		"#7f7f7f";

var xmsgNavigationBGColor=	"#f4f4f4";
var xmsgNavigationDisableColor=	"#808080";

/////////////////////////////////////////////
// HTML FILE TITLE STRINGS
/////////////////////////////////////////////
var xmsgRT0015TempSysErr=				"Connection Error (0.15)";
var xmsgRT010203071213CannotCompErrA=	"Registration Error (1.2.3.7.12.13)";
var xmsgRT09AlreadyReg=					"Already Registered (9)";
var xmsgRT10ExceededAuths=				"Exceeded Auths (10)";
var xmsgRT12StolenSN=					"Stolen Serial Number (12)";
var xmsgRT18OldSNInvalid=				"Previous Serial Number Invalid (18)";
var xmsgRT20DataError=					"Data Error (20)";
var xmsgRT21CustIDTelErr=				"Customer ID/Tel error (21)";
var xmsgRT22NetworkInstallation=		"Network Installation Error (22)";
var xmsgRT900SystemNotResponding=		"System Not Responding (900)";
var xmsgRTAuthConfirmed=				"Auth Confirmed";
var xmsgRTAuthEntryError=				"Activation Entry Error";
var xmsgRTAuthError=					"Activation Error";
var xmsgRTAuthNow=						"Auth Now";
var xmsgRTBeginReg=						"Begin Registration";
var xmsgRTBuyInformation=				"Purchase Information";
var xmsgRTConfirm=						"User Confirm";
var xmsgRTConnect=						"Connect";
var xmsgRTConnectFail=					"Connection error";
var xmsgRTContactAutodesk=				"Contact Autodesk";
var xmsgRTCountryPage=					"Country";
var xmsgRTCustID=						"Customer ID";
var xmsgRTCustIDConfirm=				"Confirm Identification";
var xmsgRTEmail=						"Email";
var xmsgRTFaxUS=						"User Fax US";
var xmsgRTMailUS=						"User Mail US";
var xmsgRTNewSerialNumber=				"New Serial Number Information";
var xmsgRTNoNewSN=						"Purchase Transaction Error";
var xmsgRTPurchAuthFailed=				"Activation Error";
var xmsgRTPurchConfirmOther=			"Purchase Confirm";
var xmsgRTPurchConfirmPerm=				"Purchase Confirm - Registered and Activated";
var xmsgRTPurchConfirmReg=				"Purchase Confirm - Registered But Not Activated";
var xmsgRTPurchConfirmRegVol=			"Purchase Confirm - Registered";
var xmsgRTPurchConfirmRent=				"Purchase Confirm - Registered and Active Rental";
var xmsgRTPurchConfirmUnReg=			"Purchase Confirm - Not Registered or Activated";
var xmsgRTPurchConnectionFailed=		"Purchase Connection Failed";
var xmsgRTPurchaseError=				"Purchase Error";
var xmsgRTPurchaseOffline=				"Purchase Offline";
var xmsgRTRegConfirmed=					"Registration Confirmed";
var xmsgRTRegVol=						"Register Product";
var xmsgRTSNMismatch=					"Purchase Transaction Error - Serial Number Mismatch";
var xmsgRTSUNDataConfirm=				"SUN Data Confirm";
var xmsgRTSUNOldSNInvalid=				"Previous Serial Number Invalid - SUN";
var xmsgRTSUNServer=					"SUN Server";
var xmsgRTSUNSubmit=					"User Confirm/Submit SUN";
var xmsgRTUserInfo=						"User Information";
var xmsgRTLicFileSaveError=				"Flexlm License File Save Error";
var xmsgNSARTBegin=						"Obtain A Network License";
var xmsgNSARTLocateLicense=				"Locate License File";
var xmsgNSARTEditServerAndSeat=			"Enter of Modify Server Information";
var xmsgNSARTConfirmServerAndSeat=		"Confirm Server Information";
var xmsgNSARTEnterSN=					"Enter Serial Number";


/////////////////////////////////////////////
// Software Information RESOURCES HERE:
/////////////////////////////////////////////
var xmsgProduct=	"Product: ";
var xmsgsernum=		"Serial number: ";
var xmsgsernumWGID=	"Serial number/Group ID: ";
var xmsgOsernum=	"Previous serial number: ";
var xmsgOldSerialNum=	"Previous serial number:*";
var xmsgNewSernum1=	"New Serial Number";
var xmsgRequestCode=	"Request code: ";
var xmsgRegAuthCode=	"Activation code: ";
var xmsgApplicationKey=	"Application key: ";
var xmsgRTHostID=				"Host ID:";
var xmsgRTFlexlmLicenseFile=	"License file:";

/////////////////////////////////////////////
// Primary Rolodex RESOURCES HERE:
/////////////////////////////////////////////
var xmsgSalutation=	"Salutation:";
var xmsgNone=		"(none)";
var xmsgNotEntered=	"(not entered)";
var xmsgFirstname=	"First name:";
var xmsgFirstnameKanji=	"First name (Kanji):";
var xmsgFirstnameFurigana=	"First name (Zenkaku Kana):";
var xmsgLastname=	"Last name:";
var xmsgLastnameKanji=	"Last name (Kanji):";
var xmsgLastnameFurigana=	"Last name (Zenkaku Kana):";
var xmsgCompanyName=	"Company name:*";
var xmsgCompanyName2=	"Company name:";
var xmsgCompanyNameJP=	"Company name (Kanji):";
var xmsgPhone=		"Telephone:";
var xmsgArea=		"Area:";
var xmsgNumber=		"Number:";
var xmsgExt=		"Ext:";
var xmsgFax=		"Fax:";
var xmsgFaxTo=		"Fax to ";
var xmsgEmail=		"Email:";
var xmsgEmailTo=	"Email to ";
var xmsgAddress=	"Street address:";
var xmsgAddress2=	"Street address 2:";
var xmsgAddress3=	"Street address 3:";
var xmsgJPAddress=	"Town:";
var xmsgJPAddress2=	"Street address:";
var xmsgJPAddress3=	"Building name (and Suite number):";
var xmsgCity=		"City:";
var xmsgSRP=		"State/Region/Province:";
var xmsgStText=		"State:";
var xmsgPrText=		"Province:";
var xmsgRegText=	"Region:";
var xmsgPrefectureText=	"Prefecture:";
var xmsgCountyText=	"County:";
var xmsgCantonText=	"Canton:";
var xmsgZipCode=	"Zip code:";
var xmsgPostCode=	"Post code:";
var xmsgCountry=	"Country or region:";
var xmsgDealer=		"Autodesk Authorized Reseller name:";
var xmsgMailTo=		"Mail to ";
var xmsgAnEmail=	"an email address";
var xmsgAFax=		"a fax number";
var xmsgAMailAddress=	"a mailing address";

///////////////////////////////////////////////
// Button RESOURCES HERE:
///////////////////////////////////////////////
var xmsgBack=			"Back";
var xmsgCancel=			"Cancel";
var xmsgExit=			"Exit";
var xmsgNext=			"Next";
var xmsgPrint=			"Print";
var xmsgNo=				"No";
var xmsgYes=			"Yes";
var xmsgRegister=		"Register";
var xmsgAuthorize=		"Activation";
var xmsgBrowse=			"Browse";
var xmsgSkipStep=		"Back";
var xmsgLoad=			"Next";
var xmsgDone=			"Done";
var xmsgOK=				"OK";
var xmsgClose=			"Close";
var xmsgSubmit=			"Submit";
var xmsgFinish=			"Finish";
var xmsgReset=			"Clear All";

///////////////////////////////////////////////
// Registration User Prompt RESOURCES HERE:
///////////////////////////////////////////////
var xmsgPleaseSelect=		"Please select ";
var xmsgPleaseSelect2=		"Select an option below and then click Next.";
var xmsgSelectCountry=		"Country/Region in which the product will be used:";
var xmsgSelectCountry2=		"Contact information for:";
var xmsgInformation=		"Information";
var xmsgCompanyInfo=		"Company Information";
var xmsgContactInfo=		"Contact Information";
var xmsgConfirmInfo=		"Confirm Information";
var xmsgConfirmInfoTxt=		"Review the following information. Click Submit to submit the information, or click Back to make changes.";
var xmsgConfirmCustIDInfoTxt=		"Confirm the following information. Click Next to continue, or click Back to make changes.";
var xmsgCountryHdr=			"Begin Registration - Activation";
var xmsgNSACountryHdr=		"Register and Activate Your Autodesk Product";
var xmsgCountryRegTo=		"This product will be registered to:";
var xmsgCountryComp=		"Company";
var xmsgCountryIndiv=		"Individual";
var xmsgCountryUpgrade=		"This is an upgrade";
var xmsgCountryUpgDetail=	"(If you are a subscription customer, click No)";
var xmsgConnectHdr1=		"Connecting";
var xmsgConnectHdr=		"Connect to the Internet";
var xmsgConnectMsg=		"Connection";
var xmsgConnectTxt=		"No Internet connection was detected. To submit an online request, connect to the Internet now.";
var xmsgConnectTxt2=	"When an Internet connection is established, click Next.";
var xmsgConnectTxt3=	"To use a different method to submit your request, click the following link:";
var xmsgConnectWarming=	"Submitting this information may take a few minutes. Please wait.";
var xmsgTryAgain=		"Try again";
var xmsgReEnterOld=		"Re-enter previous serial number";
var xmsgNewSernum3=		"Please enter your New Serial Number and click Next.";
var xmsgNewSernum4=		"Your trial software is installed with a Serial Number (000-00000000) that " +
						"cannot be registered. If you purchased a copy of this software, you were " +
						"issued a New Serial Number. You must use this Serial Number to activate and " +
						"enable your product for use.";
var xmsgNewSernum5=		"New serial number:";
var xmsgNewSernum6=		"Confirm new serial number:";
var xmsgNewSernum8=		"Re-enter New Serial Number";
var xmsgUseOtherMethod=		"Use another registration method";
var xmsgContactAutodesk=	"Contact Autodesk";
var xmsgContactAutodeskLink="To contact Autodesk, click the following link:";
var xmsgContactOptions=		"contact options are shown below.";

var xmsgPuchaseInfo1=		"Purchase Information";
var xmsgPuchaseInfo2=		"You have chosen to purchase on-line. The following screens will:";
var xmsgPuchaseInfo3=		"Collect required information";
var xmsgPuchaseInfo4=		"Store this information on your machine for future registration purposes";
var xmsgPuchaseInfo5=		"Allow you to confirm all steps of the process";
var xmsgPuchaseInfo6=		"Confirm the purchase and activation of your software";
var xmsgPuchaseInfo7=		"Please click Next to continue, or Cancel if you do not wish to proceed.";

var xmsgRegTitle=		"Registration - Activation";
var xmsgRegTitleBuy=		"Purchase Autodesk products online";
var xmsgRegTitleNSA=		"Network License Activation";
var xmsgRegInfo=		"Customer Information";
var xmsgRegInfo2=		"Registration Method";
var xmsgRegInfo3=		"Please select a registration method and click Next.";
var xmsgRegmethod1=		"Connect directly via the Internet";
var xmsgRegmethod3=		"I would like to receive my activation code by:";
var xmsgRegmethod5=		"Mail";
var xmsgRegmethod6=		"Click Print below and mail this form to:";
var xmsgRegmethod7=		"Fax";
var xmsgRegmethod8=		"To print a form that you can fax to Autodesk at %1, first choose the method "+
						"by which you want to receive your activation code. Then, click Print. "+
						"You can then fax the form.";
var xmsgRegmethod9=		"Email";
var xmsgRegmethod10NSA=		"Your registration and license server information (shown below) " +
							"has been automatically saved to a file.";
var xmsgRegmethod10=		"Your registration information has been saved to the following file, "+
							"which will be used to complete the email process (do not modify this file):";
var xmsgRegmethod11NSA=		"Email the registration and license server information file %1 to %2.";
var xmsgRegmethod11=		"Email the registration information file %1 to %2.";
var xmsgRegmethod12=		"Make sure you do not modify the file.<p>" +
							"It is recommended that you print a copy of this page and put it in " +
							"a safe place. You will need this information to complete the email "+
							"process manually.<p>" +
							"You can print a copy of this information for your records. Click Done " +
							"to close this application.";
var xmsgRegmethod13=		"To automatically attach and send %1 to Autodesk, click the link below.";
var xmsgRegmethod14=		"Attach and send email";
var xmsgRegmethod15=		"Note: The email address is automated. Do not send email communications to this address.";
var xmsgRegmethod16=		"To register by fax or by mail, click one of the following links:";
var xmsgRegmethod17=		"Error Sending Email";
var xmsgRegmethod18NSA=		"An error occurred while the system attempted to send the registration and license server information automatically.";
var xmsgRegmethod18=		"An error occurred while the system attempted to send the registration information automatically.";
var xmsgRegmethod19=		"To Email the file manually:";
var xmsgRegmethod20NSA=		"Email the registration and license server information file %1 to %2.";
var xmsgRegmethod20=		"Email the registration information file %1 to %2.";
var xmsgRegmethod21=		"Make sure you do not modify the file.<p>" +
							"It is recommended that you print a copy of this page and put it in a safe place. " +
							"You will need this information to complete the email process manually.<p>" +
							"You can print a copy of this information for your records. Click Done to "+
							"close this application.";
var xmsgRegAuthInfo=		"Activation";
var xmsgRegAuthInfo1=		"Confirmation";
var xmsgRegAuthInfo2=		"Get an activation code";
var xmsgRegAuthInfo3=		"Enter Activation Code"; 
var xmsgRegAuthInfo4=		"Select an option below:";
var xmsgRegAuthInfo4A=		"Enter an activation code";
var xmsgRegAuthInfo4B=		"Enter activation code:"; 
var xmsgRegAuthInfo5=		"Please select country or region, enter activation code and click Next.";
var xmsgRegAuthInfo6=		"Registration - Activation Confirmation";	
var xmsgRegAuthInfo7=		"Thank you for registering and activating your product.";
var xmsgRegAuthInfo8=		"Registration Confirmation";		
var xmsgRegAuthInfo9=		"Thank you for registering your Autodesk product. This will help us to serve you better in future.";
var xmsgRegAuthInfo10=		"For your reference, a copy of your registration information has also been saved to the following location:";		
var xmsgRegAuthInfo10B=		"A copy of your registration information has been saved to the following location:";		
var xmsgRegAuthInfo11=		"Thank you for registering - your product is now activated and your license file has been saved. "+
							"Click Print to keep a record of this information.";
var xmsgRegAuthInfo12=		"Paste the activation code.";
var xmsgRegAuthInfo13=		"Type the activation code.";
var xmsgRegErrorHdr=		"Registration-Activation Error";
var xmsgRegErrorSystem=		"A temporary system problem is causing errors in your registration. "+
							"Click one of the following links."
var xmsgRegErrorOnline=		"This registration cannot be completed online. Click one of the following links.";
var xmsgRegErrorPrevReg=	"This Serial Number that you entered is already registered.";
var xmsgRegErrorMaxAuth=	"You have exceeded the number of activations allowed for this product.";
var xmsgRegErrorMaxAuth2=	"You have exceeded the number of licenses allowed for this product.";
var xmsgRegErrorInvalidOld=	"The serial number that you entered for your previous product is not "+
							"valid. Verify that the previous serial number displayed above is correct. " +
							"Click one of the following links.";
var xmsgRegErrorData=		"There is a problem with the registration data that you provided. Select " +
							"Try Again to review and correct the information you entered, use a different method " +
							"to register, or contact Autodesk to continue.";
var xmsgRegErrorNetwork=	"The product you are attempting to register is a network-authorized product " +
                            "and must be installed as such. You must uninstall your software and select " +
                            "\"Network\" for the installation type.<p>Please refer to the Network installation " +
                            "section of the installation guide for the proper installation and activation " +
                            "procedures for the Network Version.";
var xmsgRegNoResponse=		"A temporary problem is causing errors in your online registration. " +
							"Click one of the following links.";
var xmsgRegErrorAuthEntry=	"There is a problem with the activation code that you entered. " +
							"Click one of the following links.";
var xmsgRegErrorAuthPermission = "To activate this Autodesk product, you must either have administrator permissions or be " +
                            "granted elevated permissions by your system administrator. See your system administrator "+
                            "for more information about administrator and elevated permissions.";							
var xmsgRegErrorAuth=		"There is a problem with the activation code. Click one of the following links.";
var xmsgRegErrorFlex=		"There is a problem with the license. Click one of the following links.";
var xmsgRegConnError=		"Connection Error";
var xmsgRegConnErrorTxt=	"There is a problem with the Internet connection. Click one of the following links.";
var xmsgRegPurchError1=		"Purchase Error";
var xmsgRegPurchError2=		"This purchase transaction was not completed on-line. Click one of the following links.";
var xmsgRegPurchError3=		"Use another purchase method";
var xmsgRegPurchError4=		"There is a temporary system problem preventing on-line purchase. Click one of the following links.";
var xmsgRegPurchError5=		"Thank you for choosing to purchase Autodesk software.<p>A connection to complete this " +
							"process is unavailable at this time, possibly due to your system settings.<p>A click on " +
							"either of the links below will open your browser and may enable you to connect successfully.<p>";

var xmsgPurchTransErrHdr=	"Purchase Transaction Error";
var xmsgPurchTransError1=	"A New Serial Number is required to complete your purchase - no New Serial Number was issued.<br>Please contact Autodesk.";
var xmsgPurchTransError2=	"An error occurred during confirmation of your Serial Number with our servers.<br>Please contact Autodesk.";
var xmsgAuthErrHdr=			"Activation Error";
var xmsgThankForPurchase=	"Thank you for purchasing Autodesk software";
var xmsgAuthError1=			"A temporary problem prevented autorization. " +
							"Click one of the following links.";
var xmsgRentalInfo=			"If you purchased a rental product, your rental period begins when you activate this product.";
var xmsgRegLaterInfo=		"If you wish to register and activate now, click Activate.  If you want to register later, select Help|Buy " +
							"Online, and then choose Activate.";
var xmsgAuthorizeNow=		"Activate now on-line";
var xmsgAuthorizeLater=		"Activate later";
var xmsgPurchOtherProdInfo=	"If you want to purchase other products, you can do this at any time by choosing Help|Buy Online.";
var xmsgPurchAdditionalProdInfo=	"If you wish to purchase additional products, you can do this at any time by " +
									"choosing Help|Buy Online.";
var xmsgRegAndAuthInfo=		"your product is registered and activated.";
var xmsgRegButNotAuthInfo=	"Your product has been registered but not activated.";
var xmsgRegisteredInfo=		"Your product has been registered.";
var xmsgNotRegOrAuthInfo=	"Your software has not been registered or activated.";
var xmsgToAuthorizeInfo=	"To activate now, select Activate."
var xmsgRegAndActiveRentalInfo=	"your product is registered and the rental period that you purchased is now active.";
var xmsgAuthNowOrLaterInfo=	"If you wish to activate later, select Help|Buy Online and then choose Activate.";
var xmsgRemindRentalInfo=	"You will receive reminders when you near the end of your rental period."
var xmsgPurchHdr=			"Purchase Confirmation";
var xmsgRentalPurchHdr=		"Rental Purchase";

var xmsgAuthResellerMsg1=	"Purchase from your local reseller. Contact your local reseller to complete your purchase.";

var xmsgLicSaveErrInfo=		"An error occurred while saving your license file. Click one of the following links.";

var xmsgOnlineStoreMsg1=	"Purchase at the Autodesk Online store. If you are unable to find the product you are looking " +
							"for at the store, please contact an Authorized Reseller.";
var xmsgOnlineStoreMsg2=    "Online Store";
var xmsgAuthResellerMsg2=	"Authorized Reseller";
var xmsgLocateLicenseMsg1=	"Locate License File";
var xmsgLocateLicenseMsg2=	"Enter a path to the license file (or use Browse to locate one), " +
							"and then click Next to continue.";
var xmsgLocateLicenseMsg3=	"Existing license file:";
var xmsgRegError1_2Hdr=		"Serial number/Subscription group ID not valid";
var xmsgRegError1_2Msg=		"<p>You entered a serial number or group ID (subscription customers only) that is not " +
							"valid. Review the number that you entered when you installed your product.</p>" +
							"<p>If you verified that the serial number/group ID you entered is incorrect, click Close to exit " +
							"Registration-Activation. Then, uninstall the product and reinstall it using the correct serial " +
							"number/group ID.</p>" +
							"<p>Note:  If your product does not require reinstallation, exit Register Today, and then re-enter your " +
							"serial number.</p>" +
							"<p>If you verified that the serial number/group ID is correct, click one of the following links.</p>";
var xmsgRegError1_2Msg2=	"<p>Note: If you are a subscription customer, you must install your software using your group ID. Do " +
							"not use the product serial number. Both the serial number and the group ID appear on your " +
							"product packaging.</p>";
var xmsgUserContactMsg=		"You must enter at least one of the following:";

/////////////////////////////////////////////
// Single User Network RESOURCES HERE:
/////////////////////////////////////////////
var xmsgRegSunData1=		"Registration Data Saved";
var xmsgRegSunData2=		"The registration data required for client deployment has been saved in ";
var xmsgRegSunData3=		"The Previous Serial Number supplied for this upgrade registration is not valid. Please contact your administrator.";
var xmsgRegSunData4=		"This software must be re-installed with a valid Previous Serial Number.";
var xmsgRegSunData5=		"Multi-Seat Stand-Alone Deployment";
var xmsgRegSunData6=		"In order to complete the multi-seat stand-alone deployment, some registration information is required. The following pages will:";
var xmsgRegSunData7=		"Collect required information.";
var xmsgRegSunData8=		"Allow for previous serial number entry if required for an upgrade.";
var xmsgRegSunData9=		"Store this information on the server for deployment to all client installations.";
var xmsgRegSunData10=		"This software will be registered using the following information. Click Next to proceed.";

/////////////////////////////////////////////
// Voluntary RESOURCES HERE:
/////////////////////////////////////////////
var xmsgRegVol1=	"Product Registration - Authorization";
var xmsgRegVol2=	"Please select one of the following options and click Next:";
var xmsgRegVol3=	"Register now - select this option to begin on-line registration";
var xmsgRegVol4=	"Register later - select this option to run your software now";
var xmsgRegVol5=	"Don't show me this again";
var xmsgRegVol6=	"(If you wish to register later, you can select the registration option in the Help|About dialog)";

/////////////////////////////////////////////
// Japan RESOURCES HERE:
/////////////////////////////////////////////
var xmsgCustIDMainTitle=	"Customer Identification";
var xmsgCustIDTitle1=		"Customer ID:";
var xmsgCustIDText2=		"Enter your Customer ID and Telephone Number below. If you do not have a Customer ID, leave both fields blank.<p> ";
var xmsgCustIDText3=		"Note: If you are reactivating your product, you must enter a Customer ID.";
var xmsgCustIDText3NSA=		"Note: If you are reauthorizing your product, you must enter a Customer ID.";
var xmsgCustIDTitle2=		"Telephone number:";
var xmsgCustIDTelEx4=		"(example: 03-111-1234)";
var xmsgRegCustIDTel=		"The Customer ID and Telephone Number that you entered do not match our records. Click one of the following links.";
var xmsgPhoneNoAst=		"Telephone number:";
var xmsgKatakana=		"Company name (Zenkaku Kana):";
var xmsgDepartment=		"Department:";
var xmsgDepartment2=		"Department name:";

/////////////////////////////////////////////
// Autodesk Contact RESOURCES HERE:
/////////////////////////////////////////////
var xmsgFormWebHdr=	"On the Internet:";
var xmsgFormWebDataMsg1= "You can also obtain an activation code from your Autodesk Authorized Reseller or by visiting:<br>";
var xmsgFormEmailHdr=	"By Email:";
var xmsgFormEmailData=	"";
var xmsgFormEmailDataBlock= "";
var xmsgFormEmailXMLData=	"";
var xmsgFormEmailXMLDataBlock= "";
var xmsgFormFaxHdr=		"By fax:";
var xmsgFormFaxHdrLocal=	"By fax local office:";
var xmsgFormFaxHdrReg=	"Fax registration";
var xmsgFormFaxData=	"";
var xmsgFormFaxDataRegister=	"";
var xmsgFormCallHdr=	"By telephone:";
var xmsgFormCallData=	"";
var xmsgFormMailHdr=	"By mail:";
var xmsgFormMailHdrLocal=	"By mail local office:";
var xmsgFormMailHdrReg=	"Mail registration";
var xmsgFormMailData=	"";
var xmsgFormMailDataRegister=   "";
var xmsgFormPurchHdr=	"Purchase software";

/////////////////////////////////////////////
// Privacy Statement RESOURCES HERE:
/////////////////////////////////////////////

var JPprivacystatement = "The information you provide will be used by Autodesk to register and/or issue your " +
			"activation code. Registration confirms your consent for Autodesk to provide information " +
			"about you to our Authorized Business Partners for product license management and " +
			"entitlement verification purposes. Please contact Autodesk at " +
			"<a href=mailto:information@autodesk.jp>information@autodesk.jp</a> " +
			"if you prefer Autodesk not share your information with its Authorized Business Partners for " +
			"these limited purposes.    If you want to correct or update your registration data, please click "+
			"<a %1>HERE</a> " +
			"to contact Autodesk, or contact your local Autodesk office.";
			
var privacystatement = "The information you provide will be used by Autodesk to register and/or issue your activation code, " +
			"to keep you informed about our products and services, and to ensure that you benefit from being an " +
			"authorized Autodesk user. Autodesk does not sell or otherwise provide your information to anyone " +
			"outside of Autodesk or our Authorized Business Partners. If you want to correct or update previously " +
			"registered customer information, please click " +
			"<a %1>HERE</a> " +
			"to contact Autodesk, or contact your local Autodesk office.";
				
			
/////////////////////////////////////////////
// Privacy Policy
/////////////////////////////////////////////
var xmsgPrivacyPolicy="Privacy Policy";

/////////////////////////////////////////////
//  Note to Localization :
//
//	"InsertRegInfo" appears in RTUserInfo.html.  The full sentence in English is
//
//	"Fields marked in <FONT color=red>red</FONT> and with an asterisk (<FONT 
//	color=red>*</FONT>) are required to process your registration. Complete the form 
//	and click Next."
//
/////////////////////////////////////////////
var xmsgRegInfoMsg1= "Fields marked <FONT color=red>*</FONT> are required.";
var xmsgRegInfoMsg2= "The information you enter on this page is stored with your license. "+
					 "It is used for product re-activation and upgrades. Make sure you enter "+
					 "valid information here to avoid delays in using your product.";

/////////////////////////////////////////////
//  Note to Localization :
//
//	"xmsgRT113SeatMsg1" appears in NSA_RT113TooManySeats.html.  The full sentence in English is
//
//	"Our records indicate that you have [Max allowed seats] valid [product name] seats available." 
/////////////////////////////////////////////
var xmsgRT113SeatMsg1=	"Our records indicate that you have %1 valid %2 seats available.<br>";

var xmsgLicLocationText1 = "Save license file for <b>%1</b> to:";

// Set Registration Info Header
var InsertRegInfo =     "<p><b>"+xmsgRegInfo+"</b></p>" +
			  "<p>" + xmsgRegInfoMsg1 + "</p>";


//=======================================================================
// Application Usage Information
//=======================================================================
var xmsgAppUsage=	"Questions:";

var xmsgAppUsageQ1=	"To help Autodesk better understand the needs of your industry, "+
					"please answer a few short questions.";
var xmsgAppUsageQ2=	"Which of the following best describes the market in which you work?";
var xmsgAppUsageQ3=	"Which of the following best describes your profession?";
var xmsgAppUsageQ4=	"Which of the following best describes your industry?";
var xmsgAppUsageRequiredMark = ":*";
var xmsgAppUsageRequired=	"Show the questions";

var xmsgAppUsageBlankSpaces= "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";

var xmsgAppUsageOption01=	"Building Design, Construction, and Management ";
var xmsgAppUsageOption02=	"Manufacturing Design, Documentation and Management ";
var xmsgAppUsageOption03=	"Mapping, Civil Engineering, and Infrastructure Management ";
var xmsgAppUsageOption04=	"Digital Media Creation, Management, and Delivery ";
var xmsgAppUsageOption05=	"Wireless Location Services ";
var xmsgAppUsageOption20=	"Other ";

var xmsgAppUsageOption21=	"Animation ";
var xmsgAppUsageOption22=	"Application Development ";
var xmsgAppUsageOption23=	"Architecture ";
var xmsgAppUsageOption24=	"Broadcast Graphics ";
var xmsgAppUsageOption25=	"Building Equipment Manufacturing and Supply ";
var xmsgAppUsageOption26=	"Building Systems Engineering ";
var xmsgAppUsageOption27=	"Civil Engineering ";
var xmsgAppUsageOption28=	"Construction Management ";
var xmsgAppUsageOption29=	"Design Visualization ";
var xmsgAppUsageOption30=	"Editing and Finishing ";
var xmsgAppUsageOption31=	"Electrical Network Design ";
var xmsgAppUsageOption32=	"Facility Management ";
var xmsgAppUsageOption33=	"Game Development ";
var xmsgAppUsageOption34=	"Geographic Information Systems ";
var xmsgAppUsageOption35=	"Interior Design ";
var xmsgAppUsageOption36=	"IT Management ";
var xmsgAppUsageOption37=	"Landscape Architecture ";
var xmsgAppUsageOption38=	"Machinery Design ";
var xmsgAppUsageOption39=	"Manufacturing ";
var xmsgAppUsageOption40=	"Manufacturing Analysis ";
var xmsgAppUsageOption41=	"Mapping ";
var xmsgAppUsageOption42=	"Operations and Leasing ";
var xmsgAppUsageOption43=	"Planning ";
var xmsgAppUsageOption44=	"Plant Management ";
var xmsgAppUsageOption45=	"Process and Power Plant Design ";
var xmsgAppUsageOption46=	"Procurement ";
var xmsgAppUsageOption47=	"Product Design ";
var xmsgAppUsageOption48=	"Project Management ";
var xmsgAppUsageOption49=	"Sales and Marketing ";
var xmsgAppUsageOption50=	"Surveying ";
var xmsgAppUsageOption51=	"Teaching and Learning ";
var xmsgAppUsageOption52=	"Telecommunication Engineering ";
var xmsgAppUsageOption53=	"Tool and Die Design ";
var xmsgAppUsageOption54=	"Visual Effects ";
var xmsgAppUsageOption55=	"Web Video Editing and Publishing ";
var xmsgAppUsageOption59=	"Other ";
var xmsgAppUsageOption60=	"Application Development ";
var xmsgAppUsageOption61=	"Automotive Components ";
var xmsgAppUsageOption62=	"Broadcast ";
var xmsgAppUsageOption63=	"Commercial Hospitality ";
var xmsgAppUsageOption64=	"Commercial Office ";
var xmsgAppUsageOption65=	"Commercial Retail ";
var xmsgAppUsageOption66=	"Communications ";
var xmsgAppUsageOption67=	"Consumer Products ";
var xmsgAppUsageOption68=	"Design Visualization ";
var xmsgAppUsageOption69=	"Education ";
var xmsgAppUsageOption70=	"Electromechanical ";
var xmsgAppUsageOption71=	"Emergency Response ";
var xmsgAppUsageOption72=	"Environmental ";
var xmsgAppUsageOption73=	"Games ";
var xmsgAppUsageOption74=	"Government ";
var xmsgAppUsageOption75=	"Industrial ";
var xmsgAppUsageOption76=	"Industrial Equipment ";
var xmsgAppUsageOption77=	"Industrial Machinery ";
var xmsgAppUsageOption78=	"Institutional Education College and University ";
var xmsgAppUsageOption79=	"Institutional Education Primary and Secondary ";
var xmsgAppUsageOption80=	"Institutional Health care ";
var xmsgAppUsageOption81=	"Institutional Other ";
var xmsgAppUsageOption82=	"Natural Resources ";
var xmsgAppUsageOption83=	"Oil and Gas ";
var xmsgAppUsageOption84=	"Postproduction ";
var xmsgAppUsageOption85=	"Process and Power ";
var xmsgAppUsageOption86=	"Residential ";
var xmsgAppUsageOption87=	"Telecommunications ";
var xmsgAppUsageOption88=	"Tool and Die ";
var xmsgAppUsageOption89=	"Transportation ";
var xmsgAppUsageOption90=	"Utilities: Water, Gas, Electric ";
var xmsgAppUsageOption91=	"Web and Interactive ";
var xmsgAppUsageOption99=	"Other ";

var xmsgWrongCountryCodeMsg=	"Error: No known country code input!";

/////////////////////////////////////////////
// Version RESOURCES HERE:
/////////////////////////////////////////////
var xmsgVersion = "Version:"

/////////////////////////////////////////////
// Access RTFax or RTMail through RTEmail
/////////////////////////////////////////////
var xmsgAccessFaxMsg=		"Register by fax. ";
var xmsgAccessMailMsg=		"Register by mail.";
var xmsgAccessHereMsg=		"here.";

/////////////////////////////////////////////
// Messages for Ease of reauth or multi-seat client registration
/////////////////////////////////////////////
var xmsgNoDialogHdrMsg=		"Please wait while your license is <br>being verified";

/////////////////////////////////////////////
// Resource for NSA_RTBegin.html
/////////////////////////////////////////////
var xmsgNSARTBeginTitle=	"Obtain A Network License";
var xmsgNSARTBeginMsg1=		"Using the Network License Activation program makes obtaining a network license quick and easy. ";
var xmsgNSARTBeginMsg2=		"Once you complete the process, you have a new or modified %1 license " +
							"that you can save as a new license file or append to an existing license file.<p>";
var xmsgNSARTBeginMsg3=		"The program works through the Internet (by default)*.<p>"+
							"The program will guide you through the following steps:<p>"+
							"Step 1 <b>Information collection</b><br>"+
							"Step 2 <b>Information transmission to Autodesk</b><br>"+
							"Step 3 <b>License(s) returned from Autodesk</b><br>"+
							"Step 4 <b>License(s) saved to new or existing license file(s)</b><br>"+
							"Step 5 <b>Transaction confirmation</b> (you can print details for your records)<p>"+
							"To complete this process you will need:<p>"+
							"<b>License server information</b> - serial number, license server model, server host name, "+
							"number of seats per server (when distributed)<br>" +
							"<b>Registration data</b> - name, address, phone number, etc. (when not pre-registered)<br>"+
							"<b>Previous product serial number</b> (upgrades only)<br>" +
							"<b>A location where license files can be saved</b> (including write access)<br>";
var xmsgNSARTBeginNoInternet=	"*Connecting to the Internet is the quickest way to obtain a license. If you do "+
								"not wish to use the Internet at this time, select this check box. (You can still " +
								"complete the process more quickly using this application and will have a choice of other methods.)";

/////////////////////////////////////////////
// Edit server and seat information
/////////////////////////////////////////////
var xmsgEditServerHdrMsg1=	"Server Information";
var xmsgEditServerHdrMsg2=	"License server model*";
var xmsgEditServerMsg1=		"Enter your license server information. Use the [...] button to locate a server host name.<br>"+
							"Clicking on the Lookup button will populate the Host ID field.<p>";
var xmsgEditServerMsg2=		"Begin with server host data from existing license file(s).";
var xmsgEditServerRemove=	"Remove";
var xmsgEditServerHostName=	"Server host name*";
var xmsgEditServerHostID=	"Host ID*";
var xmsgEditServerSeat=		"Seats*";
var xmsgEditServerSingle=	"Single Server";
var xmsgEditServerDistributed=	"Distributed Server";
var xmsgEditServerRedundant=	"Redundant Server";
var xmsgEditServerDots=		"...";
var xmsgEditServerLookup=	"Lookup";
var xmsgEditServerAddServer=	"Add";
var xmsgEditServerRemoveServer=	"Remove";
var xmsgEditServerTotal=	"Total:";
var xmsgEditServerExceedMaxServer=	"Note: You can license a maximum of 20 distributed servers at one time. If you need to license more than 20 servers, "+
									"contact Autodesk.";
var xmsgEditServerSeatError=	"Seats must contain numeric values only.";

var xmsgConfirmServerMsg=	"Review the following information. Click Next to continue or click Back to make changes.";
var xmsgConfirmServerMsg2=	"When you click Next, this information will be sent to Autodesk over the Internet.";
var xmsgConfirmServerHdr2=	"License server model:";
var xmsgConfirmServerHostName=	"Server host name";
var xmsgConfirmServerHostID=	"Host ID";
var xmsgConfirmServerSeat=		"Seats";
var xmsgConfirmServerTotal=		"Total seats:";

var xmsgLicErrorHdr=			"Licensing Error"; 
var xmsgLicErrorInvalidSN=		"The Serial Number that you entered is not valid. Verify that the serial number displayed "+
								"above is correct. Click one of the following links.";
var xmsgLicReEnterSN=			"Re-enter serial number and try again.";

var xmsgLic113Msg2=				"The total number of seats you have allocated to your distributed servers exceeds that number.";
var xmsgLic113ModifySeats=		"Modify seat distribution.";

var xmsgSaveLicenseHdr=			"License(s) Received";
var xmsgSaveLicenseMsg1=		"Your licenses have been received from Autodesk. You must now save them in license files.<p> " +
								"<b>Save License(s) in License Files</b><p>" +
								"<LI>The <b>recommended location</b> for your license file(s) is in a \"License\" directory " +
								"where you installed the Autodesk Network License Manager on your server.&nbsp;&nbsp;";
var xmsgSaveLicenseMsg6=		"<LI>If you have an <b>existing license file</b>, <b>insert</b> the new license into it.<br>" +
								"When you click Next, the system will automatically insert your license information to " +
								"the appropriate location in the existing file.</LI>";
var xmsgSaveLicenseMsg2=		"If the file specified above is an existing license file:";
var xmsgSaveLicenseMsg3=		"Insert the new license information into it.";
var xmsgSaveLicenseMsg4=		"Overwrite the existing license file.";
var xmsgSaveLicenseMsg5=		"Please correct the following errors:";

var xmsgSaveErrorNoPermission=	"ERROR: You do not have permission to save the file to the location specified.<br>" +
								"You can save the file to a temporary location, obtain the appropriate permission, and move the file later.";
var xmsgSaveErrorNoDiskSpace=	"ERROR: You do not have enough disk space available to save the file to the location " +
								"specified.<BR>You can save the file to a temporary location, make space available, and move the file later.";
var xmsgSaveErrorInvalidFile=	"ERROR: The new license could not be appended to the file specified without losing " +
								"data. The file specified may have been for different machines or different server models.<BR>" +
								"You can save the new license file with a different name or location and append it to the file manually later.";															
var xmsgSaveErrorPathNotExit=	"ERROR: The specified path does not exist.<BR>Click the Browse button to locate or create the path.";
var xmsgSaveErrorPathBlank=		"ERROR: You must specify a valid path and license file name. Specify the path and " +
								"file name in the field below or browse to locate one.";

var xmsgLicAuthConfirmHdr1=		"License Activation Successful!";
var xmsgLicAuthConfirmHdr2=		"License Server Host Information";
var xmsgLicAuthConfirmMsg1=		"Thank you for registering your product. You can print a record of this information.";
var xmsgLicAuthConfirmDate=		"Date:";

var xmsgLicPathMsg1=			"The following license files have been saved:";
var xmsgLicPathMsg2=			"Your license files have been saved to ";

var xmsgOtherMethodMsg1NSA=		"Use another method to obtain an authorized license.";
var xmsgOtherMethodMsg1=		"Use another method.";

var xmsgContactAutodeskMsg1=	"Contact Autodesk using one of the following methods:";
								
/////////////////////////////////////////////
// Email Options
/////////////////////////////////////////////
var xmsgEmailOptionNSATitle=	"Obtain a License File by Email";
var xmsgEmailOptionTitle=		"Register by Email";
var xmsgEmailOptionNSAMsg=		"Your registration and license server information has been automatically saved " +
								"to a file. The file must be emailed to Autodesk’s automated registration processing " +
								"center.<p>When you successfully email the file to Autodesk, you will receive an " +
								"automated response, an authorized license file, and instructions on how to complete " +
								"the process.";
var xmsgEmailOptionMsg=			"Your registration information has been automatically saved to a file. The file must " +
								"be emailed to Autodesk’s automated registration processing center.<p>When you successfully email " +
								"the file to Autodesk, you will receive an automated response, and instructions on how " +
								"to complete the process.";
var xmsgEmailOptionMsg2=		"If the default email system you use is currently able to send email over the Internet, " +
								"click Send File Now and then click Next. The file will be sent for you automatically.<p>" +
								"If you do not have access to send email now, click Send File Later. Instructions for " +
								"sending the file manually will be provided.";
var xmsgEmailOptionInfo1=		"Send the file now.";
var xmsgEmailOptionInfo2=		"Send the file later, manually.";
var xmsgEmailOptionInfo3=		"Send a copy of the email to the email address I provided.";

/////////////////////////////////////////////
// Local HERE customer information URLs
/////////////////////////////////////////////
var xmsgLocalHERETitle=			"Change Customer Information";
var xmsgLocalHEREInfo=			"To change your customer information, contact your Autodesk Authorized Reseller " +
								"or navigate to a link below that applies to your country or region.";
var xmsgLocalHEREUS=			"Americas";
var xmsgLocalHEREEMEA=			"Europe, Middle East, Africa";
var xmsgLocalHEREAPAC=			"Asia/Pacific";

/////////////////////////////////////////////
// Help
/////////////////////////////////////////////
var xmsgHelpTitle=				"Help";
var xmsgHelpUpgradeMsg=			"If you are registering or activating an upgrade from a previous version, you must "+
								"check this box, and you will be prompted for the serial number from the version "+
								"from which you are upgrading.";
var xmsgHelpUpgradeRequiredTitle=	"Enter the previous product serial number (required for upgrade)";
var xmsgHelpUpgradeRequiredMsg=	"This is the serial number from your previous software version. You can find the "+
								"serial number either in the Help > About dialog box in the software or on the "+
								"previous product packaging.";

//	Fax or email error message
var xmsgNotEnterErrorMsg=		"You have not entered %1. Please use the Back button to go back and "+
								"enter this if you wish to receive your activation by this method.";								

/////////////////////////////////////////////
// Email Error
/////////////////////////////////////////////
var xmsgEmailErrorHdr=			"Unable to create email";
var xmsgEmailErrorMsg1=			"To complete registration by email, attach the following file to an email:";
var xmsgEmailErrorMsg2=			"Send the email to %1.";
var xmsgEmailErrorMsg3=			"You can find the file in the following location:";

/////////////////////////////////////////////
// Error 12
/////////////////////////////////////////////
var xmsgErr12Hdr=				"The serial number you entered is invalid";
var xmsgErr12APAC=				"<p>This is an invalid serial number and cannot be activated. " +
								"Please contact us at " +
								"<a href=mailto:apacpl@listproc.autodesk.com target=\"_blank\">apacpl@listproc.autodesk.com</a></p>";
var xmsgErr12EMEA=				"<p>Dear Sir/Madam</p>" +
								"<p>Thank you for trying to register your Autodesk&#0174; or Discreet&#0153; software product with Autodesk.</p>" +
								"<p>Unfortunately, our records indicate that the serial number for the Autodesk&#0174; or Discreet&#0153; "+
								"software product you have tried to register with us is not a valid serial number or is the serial "+
								"number of a well known pirate copy of that software product and therefore the Autodesk&#0174; or " +
								"Discreet&#0153; software product you are using is unauthorized.</p>" +
								"<p>Licenses to use Autodesk&#0174; software products can be purchased from Autodesk Authorized " +
								"Resellers – please visit " +
								"<a href=\"http://www.autodesk.com\" target=\"_blank\">www.autodesk.com</a> " +
								"for a list of Autodesk Authorized Resellers or an " +
								"Autodesk office in your area. Use of legal software is the only way in which you can ensure that " +
								"you receive the maximum benefit from using your software AND avoid potential civil and " +
								"possibly criminal prosecution if caught using and/or reproducing software illegally.</p>" +
								"<p>Licenses to use Discreet&#0153; software products can be purchased from Discreet Authorized " +
								"Resellers – please visit " +
								"<a href=\"http://www.discreet.com\" target=\"_blank\">www.discreet.com</a> " +
								"for a list of Discreet Authorized Resellers or a " +
								"Discreet office in your area. Use of legal software is the only way in which you can ensure that " +
								"you receive the maximum benefit from using your software AND avoid potential civil and " +
								"possibly criminal prosecution if caught using and/or reproducing software illegally.</p>" +
								"<p>So we kindly request that you  immediately stop using the unauthorized software product you " +
								"have tried to register with us, remove all copies stored on any of your computer hardware, and " +
								"destroy any media containing the software product. While we trust you will voluntarily comply " +
								"with our request, Autodesk reserves its rights in relation to this matter and may pass your details " +
								"to the Business Software Alliance (“BSA”).</p>" +
								"<p>If you have any questions or believe that you have received this message in error, please contact " +
								"us at: " +
								"<a href=mailto:Authcodes.neu@autodesk.com target=\"_blank\">Authcodes.neu@autodesk.com</a></p>" +
								"<p>If you would like to provide information regarding the use or sale of illegal software, please " +
								"contact your local Autodesk office or contact the BSA by visiting " +
								"<a href=\"http://www.BSA.org\" target=\"_blank\">www.BSA.org</a></p>" +
								"<p>Thank you</p>";
var xmsgErr12US=				"<p>Dear Sir/Madam</p>" +
								"<p>You recently tried to register your software product with Autodesk or Discreet.</p>" +
								"<p>Unfortunately, our records indicate that the serial number for the Autodesk&#0174; or Discreet&#0153; " +
								"software product you have tried to register with us is not a valid serial number or is the serial " +
								"number of a well known pirate copy of that software product and therefore the Autodesk&#0174; or " +
								"Discreet&#0153; software product you are using is unauthorized.</p>" +
								"<p>For Autodesk users, licenses to use Autodesk&#0174; software products can be purchased from " +
								"Autodesk Authorized Resellers – please visit " +
								"<a href=\"http://www.autodesk.com\" target=\"_blank\">www.autodesk.com</a> " +
								"for a list of Autodesk " +
								"Authorized Resellers in your area.  Use of legal software is the only way in which you can " +
								"ensure that you receive the maximum benefit from using your software AND avoid potential " +
								"civil and possibly criminal prosecution if caught using and/or reproducing software illegally.</p>" +
								"<p>For Discreet&#0153; users, licenses to use Discreet&#0153; software products can be purchased from " +
								"Discreet Authorised Resellers – please visit " +
								"<a href=\"http://www.discreet.com\" target=\"_blank\">www.discreet.com</a> " +
								"for a list of Discreet Authorized " +
								"Resellers or a Discreet office in your area. Use of legal software is the only way in which you " +
								"can ensure that you receive the maximum benefit from using your software AND avoid potential " +
								"civil and possibly criminal prosecution if caught using and/or reproducing software illegally.</p>" +
								"<p>You must immediately stop using the unauthorized software product you have tried to register " +
								"with us, remove all copies stored on any of your computer hardware, and destroy any media " +
								"containing the software product. While we trust you will voluntarily comply with our request, " +
								"Autodesk reserves its rights in relation to this matter and may pass your details to the Business " +
								"Software Alliance (“BSA”).</p>" +
								"<p>If you have any questions or believe that you have received this message in error, please contact " +
								"us at: " +
								"<a href=mailto:ProductActivation@activation.autodesk.com target=\"_blank\">ProductActivation@activation.autodesk.com</a></p>" +
								"<p>If you would like to provide information regarding the use or sale of illegal software, please " +
								"contact your local office or contact the BSA by visiting " +
								"<a href=\"http://www.BSA.org\" target=\"_blank\">www.BSA.org</a></p>" +
								"<p>Thank you</p>";







// SIG // Begin signature block
// SIG // MIIWVgYJKoZIhvcNAQcCoIIWRzCCFkMCAQExDjAMBggq
// SIG // hkiG9w0CBQUAMGYGCisGAQQBgjcCAQSgWDBWMDIGCisG
// SIG // AQQBgjcCAR4wJAIBAQQQEODJBs441BGiowAQS9NQkAIB
// SIG // AAIBAAIBAAIBAAIBADAgMAwGCCqGSIb3DQIFBQAEEH/3
// SIG // Ho8SzKPJLnG/qBb2agCgghGXMIIDxDCCAy2gAwIBAgIQ
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
// SIG // 9w0BCQQxEgQQSEhGBqQEaqlGB2Z6O6clgzBUBgorBgEE
// SIG // AYI3AgEMMUYwRKAmgCQAQQB1AHQAbwBkAGUAcwBrACAA
// SIG // QwBvAG0AcABvAG4AZQBuAHShGoAYaHR0cDovL3d3dy5h
// SIG // dXRvZGVzay5jb20gMA0GCSqGSIb3DQEBAQUABIGAXFOY
// SIG // UZsdIeKSXn1ri0KazwZLyhzld9ZLUlKi9NSMupjfjd9z
// SIG // aySB6sfsYHCWUlHYn/n7UbgeN429mCuQ/9FLsmOHmdCt
// SIG // G/BGVEFDqRXkK3PHzheK7ugLwscHASuDKKczvdH1jyy/
// SIG // exXHBDPCBagnGUFPSK+xBA2/W79SuqApRoqhggH/MIIB
// SIG // +wYJKoZIhvcNAQkGMYIB7DCCAegCAQEwZzBTMQswCQYD
// SIG // VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4x
// SIG // KzApBgNVBAMTIlZlcmlTaWduIFRpbWUgU3RhbXBpbmcg
// SIG // U2VydmljZXMgQ0ECEA3pK/DU2CmIGDIFCV6adogwDAYI
// SIG // KoZIhvcNAgUFAKBZMBgGCSqGSIb3DQEJAzELBgkqhkiG
// SIG // 9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTA2MDIxMjE3NDgx
// SIG // M1owHwYJKoZIhvcNAQkEMRIEEOIMmO6cq88P88LEu+cr
// SIG // NTwwDQYJKoZIhvcNAQEBBQAEggEASBLq0ah6h67isv1E
// SIG // U0YGUwGGNs2rKDzeBTfYreS90ujGU/98O+j5yp7Agy3p
// SIG // cAWGUrsdt0yfouHaSEUyoqPHvb/vAsIhPMnLVKiU0oT5
// SIG // 28QUo3psEgyfjFdc2Vp0aF5eoEdC1rnz3cmr0ltylvAI
// SIG // vhiV9B9x4LPj6EqfNPHpRhBwDIzK5nOeVR9ZXUKblCio
// SIG // z2Yzl3XuBouOdTlEo67mTU65tAXjd9c6IdLVEH8ccRtN
// SIG // KSw23is6DyuutbG9Ey3iXAl9pAEG1rRMYjankgQS2Ezv
// SIG // N3oxBcyiUfh/xo80CyNtPvBySNK16xsYa1gX1no2WKWr
// SIG // UWgX0VX4uvGSwlImRg==
// SIG // End signature block
