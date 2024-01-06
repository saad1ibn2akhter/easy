//=======================================================================
// State, Region, and Province Information
//=======================================================================

// Country State/Province Info
var ATStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="B">Burgenland </option>' +
						'<option value="K">Kärnten </option>' +
						'<option value="NÖ">Niederösterreich </option>' +
						'<option value="OÖ">Oberösterreich </option>' +
						'<option value="S">Salzburg </option>' +
						'<option value="ST">Steiermark </option>' +
						'<option value="T">Tirol </option>' +
						'<option value="V">Vorarlberg </option>' +
						'<option value="W">Wien </option>' +
						'</select>';

var AUStPrRegHTML = '	<br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="ACT">Aust Capital Terr </option>' +
						'<option value="NSW">New South Wales </option>' +
						'<option value="NT">Northern Territory </option>' +
						'<option value="QLD">Queensland </option>' +
						'<option value="SA">South Australia </option>' +
						'<option value="TAS">Tasmania </option>' +
						'<option value="VIC">Victoria </option>' +
						'<option value="WA">Western Australia </option>' +
						'</select>';

var USStPrRegHTML = '	<br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AL">Alabama </option>' +
						'<option value="AK">Alaska </option>' +
						'<option value="AZ">Arizona </option>' +
						'<option value="AR">Arkansas </option>' +
						'<option value="CA">California </option>' +
						'<option value="CO">Colorado </option>' +
						'<option value="CT">Connecticut </option>' +
						'<option value="DE">Delaware </option>' +
						'<option value="DC">District of Columbia </option>' +
						'<option value="FL">Florida </option>' +
						'<option value="GA">Georgia </option>' +
						'<option value="HI">Hawaii </option>' +
						'<option value="ID">Idaho </option>' +
						'<option value="IL">Illinois </option>' +
						'<option value="IN">Indiana </option>' +
						'<option value="IA">Iowa </option>' +
						'<option value="KS">Kansas </option>' +
						'<option value="KY">Kentucky </option>' +
						'<option value="LA">Louisiana </option>' +
						'<option value="ME">Maine </option>' +
						'<option value="MD">Maryland </option>' +
						'<option value="MA">Massachusetts </option>' +
						'<option value="MI">Michigan </option>' +
						'<option value="MN">Minnesota </option>' +
						'<option value="MS">Mississippi </option>' +
						'<option value="MO">Missouri </option>' +
						'<option value="MT">Montana </option>' +
						'<option value="NE">Nebraska </option>' +
						'<option value="NV">Nevada </option>' +
						'<option value="NH">New Hampshire </option>' +
						'<option value="NJ">New Jersey </option>' +
						'<option value="NM">New Mexico </option>' +
						'<option value="NY">New York </option>' +
						'<option value="NC">North Carolina </option>' +
						'<option value="ND">North Dakota </option>' +
						'<option value="OH">Ohio </option>' +
						'<option value="OK">Oklahoma </option>' +
						'<option value="OR">Oregon </option>' +
						'<option value="PA">Pennsylvania </option>' +
						'<option value="RI">Rhode Island </option>' +
						'<option value="SC">South Carolina </option>' +
						'<option value="SD">South Dakota </option>' +
						'<option value="TN">Tennessee </option>' +
						'<option value="TX">Texas </option>' +
						'<option value="UT">Utah </option>' +
						'<option value="VT">Vermont </option>' +
						'<option value="VA">Virginia </option>' +
						'<option value="VI">Virgin Islands</option>' +
						'<option value="WA">Washington </option>' +
						'<option value="WV">West Virginia </option>' +
						'<option value="WI">Wisconsin </option>' +
						'<option value="WY">Wyoming </option>' +
						'<option value="AA">APOAmerica</option>' +
						'<option value="AE">APOEurope</option>' +
						'<option value="AP">APOPacific</option>' +
						'</select>';

var BRStPrRegHTML = '	<br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AC">Acre </option>' +	
						'<option value="AL">Alagoas </option>' +	
						'<option value="AP">Amapa </option>' +	
						'<option value="AM">Amazonas </option>' +	
						'<option value="BA">Bahia </option>' +	
						'<option value="CE">Ceara </option>' +	
						'<option value="DF">Distrito Federal </option>' +	
						'<option value="ES">Espirito Santo </option>' +	
						'<option value="GO">Goias </option>' +	
						'<option value="MA">Maranhao </option>' +	
						'<option value="MT">Mato Grosso </option>' +	
						'<option value="MG">Minas Gerais </option>' +	
						'<option value="PA">Para </option>' +	
						'<option value="PB">Paraiba </option>' +	
						'<option value="PR">Parana </option>' +	
						'<option value="PE">Pernambuco </option>' +	
						'<option value="PI">Piaui </option>' +	
						'<option value="RJ">Rio de Janeiro </option>' +	
						'<option value="RN">Rio Grande do Norte </option>' +	
						'<option value="RS">Rio Grande do Sul </option>' +	
						'<option value="RO">Rondonia </option>' +	
						'<option value="RR">Roraima </option>' +	
						'<option value="SC">Santa Catarina </option>' +	
						'<option value="SP">Sao Paulo </option>' +	
						'<option value="SE">Sergipe </option>' +	
						'<option value="TO">Tocantins </option>' +	
						'</select>';
						
var CAStPrRegHTML = '	<br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AB">Alberta </option>' +
						'<option value="BC">British Columbia </option>' +
						'<option value="MB">Manitoba </option>' +
						'<option value="NB">New Brunswick </option>' +
						'<option value="NF">Newfoundland </option>' +
						'<option value="NT">Northwest Territories </option>' +
						'<option value="NS">Nova Scotia </option>' +
						'<option value="NU">Nunavut </option>' +
						'<option value="ON">Ontario </option>' +
						'<option value="PE">Prince Edward Island </option>' +
						'<option value="QC">Quebec </option>' +
						'<option value="SK">Saskatchewan </option>' +
						'<option value="YT">Yukon </option>' +
						'</select>';

var CHStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AG">Aargau </option>' +
						'<option value="AI">Appenzell Innerrhoden </option>' +
						'<option value="AR">Appenzell Ausserrhoden </option>' +
						'<option value="BE">Bern </option>' +
						'<option value="BL">Basel-Landschaft </option>' +
						'<option value="BS">Basel-Stadt </option>' +
						'<option value="FR">Fribourg </option>' +
						'<option value="GE">Genève </option>' +
						'<option value="GL">Glarus </option>' +
						'<option value="GR">Graubüenden </option>' +
						'<option value="JU">Jura </option>' +
						'<option value="LU">Luzern </option>' +
						'<option value="NE">Neuchâtel </option>' +
						'<option value="NW">Nidwalden </option>' +
						'<option value="OW">Obwalden </option>' +
						'<option value="SG">St.Gallen </option>' +
						'<option value="SH">Schaffhausen </option>' +
						'<option value="SO">Solothurn </option>' +
						'<option value="SZ">Schwyz </option>' +
						'<option value="TG">Thurgau </option>' +
						'<option value="TI">Ticino </option>' +
						'<option value="UR">Uri </option>' +
						'<option value="VD">Vaud </option>' +
						'<option value="VS">Valais </option>' +
						'<option value="ZG">Zug </option>' +
						'<option value="ZH">Zürich </option>' +
						'</select>';

var CNStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="110">Anhui </option>' +
						'<option value="010">Beijing </option>' +
						'<option value="320">Chongqing </option>' +
						'<option value="150">Fujian </option>' +
						'<option value="260">Gansu </option>' +
						'<option value="190">Guangdong </option>' +
						'<option value="210">Guangxi </option>' +
						'<option value="220">Guizhou </option>' +
						'<option value="200">Hainan </option>' +
						'<option value="060">Hebei </option>' +
						'<option value="090">Heilongjiang </option>' +
						'<option value="180">Henan </option>' +
						'<option value="170">Hubei </option>' +
						'<option value="160">Hunan </option>' +
						'<option value="100">Jiangsu </option>' +
						'<option value="140">Jiangxi </option>' +
						'<option value="080">Jilin </option>' +
						'<option value="070">Liaoning </option>' +
						'<option value="040">NeiMongol </option>' +
						'<option value="270">Ningxia </option>' +
						'<option value="280">Qinghai </option>' +
						'<option value="250">Shaanxi </option>' +
						'<option value="120">Shandong </option>' +
						'<option value="020">Shanghai </option>' +
						'<option value="050">Shanxi </option>' +
						'<option value="230">Sichuan </option>' +
						'<option value="030">Tianjin </option>' +
						'<option value="290">Xinjiang </option>' +
						'<option value="300">Xizang </option>' +
						'<option value="240">Yunnan </option>' +
						'<option value="130">Zhejiang </option>' +
						'</select>';

var DEStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="08">Baden-Württemberg </option>' +
						'<option value="09">Bayern </option>' +
						'<option value="11">Berlin </option>' +
						'<option value="12">Brandenburg </option>' +
						'<option value="04">Bremen </option>' +
						'<option value="02">Hamburg </option>' +
						'<option value="06">Hessen </option>' +
						'<option value="13">Mecklenburg-Vorpommern </option>' +
						'<option value="03">Niedersachsen </option>' +
						'<option value="05">Nordrhein-Westfalen </option>' +
						'<option value="07">Rheinland-Pfalz </option>' +
						'<option value="10">Saarland </option>' +
						'<option value="14">Sachsen </option>' +
						'<option value="15">Sachsen-Anhalt</option>' +
						'<option value="01">Schleswig-Holstein </option>' +
						'<option value="16">Thüringen </option>' +
						'</select>';

var ESStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="01">Álava </option>' +
						'<option value="02">Albacete </option>' +
						'<option value="03">Alicante </option>' +
						'<option value="04">Almería </option>' +
						'<option value="33">Asturias </option>' +
						'<option value="05">Ávila </option>' +
						'<option value="06">Badajoz </option>' +
						'<option value="07">Baleares </option>' +
						'<option value="08">Barcelona </option>' +
						'<option value="09">Burgos </option>' +
						'<option value="10">Cáceres </option>' +
						'<option value="11">Cádiz </option>' +
						'<option value="39">Cantabria </option>' +
						'<option value="12">Castellón </option>' +
						'<option value="13">CiudadReal </option>' +
						'<option value="14">Córdoba </option>' +
						'<option value="16">Cuenca </option>' +
						'<option value="17">Gerona </option>' +
						'<option value="18">Granada </option>' +
						'<option value="19">Guadalajara </option>' +
						'<option value="20">Guipúzcoa </option>' +
						'<option value="21">Huelva </option>' +
						'<option value="22">Huesca </option>' +
						'<option value="23">Jaén </option>' +
						'<option value="15">LaCoruña </option>' +
						'<option value="26">LaRioja </option>' +
						'<option value="35">LasPalmas </option>' +
						'<option value="24">León </option>' +
						'<option value="25">Lérida </option>' +
						'<option value="27">Lugo </option>' +
						'<option value="28">Madrid </option>' +
						'<option value="29">Málaga </option>' +
						'<option value="30">Murcia </option>' +
						'<option value="31">Navarra </option>' +
						'<option value="32">Orense </option>' +
						'<option value="34">Palencia </option>' +
						'<option value="36">Pontevedra </option>' +
						'<option value="37">Salamanca </option>' +
						'<option value="38">Santa Cruz de Tenerife </option>' +
						'<option value="40">Segovia </option>' +
						'<option value="41">Sevilla </option>' +
						'<option value="42">Soria </option>' +
						'<option value="43">Tarragona </option>' +
						'<option value="44">Teruel </option>' +
						'<option value="45">Toledo </option>' +
						'<option value="46">Valencia </option>' +
						'<option value="47">Valladolid </option>' +
						'<option value="48">Vizcaya </option>' +
						'<option value="49">Zamora </option>' +
						'<option value="50">Zaragoza </option>' +
						'</select>';

var HKStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="HK">Hong Kong Island </option>' +
						'<option value="KLN">Kowloon </option>' +
						'<option value="NT">New Territories </option>' +
						'</select>';

var IEStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="CK">Cork </option>' +
						'<option value="CL">Clare </option>' +
						'<option value="CW">Carlow </option>' +
						'<option value="DB">Dublin </option>' +
						'<option value="DG">Donegal </option>' +
						'<option value="GW">Galway </option>' +
						'<option value="KD">Kildare </option>' +
						'<option value="KK">Kilkenny </option>' +
						'<option value="KV">Cavan </option>' +
						'<option value="KY">Kerry </option>' +
						'<option value="LF">Longford </option>' +
						'<option value="LI">Limerick </option>' +
						'<option value="LM">Leitrim </option>' +
						'<option value="LS">Laois </option>' +
						'<option value="LT">Louth </option>' +
						'<option value="MH">Monaghan </option>' +
						'<option value="MT">Meath </option>' +
						'<option value="MY">Mayo </option>' +
						'<option value="OS">Offaly </option>' +
						'<option value="RC">Rosscommon </option>' +
						'<option value="SG">Sligo </option>' +
						'<option value="TP">Tipperary </option>' +
						'<option value="WF">Waterford </option>' +
						'<option value="WK">Wicklow </option>' +
						'<option value="WM">Westmeath </option>' +
						'<option value="WX">Wexford </option>' +
						'</select>';

var ITStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AG">Agrigento </option>' +
						'<option value="AL">Alessandria </option>' +
						'<option value="AN">Ancona </option>' +
						'<option value="AO">Aosta </option>' +
						'<option value="AR">Arezzo </option>' +
						'<option value="AP">Ascoli Piceno </option>' +
						'<option value="AT">Asti </option>' +
						'<option value="AV">Avellino </option>' +
						'<option value="BA">Bari </option>' +
						'<option value="BL">Belluno </option>' +
						'<option value="BN">Benevento </option>' +
						'<option value="BG">Bergamo </option>' +
						'<option value="BI">Biella </option>' +
						'<option value="BO">Bologna </option>' +
						'<option value="BZ">Bolzano </option>' +
						'<option value="BS">Brescia </option>' +
						'<option value="BR">Brindisi </option>' +
						'<option value="CA">Cagliari </option>' +
						'<option value="CL">Caltanissetta </option>' +
						'<option value="CB">Campobasso </option>' +
						'<option value="CE">Caserta </option>' +
						'<option value="CT">Catania </option>' +
						'<option value="CZ">Catanzaro </option>' +
						'<option value="CH">Chieti </option>' +
						'<option value="CO">Como </option>' +
						'<option value="CS">Cosenza </option>' +
						'<option value="CR">Cremona </option>' +
						'<option value="KR">Crotone </option>' +
						'<option value="CN">Cuneo </option>' +
						'<option value="EN">Enna </option>' +
						'<option value="FE">Ferrara </option>' +
						'<option value="FI">Firenze </option>' +
						'<option value="FG">Foggia </option>' +
						'<option value="FO">Forlì </option>' +
						'<option value="FR">Frosinone </option>' +
						'<option value="GE">Genova </option>' +
						'<option value="GO">Gorizia </option>' +
						'<option value="GR">Grosseto </option>' +
						'<option value="IM">Imperia </option>' +
						'<option value="IS">Isernia </option>' +
						'<option value="AQ">L`Aquila </option>' +
						'<option value="SP">LaSpezia </option>' +
						'<option value="LT">Latina </option>' +
						'<option value="LE">Lecce </option>' +
						'<option value="LC">Lecco </option>' +
						'<option value="LI">Livorno </option>' +
						'<option value="LO">Lodi </option>' +
						'<option value="LU">Lucca </option>' +
						'<option value="MC">Macerata </option>' +
						'<option value="MN">Mantova </option>' +
						'<option value="MS">Massa Carrara </option>' +
						'<option value="MT">Matera </option>' +
						'<option value="ME">Messina </option>' +
						'<option value="MI">Milano </option>' +
						'<option value="MO">Modena </option>' +
						'<option value="NA">Napoli </option>' +
						'<option value="NO">Novara </option>' +
						'<option value="NU">Nuoro </option>' +
						'<option value="OR">Oristano </option>' +
						'<option value="PD">Padova </option>' +
						'<option value="PA">Palermo </option>' +
						'<option value="PR">Parma </option>' +
						'<option value="PV">Pavia </option>' +
						'<option value="PG">Perugia </option>' +
						'<option value="PS">Pesaro e Urbino </option>' +
						'<option value="PE">Pescara </option>' +
						'<option value="PC">Piacenza </option>' +
						'<option value="PI">Pisa </option>' +
						'<option value="PT">Pistoia </option>' +
						'<option value="PN">Pordenone </option>' +
						'<option value="PZ">Potenza </option>' +
						'<option value="PO">Prato </option>' +
						'<option value="RG">Ragusa </option>' +
						'<option value="RA">Ravenna </option>' +
						'<option value="RC">Reggio Calabria </option>' +
						'<option value="RE">Reggio Emilia </option>' +
						'<option value="RI">Rieti </option>' +
						'<option value="RN">Rimini </option>' +
						'<option value="RM">Roma </option>' +
						'<option value="RO">Rovigo </option>' +
						'<option value="SA">Salerno </option>' +
						'<option value="SS">Sassari </option>' +
						'<option value="SV">Savona </option>' +
						'<option value="SI">Siena </option>' +
						'<option value="SR">Siracusa </option>' +
						'<option value="SO">Sondrio </option>' +
						'<option value="TA">Taranto </option>' +
						'<option value="TE">Teramo </option>' +
						'<option value="TR">Terni </option>' +
						'<option value="TP">Trapani </option>' +
						'<option value="TN">Trento </option>' +
						'<option value="TV">Treviso </option>' +
						'<option value="TS">Trieste </option>' +
						'<option value="TO">Torino </option>' +
						'<option value="UD">Udine </option>' +
						'<option value="VA">Varese </option>' +
						'<option value="VE">Venice </option>' +
						'<option value="VB">Verbano-Cusio-Ossola </option>' +
						'<option value="VC">Vercelli </option>' +
						'<option value="VR">Verona </option>' +
						'<option value="VV">Vibo Valentia </option>' +
						'<option value="VI">Vicenza </option>' +
						'<option value="VT">Viterbo </option>' +
						'</select>';


var JPStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +					
						'<option value="01">Hokkaido </option>' +
						'<option value="02">Aomori </option>' +
						'<option value="03">Iwate </option>' +
						'<option value="04">Miyagi </option>' +
						'<option value="05">Akita </option>' +
						'<option value="06">Yamagata </option>' +
						'<option value="07">Fukushima </option>' +
						'<option value="08">Ibaraki </option>' +
						'<option value="09">Tochigi </option>' +
						'<option value="10">Gunma </option>' +
						'<option value="11">Saitama </option>' +
						'<option value="12">Chiba </option>' +
						'<option value="13">Tokyo </option>' +
						'<option value="14">Kanagawa </option>' +
						'<option value="15">Niigata </option>' +
						'<option value="16">Toyama </option>' +
						'<option value="17">Ishikawa </option>' +
						'<option value="18">Fukui </option>' +
						'<option value="19">Yamanashi </option>' +
						'<option value="42">Nagano </option>' +
						'<option value="21">Gifu </option>' +
						'<option value="22">Shizuoka </option>' +
						'<option value="23">Aichi </option>' +
						'<option value="24">Mie </option>' +
						'<option value="25">Shiga </option>' +
						'<option value="26">Kyoto </option>' +
						'<option value="27">Osaka </option>' +
						'<option value="28">Hyogo </option>' +
						'<option value="29">Nara </option>' +
						'<option value="30">Wakayama </option>' +
						'<option value="31">Tottori </option>' +
						'<option value="32">Shimane </option>' +
						'<option value="33">Okayama </option>' +
						'<option value="34">Hiroshima </option>' +
						'<option value="35">Yamaguchi </option>' +
						'<option value="36">Tokushima </option>' +
						'<option value="37">Kagawa </option>' +
						'<option value="38">Ehime </option>' +
						'<option value="39">Kochi </option>' +
						'<option value="40">Fukuoka </option>' +
						'<option value="41">Saga </option>' +
						'<option value="20">Nagasaki </option>' +
						'<option value="43">Kumamoto </option>' +
						'<option value="44">Oita </option>' +
						'<option value="45">Miyazaki </option>' +
						'<option value="46">Kagoshima </option>' +
						'<option value="47">Okinawa </option>' +
						'</select>';

var MXStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="AGS">Aguascalientes </option>' +
						'<option value="BCN">Baja CaliforniaN </option>' +
						'<option value="BCS">Baja CaliforniaS </option>' +
						'<option value="CMP">Campeche </option>' +
						'<option value="CHS">Chiapas </option>' +
						'<option value="CHI">Chihuahua </option>' +
						'<option value="COA">Coahuila </option>' +
						'<option value="COL">Colima </option>' +
						'<option value="DF">Distrito Federal </option>' +
						'<option value="DGO">Durango </option>' +
						'<option value="MEX">Estado de México </option>' +
						'<option value="GTO">Guanajuato </option>' +
						'<option value="GRO">Guerrero </option>' +
						'<option value="HGO">Hidalgo </option>' +
						'<option value="JAL">Jalisco </option>' +
						'<option value="MCH">Michoacán </option>' +
						'<option value="MOR">Morelos </option>' +
						'<option value="NL">NuevoLéon </option>' +
						'<option value="OAX">Oaxaca </option>' +
						'<option value="PUE">Puebla </option>' +
						'<option value="QRO">Querétaro </option>' +
						'<option value="QR">QuintanaRoo </option>' +
						'<option value="SLP">San Luis Potosí </option>' +
						'<option value="SIN">Sinaloa </option>' +
						'<option value="SON">Sonora </option>' +
						'<option value="TAB">Tabasco </option>' +
						'<option value="TMS">Tamaulipas </option>' +
						'<option value="TLX">Tlaxcala </option>' +
						'<option value="VER">Veracruz </option>' +
						'<option value="YUC">Yucatán </option>' +
						'<option value="ZAC">Zacatecas </option>' +
						'</select>';

var MYStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="JOH">Johor </option>' +
						'<option value="KED">Kedah </option>' +
						'<option value="KEL">Kelantan </option>' +
						'<option value="MEL">Melaka </option>' +
						'<option value="PAH">Pahang </option>' +
						'<option value="PEL">Perlis </option>' +
						'<option value="PER">Perak </option>' +
						'<option value="PIN">PulauPinang </option>' +
						'<option value="SAB">Sabah </option>' +
						'<option value="SAR">Sarawak </option>' +
						'<option value="SEL">Selangor </option>' +
						'<option value="SER">NegeriSembilan </option>' +
						'<option value="TRE">Terengganu </option>' +
						'</select>';

var PTStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="40">Alentejo </option>' +
						'<option value="50">Algarve </option>' +
						'<option value="24">Beira Interior </option>' +
						'<option value="15">Beira Litoral </option>' +
						'<option value="10">Entre Douro e Minho </option>' +
						'<option value="22">Estremadura e Ribatejo </option>' +
						'<option value="31">Lisboa e Setúbal </option>' +
						'<option value="70">Reg. Aut. da Madeira </option>' +
						'<option value="60">Reg. Aut. dos Açores </option>' +
						'<option value="16">Trás-os-Montes e Alto Douro </option>' +
						'</select>';

var TWStPrRegHTML = ' <br><select name="state" size="1">' +
						'<option value="select">'+xmsgPleaseSelect+'</option>' +
						'<option value="KSH">Kaoshung City </option>' +
						'<option value="TPE">Taipei City </option>' +
						'<option value="TWN">Taiwan Prov. </option>' +
						'</select>';


//=======================================================================
// Country Long Name Information
//=======================================================================

var CountryCollection = 
				'<option selected value="select">'+xmsgPleaseSelect+'</option>' +
				'<option value="US">United States </option>' +
				'<option value="AF">Afghanistan </option>' +
				'<option value="AL">Albania </option>' +
				'<option value="DZ">Algeria </option>' +
				'<option value="AS">American Samoa </option>' +
				'<option value="AD">Andorra </option>' +
				'<option value="AO">Angola </option>' +
				'<option value="AI">Anguilla </option>' +
				'<option value="AG">Antigua and Barbuda </option>' +
				'<option value="AR">Argentina </option>' +
				'<option value="AM">Armenia </option>' +
				'<option value="AW">Aruba </option>' +
				'<option value="AU">Australia </option>' +
				'<option value="AT">Austria </option>' +
				'<option value="AZ">Azerbaijan </option>' +
				'<option value="BS">Bahamas </option>' +
				'<option value="BH">Bahrain </option>' +
				'<option value="BD">Bangladesh </option>' +
				'<option value="BB">Barbados </option>' +
				'<option value="BY">Belarus </option>' +
				'<option value="BE">Belgium </option>' +
				'<option value="BZ">Belize </option>' +
				'<option value="BJ">Benin </option>' +
				'<option value="BM">Bermuda </option>' +
				'<option value="BT">Bhutan </option>' +
				'<option value="BO">Bolivia </option>' +
				'<option value="BA">Bosnia and Herzegovina </option>' +
				'<option value="BW">Botswana </option>' +
				'<option value="BV">Bouvet Island </option>' +
				'<option value="BR">Brazil </option>' +
				'<option value="IO">British Indian Ocean Territory </option>' +
				'<option value="BN">Brunei Darussalam </option>' +
				'<option value="BG">Bulgaria </option>' +
				'<option value="BF">Burkina Faso </option>' +
				'<option value="BI">Burundi </option>' +
				'<option value="KH">Cambodia </option>' +
				'<option value="CM">Cameroon </option>' +
				'<option value="CA">Canada </option>' +
				'<option value="CV">Cape Verde </option>' +
				'<option value="KY">Cayman Islands </option>' +
				'<option value="CF">Central African Republic </option>' +
				'<option value="TD">Chad </option>' +
				'<option value="CL">Chile </option>' +
				'<option value="CN">China </option>' +
				'<option value="CX">Christmas Island (Australia) </option>' +
				'<option value="CC">Cocos (Keeling) Islands </option>' +
				'<option value="CO">Colombia </option>' +
				'<option value="KM">Comoros </option>' +
				'<option value="CG">Congo </option>' +
				'<option value="CK">Cook Islands (New Zealand) </option>' +
				'<option value="CR">Costa Rica </option>' +
				'<option value="CI">Cote d`Ivoire </option>' +
				'<option value="HR">Croatia (Hrvatska) </option>' +
				'<option value="CY">Cyprus </option>' +
				'<option value="CZ">Czech Republic </option>' +
				'<option value="CD">Democratic Republic of the Congo </option>' +
				'<option value="DK">Denmark </option>' +
				'<option value="DJ">Djibouti </option>' +
				'<option value="DM">Dominica </option>' +
				'<option value="DO">Dominican Republic </option>' +
				'<option value="TP">East Timor </option>' +
				'<option value="EC">Ecuador </option>' +
				'<option value="EG">Egypt </option>' +
				'<option value="SV">El Salvador </option>' +
				'<option value="GQ">Equatorial Guinea </option>' +
				'<option value="ER">Eritrea </option>' +
				'<option value="EE">Estonia </option>' +
				'<option value="ET">Ethiopia </option>' +
				'<option value="FK">Falkland Islands </option>' +
				'<option value="FO">Faroe Islands </option>' +
				'<option value="FJ">Fiji </option>' +
				'<option value="FI">Finland </option>' +
				'<option value="FR">France </option>' +
				'<option value="GF">French Guiana </option>' +
				'<option value="PF">French Polynesia </option>' +
				'<option value="GA">Gabon </option>' +
				'<option value="GM">Gambia </option>' +
				'<option value="GE">Georgia </option>' +
				'<option value="DE">Germany </option>' +
				'<option value="GH">Ghana </option>' +
				'<option value="GI">Gibraltar </option>' +
				'<option value="GR">Greece </option>' +
				'<option value="GL">Greenland </option>' +
				'<option value="GD">Grenada </option>' +
				'<option value="GP">Guadeloupe </option>' +
				'<option value="GU">Guam </option>' +
				'<option value="GT">Guatemala </option>' +
				'<option value="GN">Guinea </option>' +
				'<option value="GW">Guinea-Bissau </option>' +
				'<option value="GY">Guyana </option>' +
				'<option value="HT">Haiti </option>' +
				'<option value="HN">Honduras </option>' +
				'<option value="HK">Hong Kong </option>' +
				'<option value="HU">Hungary </option>' +
				'<option value="IS">Iceland </option>' +
				'<option value="IN">India </option>' +
				'<option value="ID">Indonesia </option>' +
				'<option value="IQ">Iraq </option>' +
				'<option value="IE">Ireland </option>' +
				'<option value="IL">Israel </option>' +
				'<option value="IT">Italy </option>' +
				'<option value="JM">Jamaica </option>' +
				'<option value="JP">Japan </option>' +
				'<option value="JO">Jordan </option>' +
				'<option value="KZ">Kazakhstan </option>' +
				'<option value="KE">Kenya </option>' +
				'<option value="KI">Kiribati </option>' +
				'<option value="KR">Korea, South </option>' +
				'<option value="KW">Kuwait </option>' +
				'<option value="KG">Kyrgyzstan </option>' +
				'<option value="LA">Laos </option>' +
				'<option value="LV">Latvia </option>' +
				'<option value="LB">Lebanon </option>' +
				'<option value="LS">Lesotho </option>' +
				'<option value="LR">Liberia </option>' +
				'<option value="LY">Libya </option>' +
				'<option value="LI">Liechtenstein </option>' +
				'<option value="LT">Lithuania </option>' +
				'<option value="LU">Luxembourg </option>' +
				'<option value="MO">Macau </option>' +
				'<option value="MK">Macedonia </option>' +
				'<option value="MG">Madagascar </option>' +
				'<option value="MW">Malawi </option>' +
				'<option value="MY">Malaysia </option>' +
				'<option value="MV">Maldives </option>' +
				'<option value="ML">Mali </option>' +
				'<option value="MT">Malta </option>' +
				'<option value="MH">Marshall Islands </option>' +
				'<option value="MQ">Martinique </option>' +
				'<option value="MR">Mauritania </option>' +
				'<option value="MU">Mauritius </option>' +
				'<option value="YT">Mayotte </option>' +
				'<option value="MX">Mexico </option>' +
				'<option value="FM">Micronesia </option>' +
				'<option value="MD">Moldova </option>' +
				'<option value="MC">Monaco </option>' +
				'<option value="MN">Mongolia </option>' +
				'<option value="MS">Montserrat </option>' +
				'<option value="MA">Morocco </option>' +
				'<option value="MZ">Mozambique </option>' +
				'<option value="MM">Myanmar </option>' +
				'<option value="NA">Namibia </option>' +
				'<option value="NR">Nauru </option>' +
				'<option value="NP">Nepal </option>' +
				'<option value="NL">Netherlands </option>' +
				'<option value="AN">Netherlands Antilles </option>' +
				'<option value="NC">New Caledonia </option>' +
				'<option value="NZ">New Zealand </option>' +
				'<option value="NI">Nicaragua </option>' +
				'<option value="NE">Niger </option>' +
				'<option value="NG">Nigeria </option>' +
				'<option value="NU">Niue </option>' +
				'<option value="NF">Norfolk Island </option>' +
				'<option value="KP">North Korea </option>' +
				'<option value="MP">Northern Mariana Islands </option>' +
				'<option value="NO">Norway </option>' +
				'<option value="OM">Oman </option>' +
				'<option value="PK">Pakistan </option>' +
				'<option value="PW">Palau </option>' +
				'<option value="PA">Panama </option>' +
				'<option value="PG">Papua New Guinea </option>' +
				'<option value="PY">Paraguay </option>' +
				'<option value="PE">Peru </option>' +
				'<option value="PH">Philippines </option>' +
				'<option value="PN">Pitcairn Islands </option>' +
				'<option value="PL">Poland </option>' +
				'<option value="PT">Portugal </option>' +
				'<option value="PR">Puerto Rico </option>' +
				'<option value="QA">Qatar </option>' +
				'<option value="RE">Reunion </option>' +
				'<option value="RO">Romania </option>' +
				'<option value="RU">Russian Federation </option>' +
				'<option value="RW">Rwanda </option>' +
				'<option value="KN">Saint Kitts and Nevis </option>' +
				'<option value="LC">Saint Lucia </option>' +
				'<option value="VC">Saint Vincent and the Grenadines </option>' +
				'<option value="SM">San Marino </option>' +
				'<option value="ST">Sao Tome and Principe </option>' +
				'<option value="SA">Saudi Arabia </option>' +
				'<option value="SN">Senegal </option>' +
				'<option value="YU">Serbia and Montengro </option>' +
				'<option value="SC">Seychelles </option>' +
				'<option value="SL">Sierra Leone </option>' +
				'<option value="SG">Singapore </option>' +
				'<option value="SK">Slovak Republic </option>' +
				'<option value="SI">Slovenia </option>' +
				'<option value="SB">Solomon Islands </option>' +
				'<option value="SO">Somalia </option>' +
				'<option value="ZA">South Africa </option>' +
				'<option value="ES">Spain </option>' +
				'<option value="LK">Sri Lanka </option>' +
				'<option value="SH">St Helena </option>' +
				'<option value="PM">St Pierre and Miquelon </option>' +
				'<option value="SR">Suriname </option>' +
				'<option value="SJ">Svalbard and Jan Mayen Islands </option>' +
				'<option value="SZ">Swaziland </option>' +
				'<option value="SE">Sweden </option>' +
				'<option value="CH">Switzerland </option>' +
				'<option value="TW">Taiwan </option>' +
				'<option value="TJ">Tajikistan </option>' +
				'<option value="TZ">Tanzania, United Republic of </option>' +
				'<option value="TH">Thailand </option>' +
				'<option value="TG">Togo </option>' +
				'<option value="TK">Tokelau </option>' +
				'<option value="TO">Tonga </option>' +
				'<option value="TT">Trinidad and Tobago </option>' +
				'<option value="TN">Tunisia </option>' +
				'<option value="TR">Turkey </option>' +
				'<option value="TM">Turkmenistan </option>' +
				'<option value="TC">Turks and Caicos Islands </option>' +
				'<option value="TV">Tuvalu </option>' +
				'<option value="UG">Uganda </option>' +
				'<option value="UA">Ukraine </option>' +
				'<option value="AE">United Arab Emirates </option>' +
				'<option value="GB">United Kingdom </option>' +
				'<option value="US">United States </option>' +
				'<option value="UY">Uruguay </option>' +
				'<option value="UM">US Minor Outlying Islands </option>' +
				'<option value="UZ">Uzbekistan </option>' +
				'<option value="VU">Vanuatu </option>' +
				'<option value="VA">Vatican City State </option>' +
				'<option value="VE">Venezuela </option>' +
				'<option value="VN">Vietnam </option>' +
				'<option value="VG">Virgin Islands (British) </option>' +
				'<option value="VI">Virgin Islands (US) </option>' +
				'<option value="WF">Wallis and Futuna Islands </option>' +
				'<option value="EH">Western Sahara </option>' +
				'<option value="WS">Western Samoa </option>' +
				'<option value="YE">Yemen </option>' +
				'<option value="ZM">Zambia </option>' +
				'<option value="ZW">Zimbabwe </option>' +
				'</select>';
				
var CountryHTML= xmsgRedStar + xmsgSelectCountry +'<br>' +
				'<select name="country" size="1">' +
				CountryCollection;

function ChangeCountry() {
	if (document.RTForm.country.selectedIndex == 0) {
		document.RTForm.country.selectedIndex = 1;
	}
	country = document.RTForm.country.options[document.RTForm.country.selectedIndex].value;
	SetCountryContactInfo();
}

var CountryDHTML= xmsgSelectCountry2+'<br>' +
				'<select name="country" size="1" onchange="ChangeCountry()">' +
				CountryCollection;














// SIG // Begin signature block
// SIG // MIIWVgYJKoZIhvcNAQcCoIIWRzCCFkMCAQExDjAMBggq
// SIG // hkiG9w0CBQUAMGYGCisGAQQBgjcCAQSgWDBWMDIGCisG
// SIG // AQQBgjcCAR4wJAIBAQQQEODJBs441BGiowAQS9NQkAIB
// SIG // AAIBAAIBAAIBAAIBADAgMAwGCCqGSIb3DQIFBQAEEOsp
// SIG // OiIiNlepdeZq48bFacugghGXMIIDxDCCAy2gAwIBAgIQ
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
// SIG // 9w0BCQQxEgQQhE8ulPFXNIdOAw3Yqbj+8DBUBgorBgEE
// SIG // AYI3AgEMMUYwRKAmgCQAQQB1AHQAbwBkAGUAcwBrACAA
// SIG // QwBvAG0AcABvAG4AZQBuAHShGoAYaHR0cDovL3d3dy5h
// SIG // dXRvZGVzay5jb20gMA0GCSqGSIb3DQEBAQUABIGAFYEU
// SIG // nId4UItNnEzYw+6WHLaUDsWRop0TxkNl67u6ms/IhF3a
// SIG // 0LRpiqHSUMQWGRH6bBZ9XTPIj40l+8hPkNNoIkVf6i79
// SIG // 1CcVtIPu6W1BoLLxFA5M+ULlm07hed305HECcfXZqQ1i
// SIG // l4lhETYOHUyOefwpxxQTrzrh06RcrJ06qxuhggH/MIIB
// SIG // +wYJKoZIhvcNAQkGMYIB7DCCAegCAQEwZzBTMQswCQYD
// SIG // VQQGEwJVUzEXMBUGA1UEChMOVmVyaVNpZ24sIEluYy4x
// SIG // KzApBgNVBAMTIlZlcmlTaWduIFRpbWUgU3RhbXBpbmcg
// SIG // U2VydmljZXMgQ0ECEA3pK/DU2CmIGDIFCV6adogwDAYI
// SIG // KoZIhvcNAgUFAKBZMBgGCSqGSIb3DQEJAzELBgkqhkiG
// SIG // 9w0BBwEwHAYJKoZIhvcNAQkFMQ8XDTA1MTAzMDE3NTc1
// SIG // N1owHwYJKoZIhvcNAQkEMRIEEIhaerTWMAMYSQKfppcs
// SIG // uK0wDQYJKoZIhvcNAQEBBQAEggEANLQUNMFgO/AwriRX
// SIG // PRRpqp9UEPEYGbQyoSaOHS+4iyAJr9IH4qL+MQxwQdeW
// SIG // KlFWft0mVrMXJx0F0CPSUaqyaY782kT0pC/4+9ied0N3
// SIG // OW14iVntJOnvX4OL4Bi/e/OqZKliURPPkSOOy0ziakHJ
// SIG // LeaHGdLbgUar7XjOgquv11QBiryI4xRoDZy2oCPlkYzt
// SIG // XfgKHEfqi/5SruWOhvpSvem1RNZYvBzyYd2ZWlP6Vk+n
// SIG // TkXwkNrUde7CHNBQs8s9fz+PYXAhbNnuve4yy3pyMObt
// SIG // nKXfpty2qq2D+dt9O5SJI+F8Y27T0vcgEyxQykcaAU/Q
// SIG // g1qpaLxYbs9F5UxOZA==
// SIG // End signature block
